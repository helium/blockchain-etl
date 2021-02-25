-module(be_db_validator).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-record(state, {
    base_secs = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) ::
        pos_integer()
}).

-define(S_VALIDATOR_INSERT, "validator_insert").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_VALIDATOR_INSERT,
            [
                "insert into validators (block, address, owner, stake, status, nonce, last_heartbeat, version_heartbeat) select ",
                "$1 as block, ",
                "$2 as address, ",
                "$3 as owner, ",
                "$4 as stake, ",
                "$5 as status, ",
                "$6 as nonce, "
                "$7 as last_heartbeat, "
                "$8 as version_heartbeat "
            ],
            []
        ),

    #{
        ?S_VALIDATOR_INSERT => S1
    }.

%%
%% be_block_handler
%%

init(_) ->
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State = #state{}) ->
    Txns = blockchain_block_v1:transactions(Block),
    %% Fetch actor keys that relate to validators from each transaction's actors
    ValidatorsFromActors = fun (Actors) ->
        lists:filtermap(
            fun
                ({"validator", Key}) -> {true, Key};
                (_) -> false
            end,
            Actors
        )
    end,
    Addresses = lists:usort(
        lists:foldl(
            fun (Txn, Acc) ->
                Addresses = ValidatorsFromActors(be_db_txn_actor:to_actors(Txn)),
                Addresses ++ Acc
            end,
            [],
            Txns
        )
    ),
    BlockHeight = blockchain_block_v1:height(Block),
    Queries = lists:foldl(
        fun (Address, Acc) ->
            case blockchain_ledger_v1:get_validator(Address, Ledger) of
                {error, _} ->
                    Acc;
                {ok, Entry} ->
                    Params = [
                        BlockHeight,
                        ?BIN_TO_B58(Address),
                        ?BIN_TO_B58(blockchain_ledger_validator_v1:owner_address(Entry)),
                        blockchain_ledger_validator_v1:stake(Entry),
                        blockchain_ledger_validator_v1:nonce(Entry),
                        blockchain_ledger_validator_v1:status(Entry),
                        blockchain_ledger_validator_v1:last_heartbeat(Entry),
                        blockchain_ledger_validator_v1:version(Entry)
                    ],
                    [{?S_VALIDATOR_INSERT, Params} || Acc]
            end
        end,
        [],
        Addresses
    ),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State}.
