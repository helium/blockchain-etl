-module(be_db_validator).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_db_follower
-export([init/1, load_block/6]).
%% hooks
-export([incremental_commit_hook/1, end_commit_hook/1]).

-record(state, {}).

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
    ets:new(?MODULE, [public, named_table]),
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State = #state{}) ->
    BlockHeight = blockchain_block_v1:height(Block),
    Queries = ets:foldl(
        fun
            ({Key}, Acc) ->
                case blockchain_ledger_v1:get_validator(Key, Ledger) of
                    {error, _} ->
                        Acc;
                    {ok, Entry} ->
                        Params = [
                            BlockHeight,
                            ?BIN_TO_B58(Key),
                            ?BIN_TO_B58(
                                blockchain_ledger_validator_v1:owner_address(Entry)
                            ),
                            blockchain_ledger_validator_v1:stake(Entry),
                            blockchain_ledger_validator_v1:status(Entry),
                            blockchain_ledger_validator_v1:nonce(Entry),
                            blockchain_ledger_validator_v1:last_heartbeat(Entry),
                            blockchain_ledger_validator_v1:version(Entry)
                        ],
                        [{?S_VALIDATOR_INSERT, Params} | Acc]
                end;
            (_, Acc) ->
                Acc
        end,
        [],
        ?MODULE
    ),
    ok = ?BATCH_QUERY(Conn, Queries),
    ets:delete_all_objects(?MODULE),
    {ok, State}.

incremental_commit_hook(Changes) ->
    lists:foreach(
        fun
            ({_CF, put, Key, _Value}) ->
                ets:insert(?MODULE, {Key});
            (_) ->
                ok
        end,
        Changes
    ).

end_commit_hook(_) -> ok.
