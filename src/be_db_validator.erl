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
-export([incremental_commit_hook/1, end_commit_hook/2]).

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
    %% Collect all validator affected by transactions in the block itself
    BlockValidators = be_db_follower:fold_actors(
        ["validator"],
        fun({_Role, Key}, Acc) ->
            {ok, Entry} = blockchain_ledger_v1:get_validator(Key, Ledger),
            maps:put(Key, Entry, Acc)
        end,
        #{},
        Block
    ),
    %% Merge in any validators that are indirectly updated by the ledger and stashed
    %% in the module ets table
    Validators = ets:foldl(
        fun
            ({Key}, Acc) ->
                case maps:is_key(Key, Acc) of
                    true ->
                        Acc;
                    false ->
                        lager:info("processing unhandled validator ~p", [?BIN_TO_B58(Key)]),
                        {ok, Entry} = blockchain_ledger_v1:get_validator(Key, Ledger),
                        maps:put(Key, Entry, Acc)
                end;
            (_, Acc) ->
                Acc
        end,
        BlockValidators,
        ?MODULE
    ),

    BlockHeight = blockchain_block_v1:height(Block),
    Queries = maps:fold(
        fun(_Key, Validator, Acc) ->
            [q_insert_validator(BlockHeight, Validator) | Acc]
        end,
        [],
        Validators
    ),

    ok = ?BATCH_QUERY(Conn, Queries),
    ets:delete_all_objects(?MODULE),
    {ok, State}.

q_insert_validator(BlockHeight, Entry) ->
    Params = [
        BlockHeight,
        ?BIN_TO_B58(blockchain_ledger_validator_v1:address(Entry)),
        ?BIN_TO_B58(
            blockchain_ledger_validator_v1:owner_address(Entry)
        ),
        blockchain_ledger_validator_v1:stake(Entry),
        blockchain_ledger_validator_v1:status(Entry),
        blockchain_ledger_validator_v1:nonce(Entry),
        blockchain_ledger_validator_v1:last_heartbeat(Entry),
        blockchain_ledger_validator_v1:version(Entry)
    ],
    {?S_VALIDATOR_INSERT, Params}.

incremental_commit_hook(_Changes) ->
    ok.

end_commit_hook(_CF, Changes) ->
    Keys = lists:filtermap(
        fun
            ({put, Key}) -> {true, {Key}};
            (_) -> false
        end,
        Changes
    ),
    ets:insert(?MODULE, Keys).
