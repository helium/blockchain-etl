-module(be_db_oui).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_db_follower
-export([init/1, load_block/6]).
%% API
-export([subnet_to_string/1]).
%% hooks
-export([incremental_commit_hook/1, end_commit_hook/2]).

-record(state, {}).

-define(S_OUI_INSERT, "oui_insert").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_OUI_INSERT,
            [
                "insert into ouis (block, oui, owner, nonce, addresses, subnet) select ",
                "$1 as block, ",
                "$2 as oui, ",
                "$3 as owner, ",
                "$4 as nonce, ",
                "$5 as addresses, ",
                "$6 as subnet "
            ],
            []
        ),

    #{
        ?S_OUI_INSERT => S1
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
                case blockchain_ledger_v1:find_routing(Key, Ledger) of
                    {error, _} ->
                        Acc;
                    {ok, Entry} ->
                        Params = [
                            BlockHeight,
                            Key,
                            ?BIN_TO_B58(blockchain_ledger_routing_v1:owner(Entry)),
                            blockchain_ledger_routing_v1:nonce(Entry),
                            [
                                ?BIN_TO_B58(A)
                                || A <- blockchain_ledger_routing_v1:addresses(Entry)
                            ],
                            [
                                subnet_to_string(S)
                                || S <- blockchain_ledger_routing_v1:subnets(Entry)
                            ]
                        ],
                        [{?S_OUI_INSERT, Params} | Acc]
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

-spec subnet_to_string(<<_:48>>) -> string().
subnet_to_string(<<ABase:25/integer-unsigned-big, AMask:23/integer-unsigned-big>>) ->
    io_lib:format("~b/~b", [ABase, AMask]).

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

end_commit_hook(_, _) -> ok.
