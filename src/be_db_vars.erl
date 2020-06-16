-module(be_db_vars).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").


-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-record(state,
       {
        check_vars = true :: boolean()
       }).


-define(S_VARS_INSERT, "vars_insert").
-define(S_VARS_DELETE, "vars_delete").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(Conn, ?S_VARS_INSERT,
                     ["insert into vars_inventory (name, type, value) values ($1, $2, $3) ",
                      "on conflict (name) do update set ",
                      " type = EXCLUDED.type,",
                      " value = EXCLUDED.value"
                     ], []),

    {ok, S2} =
        epgsql:parse(Conn, ?S_VARS_DELETE,
                     ["delete from vars_inventory where name = $1"
                     ], []),

    #{
      ?S_VARS_INSERT => S1,
      ?S_VARS_DELETE => S2
     }.


%%
%% be_block_handler
%%

init(_) ->
    {ok, #state{}}.

load_block(Conn, Hash, Block, Sync, Ledger, State=#state{check_vars=true}) ->
    Vars = blockchain_ledger_v1:snapshot_vars(Ledger),
    Queries = lists:foldl(fun q_insert_var/2, [], Vars),
    ok = ?BATCH_QUERY(Conn, Queries),
    load_block(Conn, Hash, Block, Sync, Ledger, State#state{check_vars=false});
load_block(Conn, _Hash, Block, _Sync, Ledger, State=#state{}) ->
    Txns = lists:filter(fun(Txn) ->
                                blockchain_txn:type(Txn) == blockchain_txn_vars_v1
                            end,
                            blockchain_block:transactions(Block)),
    UnsetQueries = lists:foldl(fun(Txn, Acc) ->
                                       Unsets = blockchain_txn_vars_v1:unsets(Txn),
                                       lists:foldl(fun q_delete_var/2, Acc, Unsets)
                               end, [], Txns),
    case UnsetQueries of
        [] -> [];
        _ ->
            Vars = blockchain_ledger_v1:snapshot_vars(Ledger),
            Queries = lists:foldl(fun q_insert_var/2, UnsetQueries, Vars),
            ok = ?BATCH_QUERY(Conn, Queries)
    end,
    {ok, State}.


q_delete_var(Key, Acc) ->
    [{?S_VARS_DELETE, [encode_key(Key)]} | Acc].


encode_key(K) when is_atom(K) ->
    atom_to_binary(K, utf8);
encode_key(K) when is_binary(K) ->
    K.

q_insert_var({K, V}, Acc) when is_integer(V) ->
    [{?S_VARS_INSERT, [encode_key(K), <<"integer">>, integer_to_binary(V)]} | Acc];
q_insert_var({K, V}, Acc) when is_float(V) ->
    [{?S_VARS_INSERT, [encode_key(K), <<"float">>, float_to_binary(V, [{decimals, 8}, compact])]} | Acc];
q_insert_var({K, V}, Acc) when is_atom(V)->
    [{?S_VARS_INSERT, [encode_key(K), <<"atom">>, atom_to_binary(V, utf8)]} | Acc];
q_insert_var({K, V}, Acc) when is_list(V) orelse is_binary(V) ->
    [{?S_VARS_INSERT, [encode_key(K), <<"binary">>, ?BIN_TO_B64(V)]} | Acc].
