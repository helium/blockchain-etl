-module(be_pending_txn_worker).

-include("be_db_worker.hrl").
-include("be_block_handler.hrl"). % for BIN utility

-behavior(gen_server).
-behavior(be_db_worker).

%% be_db_worker
-export([prepare_conn/1]).
%% gen_server
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
                s_set_pending :: epgsql:statement(),
                s_insert_actor :: epgsql:statement()
               }).

%%
%% be_db_worker
%%

-define(S_PENDING_TXN_INSERT_ACTOR, "worker_pending_txn_insert_actor").
-define(S_PENDING_TXN_LIST_INIT, "worker_pending_txn_list_init").
-define(S_PENDING_TXN_LIST_RECEIVED, "worker_pending_txn_list_received").
-define(S_PENDING_TXN_DELETE, "worker_pending_txn_delete").
-define(S_PENDING_TXN_FAIL, "worker_pending_txn_fail").
-define(S_PENDING_TXN_SET_PENDING, "worker_pending_txn_set_pending").

-define(SELECT_PENDING_TXN_BASE, "select t.hash, t.data from pending_transactions t ").

prepare_conn(Conn) ->
    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_LIST_INIT,
                     ?SELECT_PENDING_TXN_BASE "where t.status in ('received', 'pending') order by created_at", []),

    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_LIST_RECEIVED,
                     ?SELECT_PENDING_TXN_BASE "where t.status = 'received' order by created_at", []),

    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_DELETE,
                    "DELETE from pending_transactions where hash = $1", []),

    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_FAIL,
                     ["UPDATE pending_transactions SET ",
                      "status = 'failed', ",
                      "failed_reason = $2 ",
                      "WHERE hash = $1"],
                     []),

    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_SET_PENDING,
                     ["UPDATE pending_transactions SET ",
                      "status = 'pending', ",
                      "failed_reason = '', ",
                      "fields = $2",
                      "WHERE hash = $1"],
                     []),
    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_INSERT_ACTOR,
                     ["insert into pending_transaction_actors ",
                      "(actor, actor_role, transaction_hash) values ",
                      "($1, $2, $3) ",
                     "on conflict do nothing"], []),
    ok.

%%
%% gen_server
%%

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {SetPending, InsertActor} = ?WITH_CONNECTION(
                       fun(Conn) ->
                               {ok, Set} = epgsql:describe(Conn, statement, ?S_PENDING_TXN_SET_PENDING),
                               {ok, Insert} = epgsql:describe(Conn, statement, ?S_PENDING_TXN_INSERT_ACTOR),
                               {Set, Insert}
                       end),
    self() ! {submit_pending, ?S_PENDING_TXN_LIST_INIT},
    {ok, #state{
            s_set_pending = SetPending,
            s_insert_actor = InsertActor
           }}.

handle_call(Request, _From, State) ->
    lager:notice("Unhandled call ~p", [Request]),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    lager:notice("Unhandled cast ~p", [Msg]),
    {noreply, State}.


handle_info({submit_pending, Stmt}, State=#state{s_insert_actor=StmtInsertActor,
                                                 s_set_pending=StmtSetPending}) ->
    {ok, PendingTxns} = get_pending_txns(Stmt),
    Parent = self(),
    lager:info("Submitting ~p pending transactions", [length(PendingTxns)]),
    %% Translates the pending transaction to it's json form. The
    %% currently supported pending transactions are always submitted
    %% without the need for a ledger so we don't use one here, but
    %% safeguard against any errors converting to json by setting
    %% fields to null
    QUpdatePendingTxn = fun(TxnHash, Txn, Acc) ->
                                Fields = try
                                             be_txn:to_json(Txn, undefined)
                                         catch
                                             _:_ -> null
                                         end,
                                [{StmtSetPending, [TxnHash, Fields]} | Acc]
                        end,
    %% Insert the actors for a given transaction
    QInsertActors = fun(TxnHash, Txn, Acc) ->
                            lists:foldl(fun({Role, Actor}, TAcc) ->
                                                  [{StmtInsertActor,
                                                    [?BIN_TO_B58(Actor), Role, TxnHash]} | TAcc]
                                        end, Acc, be_txn_actor:to_actors(Txn))
                    end,

    %% Submit all pending txns with given statuses to the chain
    %% txn_manager and mark them as pending
    lists:foreach(fun({TxnHash, PendingTxn}) ->
                          blockchain_worker:submit_txn(PendingTxn,
                                                       fun(Res) ->
                                                               Parent ! {pending_result, TxnHash, Res}
                                                       end),
                          Queries = lists:foldl(fun(Fun, Acc) ->
                                                        Fun(TxnHash, PendingTxn, Acc)
                                                end, [],
                                                [QUpdatePendingTxn,
                                                 QInsertActors]),
                          ?WITH_TRANSACTION(fun(Conn) ->
                                                    be_block_handler:run_queries(Conn, Queries, undefined)
                                            end)
                  end, PendingTxns),
    PendingTime = application:get_env(blockchain_etl, pending_interval, 10000),
    erlang:send_after(PendingTime, self(), {submit_pending, ?S_PENDING_TXN_LIST_RECEIVED}),
    {noreply, State};
handle_info({pending_result, Key, ok}, State) ->
    {ok, _} = ?PREPARED_QUERY(?S_PENDING_TXN_DELETE, [Key]),
    {noreply, State};
handle_info({pending_result, Key, {error, Error}}, State) ->
    ErrorStr = lists:flatten(io_lib:format("~p", [Error])),
    {ok, _} = ?PREPARED_QUERY(?S_PENDING_TXN_FAIL, [Key, ErrorStr]),
    {noreply, State};

handle_info(Info, State) ->
    lager:notice("Unhandled info ~p", [Info]),
    {noreply, State}.



-spec get_pending_txns(string()) -> {ok, [{blockchain_txn:hash(), blockchain_core_txn:txn()}]}.
get_pending_txns(Stmt) ->
    {ok, _, Results} = ?PREPARED_QUERY(Stmt, []),
    {ok, lists:foldl(fun({Hash, BinTxn}, Acc) ->
                           case catch blockchain_txn_pb:decode_msg(BinTxn, blockchain_txn_pb) of
                               {'EXIT', _} ->
                                   ?PREPARED_QUERY(?S_PENDING_TXN_FAIL, [Hash, "decoding_failure"]),
                                   Acc;
                               Txn ->
                                   [{Hash, blockchain_txn:unwrap_txn(Txn)} | Acc]
                           end
                     end, [], lists:reverse(Results))}.
