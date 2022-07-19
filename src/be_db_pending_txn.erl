-module(be_db_pending_txn).

-include("be_db_worker.hrl").
% for BIN utility
-include("be_db_follower.hrl").

-behavior(gen_server).
-behavior(be_db_worker).

%% be_db_worker
-export([prepare_conn/1]).
%% gen_server
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {}).

%%
%% be_db_worker
%%

-define(S_PENDING_TXN_INSERT_ACTOR, "worker_pending_txn_insert_actor").
-define(S_PENDING_TXN_LIST_INIT, "worker_pending_txn_list_init").
-define(S_PENDING_TXN_LIST_RECEIVED, "worker_pending_txn_list_received").
-define(S_PENDING_TXN_SET_FAILED, "worker_pending_txn_set_failed").
-define(S_PENDING_TXN_SET_PENDING, "worker_pending_txn_set_pending").
-define(S_PENDING_TXN_SET_CLEARED, "worker_pending_txn_set_cleared").

-define(SELECT_PENDING_TXN_BASE,
    "select t.created_at, t.hash, t.data from pending_transactions t "
).

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_PENDING_TXN_LIST_INIT,
            ?SELECT_PENDING_TXN_BASE
            "where t.status in ('received', 'pending') order by created_at",
            []
        ),

    {ok, S2} =
        epgsql:parse(
            Conn,
            ?S_PENDING_TXN_LIST_RECEIVED,
            ?SELECT_PENDING_TXN_BASE "where t.status = 'received' order by created_at",
            []
        ),

    {ok, S4} =
        epgsql:parse(
            Conn,
            ?S_PENDING_TXN_SET_FAILED,
            [
                "UPDATE pending_transactions SET ",
                "status = 'failed', ",
                "failed_reason = $2 ",
                "WHERE created_at = $1 ",
                "AND status != 'cleared' "
            ],
            []
        ),

    {ok, S5} =
        epgsql:parse(
            Conn,
            ?S_PENDING_TXN_SET_PENDING,
            [
                "UPDATE pending_transactions SET ",
                "status = 'pending', ",
                "failed_reason = '', ",
                "fields = $2 ",
                "WHERE created_at = $1"
            ],
            []
        ),

    {ok, S6} =
        epgsql:parse(
            Conn,
            ?S_PENDING_TXN_SET_CLEARED,
            [
                "UPDATE pending_transactions SET ",
                "status = 'cleared', ",
                "failed_reason = '' ",
                "WHERE created_at = $1"
            ],
            []
        ),

    {ok, S7} =
        epgsql:parse(
            Conn,
            ?S_PENDING_TXN_INSERT_ACTOR,
            [
                "insert into pending_transaction_actors ",
                "(actor, actor_role, transaction_hash, created_at) values ",
                "($1, $2, $3, $4) ",
                "on conflict do nothing"
            ],
            []
        ),
    #{
        ?S_PENDING_TXN_LIST_INIT => S1,
        ?S_PENDING_TXN_LIST_RECEIVED => S2,
        ?S_PENDING_TXN_SET_FAILED => S4,
        ?S_PENDING_TXN_SET_PENDING => S5,
        ?S_PENDING_TXN_SET_CLEARED => S6,
        ?S_PENDING_TXN_INSERT_ACTOR => S7
    }.

%%
%% gen_server
%%

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    self() ! {submit_pending, ?S_PENDING_TXN_LIST_INIT},
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    lager:notice("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:notice("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info({submit_pending, Stmt}, State = #state{}) ->
    {ok, PendingTxns} = get_pending_txns(Stmt),
    Parent = self(),
    lager:info("Submitting ~p pending transactions", [length(PendingTxns)]),
    %% Translates the pending transaction to it's json form. The
    %% currently supported pending transactions are always submitted
    %% without the need for a ledger so we don't use one here, but
    %% safeguard against any errors converting to json by setting
    %% fields to null
    QUpdatePendingTxn = fun(TxnCreatedAt, _TxnHash, Txn, Acc) ->
        Fields =
            try
                be_txn:to_json(Txn)
            catch
                _:_ -> null
            end,
        [{?S_PENDING_TXN_SET_PENDING, [TxnCreatedAt, Fields]} | Acc]
    end,
    %% Insert the actors for a given transaction
    QInsertActors = fun(TxnCreatedAt, TxnHash, Txn, Acc) ->
        lists:foldl(
            fun({Role, Actor}, TAcc) ->
                [
                    {?S_PENDING_TXN_INSERT_ACTOR, [?BIN_TO_B58(Actor), Role, TxnHash, TxnCreatedAt]}
                    | TAcc
                ]
            end,
            Acc,
            be_db_txn_actor:to_actors(Txn)
        )
    end,

    %% Submit all pending txns with given statuses to the chain
    %% txn_manager and mark them as pending
    lists:foreach(
        fun({TxnCreatedAt, TxnHash, PendingTxn}) ->
            blockchain_txn_mgr:submit(
                PendingTxn,
                term_to_binary(TxnCreatedAt),
                fun(Res) ->
                    Parent ! {pending_result, TxnCreatedAt, Res}
                end
            ),
            Queries = lists:foldl(
                fun(Fun, Acc) ->
                    Fun(TxnCreatedAt, TxnHash, PendingTxn, Acc)
                end,
                [],
                [
                    QUpdatePendingTxn,
                    QInsertActors
                ]
            ),
            ?WITH_TRANSACTION(fun(C) ->
                ?BATCH_QUERY(C, Queries)
            end)
        end,
        PendingTxns
    ),
    PendingTime = application:get_env(blockchain_etl, pending_interval, 10000),
    erlang:send_after(PendingTime, self(), {submit_pending, ?S_PENDING_TXN_LIST_RECEIVED}),
    {noreply, State};
handle_info({pending_result, Key, ok}, State) ->
    {ok, _} = ?PREPARED_QUERY(?S_PENDING_TXN_SET_CLEARED, [Key]),
    {noreply, State};
handle_info({pending_result, Key, {error, duplicate_key}}, State) ->
    lager:error("Duplicate submission reported for: ~p", [Key]),
    {noreply, State};
handle_info({pending_result, Key, {error, Error}}, State) ->
    ErrorStr = lists:flatten(io_lib:format("~p", [Error])),
    {ok, _} = ?PREPARED_QUERY(?S_PENDING_TXN_SET_FAILED, [Key, ErrorStr]),
    {noreply, State};
handle_info(Info, State) ->
    lager:notice("Unhandled info ~p", [Info]),
    {noreply, State}.

-spec get_pending_txns(string()) -> {ok, [{epgsql:pg_datetime(), blockchain_core_txn:txn()}]}.
get_pending_txns(Stmt) ->
    {ok, _, Results} = ?PREPARED_QUERY(Stmt, []),
    {ok,
        lists:foldl(
            fun({CreatedAt, Hash, BinTxn}, Acc) ->
                case catch blockchain_txn_pb:decode_msg(BinTxn, blockchain_txn_pb) of
                    {'EXIT', _} ->
                        ?PREPARED_QUERY(?S_PENDING_TXN_SET_FAILED, [CreatedAt, "decoding_failure"]),
                        Acc;
                    Txn ->
                        [{CreatedAt, Hash, blockchain_txn:unwrap_txn(Txn)} | Acc]
                end
            end,
            [],
            lists:reverse(Results)
        )}.
