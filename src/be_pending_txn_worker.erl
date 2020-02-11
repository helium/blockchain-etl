-module(be_pending_txn_worker).

-include("be_db_worker.hrl").
-include("be_block_handler.hrl"). % for BIN utility

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

-define(S_PENDING_TXN_LIST_INIT, "worker_pending_txn_list_init").
-define(S_PENDING_TXN_LIST_PENDING, "worker_pending_txn_list_pending").
-define(S_PENDING_TXN_DELETE, "worker_pending_txn_delete").
-define(S_PENDING_TXN_FAIL, "worker_pending_txn_fail").
-define(S_PENDING_TXN_PENDING, "worker_pending_txn_pending").

-define(SELECT_PENDING_TXN_BASE, "select t.data from pending_transactions t ").

prepare_conn(Conn) ->
    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_LIST_INIT,
                     ?SELECT_PENDING_TXN_BASE "where t.status in ('received', 'pending') order by created_at", []),

    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_LIST_PENDING,
                     ?SELECT_PENDING_TXN_BASE "where t.status = 'pending' order by created_at", []),

    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_DELETE,
                    "DELETE from pending_transactions where hash = $1", []),

    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_FAIL,
                     "UPDATE pending_transactions SET status = 'failed', failed_reason = $2 WHERE created_at = $1", []),

    {ok, _} =
        epgsql:parse(Conn, ?S_PENDING_TXN_PENDING,
                     "UPDATE pending_transactions SET status = 'pending', failed_reason = '' WHERE created_at = $1", []),
    ok.

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


handle_info({submit_pending, Stmt}, State) ->
    {ok, PendingTxns} = get_pending_txns(Stmt),
    Parent = self(),
    lager:info("Submitting ~p pending transactions", [length(PendingTxns)]),
    %% Submit all pending txns with given statuses to the chain
    %% txn_manager and mark them as pending
    lists:foreach(fun({Key, PendingTxn}) ->
                          blockchain_worker:submit_txn(PendingTxn,
                                                       fun(Res) ->
                                                               Parent ! {pending_result, Key, Res}
                                                       end),
                          {ok, _} = ?PREPARED_QUERY(?S_PENDING_TXN_PENDING, [Key])
                  end, PendingTxns),
    PendingTime = application:get_env(blockchain_etl, pending_interval, 10000),
    erlang:send_after(PendingTime, self(), {submit_pending, ?S_PENDING_TXN_LIST_PENDING}),
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
    {ok, lists:map(fun(BinTxn) ->
                           Txn = blockchain_txn_pb:decode_msg(BinTxn, blockchain_txn_pb),
                           {blockchain_txn:hash(Txn), Txn}
                   end, Results)}.
