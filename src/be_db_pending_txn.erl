-module(be_db_pending_txn).

-include("be_db_worker.hrl").
-include("be_follower.hrl"). % for BIN utility

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
-define(S_PENDING_TXN_DELETE, "worker_pending_txn_delete").
-define(S_PENDING_TXN_FAIL, "worker_pending_txn_fail").
-define(S_PENDING_TXN_SET_PENDING, "worker_pending_txn_set_pending").

-define(SELECT_PENDING_TXN_BASE, "select t.hash, t.data from pending_transactions t ").

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(Conn, ?S_PENDING_TXN_LIST_INIT,
                     ?SELECT_PENDING_TXN_BASE "where t.status in ('received', 'pending') order by created_at", []),

    {ok, S2} =
        epgsql:parse(Conn, ?S_PENDING_TXN_LIST_RECEIVED,
                     ?SELECT_PENDING_TXN_BASE "where t.status = 'received' order by created_at", []),

    {ok, S3} =
        epgsql:parse(Conn, ?S_PENDING_TXN_DELETE,
                    "DELETE from pending_transactions where hash = $1", []),

    {ok, S4} =
        epgsql:parse(Conn, ?S_PENDING_TXN_FAIL,
                     ["UPDATE pending_transactions SET ",
                      "status = 'failed', ",
                      "failed_reason = $2 ",
                      "WHERE hash = $1"],
                     []),

    {ok, S5} =
        epgsql:parse(Conn, ?S_PENDING_TXN_SET_PENDING,
                     ["UPDATE pending_transactions SET ",
                      "status = 'pending', ",
                      "failed_reason = '', ",
                      "fields = $2",
                      "WHERE hash = $1"],
                     []),
    {ok, S6} =
        epgsql:parse(Conn, ?S_PENDING_TXN_INSERT_ACTOR,
                     ["insert into pending_transaction_actors ",
                      "(actor, actor_role, transaction_hash) values ",
                      "($1, $2, $3) ",
                     "on conflict do nothing"], []),
    #{
      ?S_PENDING_TXN_LIST_INIT => S1,
      ?S_PENDING_TXN_LIST_RECEIVED => S2,
      ?S_PENDING_TXN_DELETE => S3,
      ?S_PENDING_TXN_FAIL => S4,
      ?S_PENDING_TXN_SET_PENDING => S5,
      ?S_PENDING_TXN_INSERT_ACTOR => S6
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

handle_info({submit_pending, Stmt}, State=#state{}) ->
    {ok, PendingTxns} = get_pending_txns(Stmt),
    Parent = self(),
    lager:info("Submitting ~p pending transactions", [length(PendingTxns)]),

    %% Submit all pending txns with given statuses to the chain
    %% txn_manager and mark them as pending
    lists:foreach(fun({TxnHash, PendingTxn}) ->
                          %% Fetch the actors and check it any of them is in the blacklist
                          TxnActors = lists:map(fun({Role, Actor}) ->
                                                        {Role, ?BIN_TO_B58(Actor)}
                                                end, be_db_txn_actor:to_actors(PendingTxn)),
                          %% If any of the actors is in the blacklist we deny the transaction
                          Queries = case is_blacklisted(TxnActors) of
                                        true ->
                                            lager:info("Blacklisted transaction: ~s", [TxnHash]),
                                            [{?S_PENDING_TXN_FAIL, [TxnHash, <<"blacklist">>]}];
                                        false ->
                                            blockchain_worker:submit_txn(PendingTxn,
                                                                         fun(Res) ->
                                                                                 Parent ! {pending_result, TxnHash, Res}
                                                                         end),
                                            q_insert_actors(TxnHash, TxnActors,
                                                            q_update_pending_txn(TxnHash, PendingTxn, []))
                                    end,

                          ?WITH_TRANSACTION(fun(C) ->
                                                    ?BATCH_QUERY(C, Queries)
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

q_update_pending_txn(TxnHash, Txn, Acc) ->
    %% Translates the pending transaction to it's json form. The
    %% currently supported pending transactions are always submitted
    %% without the need for a ledger so we don't use one here, but
    %% safeguard against any errors converting to json by setting
    %% fields to null
    Fields = try
                 be_txn:to_json(Txn, undefined)
             catch
                 _:_ -> null
             end,
    [{?S_PENDING_TXN_SET_PENDING, [TxnHash, Fields]} | Acc].

q_insert_actors(TxnHash, TxnActors, Acc) ->
    %% Takes the list of b58 encoded transaction rols, actor pairs and
    %% sets up insertion queries. NOTE: The conversion to B58 is done
    %% by the caller!
    lists:foldl(fun({Role, Actor}, TAcc) ->
                        [{?S_PENDING_TXN_INSERT_ACTOR,
                          [Actor, Role, TxnHash]} | TAcc]
                end, Acc, TxnActors).

%% Returns if any of the given b58 encoded list of transactin actors
%% is in the configured blacklist for the application.
is_blacklisted(TxnActors) ->
    {ok, BlackListStrings} = application:get_env(blockchain_etl, pending_blacklist, []),
    BlackList = lists:map(fun(Str) -> list_to_binary(Str) end, BlackListStrings),
    lists:any(fun(BlackListEntry) ->
                      lists:keyfind(BlackListEntry, 2, TxnActors) /= false
              end, BlackList).
