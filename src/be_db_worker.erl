-module(be_db_worker).

-include("be_db_worker.hrl").

-behaviour(gen_server).

-callback prepare_conn(epgsql:connection()) -> ok.

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([squery/1, equery/2, prepared_query/2,
         with_transaction/1, with_connection/1]).

-record(state,
        {
         db_conn :: epgsql:connection()
        }).


squery(Sql) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {squery, Sql}, infinity)
                        end).

-spec equery(Stmt::string(), Params::[epgsql:bind_param()]) -> epgsql_cmd_equery:response().
equery(Stmt, Params) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {equery, Stmt, Params}, infinity)
                        end).

-spec prepared_query(Name::string(), Params::[epgsql:bind_param()]) -> epgsql_cmd_prepared_query:response().
prepared_query(Name, Params) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {prepared_query, Name, Params}, infinity)
                        end).

-spec with_transaction(fun((epgsql:connection()) -> Reply)) -> Reply | {rollback, any()} when
      Reply::any().
with_transaction(Fun) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {with_transaction, Fun}, infinity)
                        end).

-spec with_connection(fun((epgsql:connection()) -> Reply)) -> Reply when Reply::any().
with_connection(Fun) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {with_connection, Fun}, infinity)
                        end).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    GetOpt = fun(K) ->
                     case lists:keyfind(K, 1, Args) of
                         false -> error({missing_opt, K});
                         {_, V} -> V
                     end
             end,
    DBOpts = GetOpt(db_opts),
    Codecs = [{epgsql_codec_json, {jsone,
                                   [{object_key_type, scalar}, undefined_as_null],
                                   [undefined_as_null]}}],
    {ok, Conn} = epgsql:connect(DBOpts#{codecs => Codecs}),
    lists:foreach(fun(Mod) ->
                          Mod:prepare_conn(Conn)
                  end, GetOpt(db_handlers)),
    {ok, #state{db_conn=Conn}}.

handle_call({squery, Sql}, _From, #state{db_conn=Conn}=State) ->
    {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{db_conn=Conn}=State) ->
    {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call({prepared_query, Name, Params}, _From, #state{db_conn=Conn}=State) ->
    {reply, epgsql:prepared_query(Conn, Name, Params), State};
handle_call({with_transaction, Fun}, _From, #state{db_conn=Conn}=State) ->
    {reply, epgsql:with_transaction(Conn, Fun, [{reraise, false}]), State};
handle_call({with_connection, Fun}, _From, #state{db_conn=Conn}=State) ->
    {reply, Fun(Conn), State};
handle_call(Request, _From, State) ->
    lager:notice("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{db_conn=Conn}) ->
    catch epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
