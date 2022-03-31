-module(be_db_worker).

-include("be_db_worker.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-behaviour(gen_server).

-callback prepare_conn(epgsql:connection()) -> #{string() => epgsql:statement()}.

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([squery/1,
         equery/2,
         prepared_query/2, prepared_query/3,
         batch_query/1, batch_query/2,
         copy_list/2,
         with_transaction/1, with_connection/1]).

-record(state,
        {
         db_conn :: epgsql:connection(),
         prepared_statements :: #{string() => epgsql:statement()}
        }).


-spec squery(Stmt::epgsql:sql_query()) -> epgsql_cmd_squery:response().
squery(Sql) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {squery, Sql}, infinity)
                        end).

-spec equery(Stmt::epgsql:sql_query(), Params::[epgsql:bind_param()]) -> epgsql_cmd_equery:response().
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

prepared_query({Stmts, Conn}, Name, Params) ->
    Statement = maps:get(Name, Stmts),
    #statement{types = Types} = Statement,
    TypedParameters = lists:zip(Types, Params),
    epgsql_sock:sync_command(Conn, epgsql_cmd_prepared_query, {Statement, TypedParameters}).

-spec batch_query([{Name::string(), Params::[epgsql:bind_param()]}]) -> ok | {error, list()}.
batch_query(Batch) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {batch_query, Batch}, infinity)
                        end).

-spec batch_query(Resource::term(), [{Name::string(), Params::[epgsql:bind_param()]}]) -> ok.
batch_query({Stmts, Conn}, Batch) ->
    Query = lists:map(fun({Name, Params}) ->
                              {maps:get(Name, Stmts), Params}
                      end, Batch),
    Results = epgsql:execute_batch(Conn, Query),
    case lists:filter(fun({ok, _}) -> false;
                         ({error, _}) -> true
                      end, Results) of
        [] -> ok;
        Errors ->
            throw({error, Errors})
    end.

%% Config - is a tuple consisting of the following;
%%          TableString in the following format "table_copied_to (col1, col2, ... colN)"
%%          Format is a list of epgsql_type()'s [text, int4, ... type] https://github.com/epgsql/epgsql/tree/devel/src/datatypes
%% The Tablestring columns (col1, col2, ... colN) need to match the format list created out of epgsql_type()'s
%% List   - is a list of lists that represent the data as rows in the table being copied to;
%%          [ [Row1], [Row2], ... [RowN] ], Row1 = [col1_value, col2_value, ... colN_value]
-spec copy_list(Config::{TableString::string, Format::list()}, List::list()) -> ok | {error, list()}.
copy_list(Config, List) ->
    poolboy:transaction(?DB_POOL,
                        fun(Worker) ->
                                gen_server:call(Worker, {copy_list, Config, List}, infinity)
                        end).

-spec copy_list({TableString::string, Format::list()}, List::list(), Conn::epgsql:connection()) -> ok.
copy_list({TableString, Format}, List, Conn) ->
    case epgsql:copy_from_stdin(
        Conn,
        "COPY " ++ TableString ++ " FROM STDIN WITH (FORMAT binary)",
        {binary, Format}
    ) of
        {ok, _} ->
            case epgsql:copy_send_rows(
                Conn,
                List,
                infinity
            ) of
                ok ->
                    epgsql:copy_done(Conn);
                {error, Error} ->
                    lager:error("~p", [{error, Error}]),
                    throw({error, Error})
            end;
        {error, Error} ->
            lager:error("~p", [{error, Error}]),
            throw({error, Error})
    end.

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
    PreparedStatements = lists:foldl(fun(Mod, Acc) ->
                                             maps:merge(Mod:prepare_conn(Conn), Acc)
                                     end, #{}, GetOpt(db_handlers)),
    {ok, #state{db_conn=Conn, prepared_statements=PreparedStatements}}.

handle_call({squery, Sql}, _From, #state{db_conn=Conn}=State) ->
    {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{db_conn=Conn}=State) ->
    {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call({prepared_query, Name, Params}, _From, #state{db_conn=Conn, prepared_statements=Stmts}=State) ->
    {reply, prepared_query({Stmts, Conn}, Name, Params), State};
handle_call({batch_query, Batch}, _From, #state{db_conn=Conn, prepared_statements=Stmts}=State) ->
    {reply, batch_query({Stmts, Conn}, Batch), State};
handle_call({copy_list, Config, List}, _From, #state{db_conn=Conn}=State) ->
    {reply, copy_list(Config, List, Conn), State};
handle_call({with_transaction, Fun}, _From, #state{db_conn=Conn, prepared_statements=Stmts}=State) ->
    TransactionFun = fun(_) ->
                             Fun({Stmts, Conn})
                     end,
    {reply, epgsql:with_transaction(Conn, TransactionFun, [{reraise, true}]), State};
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
