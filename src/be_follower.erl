-module(be_follower).

-behaviour(gen_server).

%% API
-export([start_link/0, counts/0, pause/0, resume/0]).
%% Utils
-export([connect_database/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HANDLER_MODULES,
        [
         be_block,
         be_txn_actor,
         be_account
        ]).

-record(state,
        {
         chain :: blockchain:chain(),
         db_conn :: epgsql:connection() | undefined,
         handler_state :: [{Module::atom(), State::any()}]
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

pause() ->
    gen_server:call(?SERVER, pause).

resume() ->
    gen_server:call(?SERVER, resume).

counts() ->
    {ok, Conn} = psql_migration:open_connection([]),
    Stats = #{
              block_count => row_count("blocks", Conn),
              transaction_count => row_count("transactions", Conn),
              actor_count => row_count("transaction_actors", Conn),
              block_sig_count => row_count("block_signatures", Conn),
              account_count => row_count("accounts", Conn)
             },
    epgsql:close(Conn),
    Stats.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {Conn, Handlers} = connect_database(?HANDLER_MODULES),
    self() ! chain_check,
    {ok, #state{handler_state=Handlers, db_conn=Conn}}.

handle_call(pause, _From, State=#state{db_conn=undefined}) ->
    {reply, ok, State};
handle_call(pause, _From, State=#state{}) ->
    blockchain_worker:pause_sync(),
    catch epgsql:close(State#state.db_conn),
    lager:info("paused"),
    {reply, ok, State#state{handler_state=[], db_conn=undefined}};
handle_call(resume, _From, State=#state{db_conn=undefined}) ->
    {Conn, Handlers} = connect_database(?HANDLER_MODULES),
    blockchain_worker:sync(),
    self() ! chain_check,
    lager:info("resumed"),
    {reply, ok, State#state{handler_state=Handlers, db_conn=Conn}};
handle_call(resume, _From, State=#state{}) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({blockchain_event, {add_block, Hash, _Sync, _Ledger}}, State=#state{db_conn=undefined}) ->
    {ok, Block} = blockchain:get_block(Hash, State#state.chain),
    lager:info("Heard about block: ~p", [blockchain_block_v1:height(Block)]),
    {noreply, State};
handle_info({blockchain_event, {add_block, Hash, _Sync, Ledger}}, State=#state{}) ->
    {ok, Block} = blockchain:get_block(Hash, State#state.chain),
    Start = erlang:monotonic_time(),
    LoadFun = fun(_) ->
                      {Counts, States} =
                          lists:foldl(fun({Handler, HandlerState}, {CountMap, HandlerStates}) ->
                                              {ok, Count, NewHandlerState} =
                                                  Handler:load(Hash, Block, Ledger, HandlerState),
                                              {CountMap#{Handler => Count},
                                               [{Handler, NewHandlerState} | HandlerStates]}
                                      end, {#{}, []}, State#state.handler_state),
                      {Counts, lists:reverse(States)}
              end,
    lager:info("Storing block: ~p", [blockchain_block_v1:height(Block)]),
    {Counts, HandlerStates} = epgsql:with_transaction(State#state.db_conn, LoadFun, [{reraise, true}]),
    Latency = erlang:monotonic_time() - Start,
    telemetry:execute([be_follower, add_block],
                      Counts#{latency => Latency},
                      #{block => blockchain_block_v1:height(Block) }),

    {noreply, State#state{handler_state=HandlerStates}};

%% Wait fo chain to come up
handle_info(chain_check, State=#state{chain=undefined}) ->
    case blockchain_worker:blockchain() of
        undefined ->
            erlang:send_after(500, self(), chain_check),
            {noreply, State};
        Chain ->
            ok = blockchain_event:add_handler(self()),
            {noreply, State#state{chain = Chain}}
    end;
handle_info(chain_check, State=#state{}) ->
    {noreply, State};
handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Utilities
%%

row_count(TableName, Conn) ->
    {ok, _, [{V}]} = epgsql:squery(Conn, lists:flatten(io_lib:format("select count(*) from ~s", [TableName]))),
    {Num, <<>>} = string:to_integer(V),
    Num.

connect_database([]) ->
    {undefined, []};
connect_database(HandlerModules) ->
    {ok, Conn} = psql_migration:open_connection([]),
    Handlers = lists:map(fun(Mod) ->
                                 {ok, State} = Mod:init(Conn),
                                 {Mod, State}
                         end, HandlerModules),
    {Conn, Handlers}.
