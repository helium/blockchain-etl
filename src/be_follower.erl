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
         be_account,
         be_gateway
        ]).

-record(state,
        {
         chain :: blockchain:chain(),
         height :: non_neg_integer(),
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
    {ok, _, [{HeightStr}]} = epgsql:squery(Conn, "select max(height) from blocks"),
    Height = case HeightStr of
                 null -> 0;
                 _ -> binary_to_integer(HeightStr)
             end,
    lager:info("Block database at height: ~p", [Height]),
    self() ! chain_check,
    {ok, #state{handler_state=Handlers, height=Height, db_conn=Conn}}.

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

handle_info({blockchain_event, From, {add_block, Hash, _Sync, Ledger}}, State0=#state{height=Height, chain=Chain}) ->
    {ok, Block} = blockchain:get_block(Hash, Chain),
    BlockHeight = blockchain_block:height(Block),
    State = case BlockHeight of
        X when X == Height + 1 ->
            %% as expected, just continue below
            State0;
        X when X =< Height ->
            lager:info("ignoring block ~p", [BlockHeight]),
            %% already have these
            blockchain_event:acknowledge(From),
            %% throws count as early returns from gen_servers
            throw({noreply, State0});
        X when X > Height + 1 ->
            %% missing some blocks, try to obtain them
            BlockHeights = lists:seq(Height + 1, BlockHeight - 1),
            lager:info("trying to absorb missing blocks ~p", [BlockHeights]),
            MissingHandlerStates = lists:foldl(fun(MissingHeight, HS) ->
                                                       {ok, MissingBlock} = blockchain:get_block(MissingHeight, Chain),
                                                       MissingHash = blockchain_block:hash_block(MissingBlock),
                                                       {ok, MissingLedger} = blockchain:ledger_at(MissingHeight, Chain),
                                                       ingest_block(MissingHash, MissingBlock, MissingLedger, State0#state{handler_state=HS})
                                               end, State0#state.handler_state, BlockHeights),
            %% update the handler states and the height
            State0#state{handler_state=MissingHandlerStates, height=Height + length(BlockHeights)}
    end,
    HandlerStates = ingest_block(Hash, Block, Ledger, State),
    blockchain_event:acknowledge(From),
    {noreply, State#state{handler_state=HandlerStates, height= State#state.height + 1}};
handle_info({blockchain_event, From, Other}, State=#state{}) ->
    lager:info("Ignoring blockchain event: ~p", [Other]),
    blockchain_event:acknowledge(From),
    {noreply, State};

%% Wait fo chain to come up
handle_info(chain_check, State=#state{chain=undefined}) ->
    case blockchain_worker:blockchain() of
        undefined ->
            erlang:send_after(500, self(), chain_check),
            {noreply, State};
        Chain ->
            ok = blockchain_event:add_sync_handler(self()),
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

ingest_block(Hash, Block, Ledger, State) ->
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
    HandlerStates.
