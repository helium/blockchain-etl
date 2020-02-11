-module(be_follower).

-behaviour(gen_server).

-include("be_db_worker.hrl").

%% API
-export([start_link/0, counts/0, height/0, is_syncing/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(CACHE_HEIGHT, height).
-define(CACHE_SYNC, sync).
-define(CACHE, be_cache).

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
         handler_state :: [{Module::atom(), State::any()}]
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec height() -> pos_integer().
height() ->
    ets:lookup_element(?CACHE, ?CACHE_HEIGHT, 2).

-spec is_syncing() -> boolean().
is_syncing() ->
    ets:lookup_element(?CACHE, ?CACHE_SYNC, 2).

counts() ->
    ?WITH_CONNECTION(fun(Conn) ->
                             #{
                               block_count => row_count("blocks", Conn),
                               transaction_count => row_count("transactions", Conn),
                               actor_count => row_count("transaction_actors", Conn),
                               block_sig_count => row_count("block_signatures", Conn),
                               account_count => row_count("accounts", Conn)
                              }
                     end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Handlers} = init_handlers(?HANDLER_MODULES),
    ets:new(?CACHE, [public, named_table, {read_concurrency, true}]),
    Height = case ?EQUERY("select max(height) from blocks", []) of
                 {ok, _, [{null}]} -> 0;
                 {ok, _, [{H}]} -> H
             end,
    lager:info("Block database at height: ~p", [Height]),
    ets:insert(?CACHE, [{?CACHE_HEIGHT, Height},
                        {?CACHE_SYNC, true}
                       ]),
    self() ! chain_check,
    {ok, #state{handler_state=Handlers}}.

handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({blockchain_event, From, {add_block, Hash, Sync, Ledger}}, State0=#state{chain=Chain}) ->
    {ok, Block} = blockchain:get_block(Hash, Chain),
    Height = ets:lookup_element(?CACHE, ?CACHE_HEIGHT, 2),
    BlockHeight = blockchain_block:height(Block),
    {State, NewHeight} =
        case BlockHeight of
            X when X == Height + 1 ->
            %% as expected, just continue below
            {State0, Height};
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
                    MissingHandlerStates = lists:foldl(
                                             fun(MissingHeight, HS) ->
                                                     {ok, MissingBlock} = blockchain:get_block(MissingHeight, Chain),
                                                     MissingHash = blockchain_block:hash_block(MissingBlock),
                                                     {ok, MissingLedger} = blockchain:ledger_at(MissingHeight, Chain),
                                                     ingest_block(MissingHash, MissingBlock, true, MissingLedger,
                                                                  State0#state{handler_state=HS})
                                             end, State0#state.handler_state, BlockHeights),
                    %% update the handler states and the height
                    {State0#state{handler_state=MissingHandlerStates}, Height + length(BlockHeights)}
            end,
    HandlerStates = ingest_block(Hash, Block, Sync, Ledger, State),
    blockchain_event:acknowledge(From),
    ets:insert(?CACHE, [{?CACHE_HEIGHT, NewHeight + 1},
                        {?CACHE_SYNC, Sync}
                       ]),
    {noreply, State#state{handler_state=HandlerStates}};
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

init_handlers(HandlerModules) ->
    InitFun = fun(Conn) ->
                      lists:map(fun(Mod) ->
                                        {ok, State} = Mod:init(Conn),
                                        {Mod, State}
                                end, HandlerModules)
              end,
    Handlers = ?WITH_CONNECTION(InitFun),
    {ok, Handlers}.


ingest_block(Hash, Block, Sync, Ledger, State) ->
    Start = erlang:monotonic_time(),
    LoadFun = fun(Conn) ->
                      {Counts, States} =
                          lists:foldl(fun({Handler, HandlerState}, {CountMap, HandlerStates}) ->
                                              {ok, Count, NewHandlerState} =
                                                  Handler:load(Conn, Hash, Block, Sync, Ledger, HandlerState),
                                              {CountMap#{Handler => Count},
                                               [{Handler, NewHandlerState} | HandlerStates]}
                                      end, {#{}, []}, State#state.handler_state),
                      {ok, {Counts, lists:reverse(States)}}
              end,

    lager:info("Storing block: ~p", [blockchain_block_v1:height(Block)]),
    {ok, {Counts, HandlerStates}} = ?WITH_TRANSACTION(LoadFun),

    Latency = erlang:monotonic_time() - Start,
    telemetry:execute([be_follower, add_block],
                      Counts#{latency => Latency},
                      #{block => blockchain_block_v1:height(Block) }),
    HandlerStates.
