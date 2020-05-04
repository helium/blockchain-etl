-module(be_follower).

-callback init() -> {ok, State::any()} | {error, term()}.
-callback load(Hash::binary(),
               blockchain:block(),
               Sync::boolean(),
               blockchain_ledger_v1:ledger(),
               State::any()) -> {ok, NewState::any()}.

-behaviour(gen_server).

-include("be_follower.hrl").

%% API
-export([start_link/1, height/0, is_syncing/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Utilities
-export([maybe_undefined/1, maybe_fn/2, maybe_b64/1, maybe_b58/1, maybe_h3/1]).

-define(SERVER, ?MODULE).

-record(state,
        {
         chain :: blockchain:chain(),
         handler_state :: {Module::atom(), State::any()}
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

-spec height() -> pos_integer().
height() ->
    ets:lookup_element(?CACHE, ?CACHE_HEIGHT, 2).

-spec is_syncing() -> boolean().
is_syncing() ->
    ets:lookup_element(?CACHE, ?CACHE_SYNC, 2).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    ok = blockchain_event:add_sync_handler(self()),
    ets:new(?CACHE, [public, named_table, {read_concurrency, true}]),
    ets:insert(?CACHE, [
                        {?CACHE_SYNC, true}
                       ]),
    HandlerMod = proplists:get_value(handler, Args),
    {ok, HandlerState} = HandlerMod:init(),
    blockchain_worker:load(application:get_env(blockchain, base_dir, "data"),
                           "update"),
    {ok, #state{handler_state = {HandlerMod, HandlerState}}}.

handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({blockchain_event, From, {new_chain, Chain}}, State0=#state{}) ->
    blockchain_event:acknowledge(From),
    {noreply, State0#state{chain=Chain}};
handle_info({blockchain_event, From, {add_block, Hash, Sync, Ledger}}, State=#state{chain=Chain}) ->
    {ok, Block} = blockchain:get_block(Hash, Chain),
    Height = height(),
    BlockHeight = blockchain_block:height(Block),
    {HandlerMod, HandlerState} =
        case BlockHeight of
            X when X == Height + 1 ->
                %% as expected, just continue below
                State#state.handler_state;
        X when X =< Height ->
            lager:info("ignoring block ~p", [BlockHeight]),
            %% already have these
            blockchain_event:acknowledge(From),
            %% throws count as early returns from gen_servers
            throw({noreply, State});
        X when X > Height + 1 ->
                %% missing some blocks, try to obtain them
                BlockHeights = lists:seq(Height + 1, BlockHeight - 1),
                lager:info("trying to absorb missing blocks ~p", [BlockHeights]),
                MissingHandlerState = lists:foldl(
                                        fun(MissingHeight, {Mod, HS}) ->
                                                {ok, MissingBlock} = blockchain:get_block(MissingHeight, Chain),
                                                MissingHash = blockchain_block:hash_block(MissingBlock),
                                                {ok, MissingLedger} = blockchain:ledger_at(MissingHeight, Chain),
                                                {ok, NewHS} = Mod:load(MissingHash,
                                                                       MissingBlock,
                                                                       true,
                                                                       MissingLedger,
                                                                       HS),
                                                {Mod, NewHS}
                                        end, State#state.handler_state, BlockHeights),
                %% update the handler states and the height
                MissingHandlerState
        end,
    {ok, NewHandlerState} = HandlerMod:load(Hash, Block, Sync, Ledger, HandlerState),
    blockchain_event:acknowledge(From),
    ets:insert(?CACHE, [
                        {?CACHE_SYNC, Sync}
                       ]),
    {noreply, State#state{handler_state={HandlerMod, NewHandlerState}}};
handle_info({blockchain_event, From, Other}, State=#state{}) ->
    lager:info("Ignoring blockchain event: ~p", [Other]),
    blockchain_event:acknowledge(From),
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

-spec maybe_undefined(any() | undefined | null) -> any() | undefined.
maybe_undefined(undefined) ->
    undefined;
maybe_undefined(null) ->
    undefined;
maybe_undefined(V) ->
    V.

-spec maybe_fn(fun((any()) -> any()), undefined | null | any()) -> undefined | any().
maybe_fn(_Fun, undefined) ->
    undefined;
maybe_fn(_Fun, null) ->
    undefined;
maybe_fn(Fun, V) ->
    Fun(V).

-spec maybe_b64(undefined | binary()) -> null | string().
maybe_b64(V) ->
    maybe_fn(fun(Bin) -> ?BIN_TO_B64(Bin) end, V).

-spec maybe_b58(undefined | binary()) -> null | binary().
maybe_b58(V) ->
    maybe_fn(fun(Bin) -> ?BIN_TO_B58(Bin) end, V).

-spec maybe_h3(undefined | h3:h3index()) -> undefined | binary().
maybe_h3(V) ->
    maybe_fn(fun(I) -> list_to_binary(h3:to_string(I)) end, V).
