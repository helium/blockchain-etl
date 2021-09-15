-module(be_db_follower).

-callback init(Args :: any()) -> {ok, State :: any()} | {error, term()}.
-callback load_chain(Conn :: term(), Chain :: blockchain:blockchain(), State :: any()) -> {ok, State :: any()} | {error, term()}.
-callback snap_loaded(Conn :: term(), Chain :: blockchain:blockchain(), State :: any()) -> {ok, State :: any()} | {error, term()}.
-callback load_block(
    Conn :: term(),
    Hash :: binary(),
    blockchain:block(),
    Sync :: boolean(),
    blockchain_ledger_v1:ledger(),
    State :: any()
) -> {ok, NewState :: any()}.

-optional_callbacks([load_chain/3, snap_loaded/3]).

-dialyzer(no_undefined_callbacks).
-behavior(blockchain_follower).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

%% blockchain_follower
-export([
    requires_sync/0,
    requires_ledger/0,
    init/1,
    follower_height/1,
    load_chain/2,
    snap_loaded/2,
    load_block/5,
    terminate/2
]).

%% utilities
-export([
    fold_actors/4,
    fold_actors/5,
    maybe_undefined/1,
    maybe_fn/2,
    maybe_b64/1,
    maybe_b58/1,
    maybe_h3/1,
    random_val_predicate/1,
    maybe_log_duration/2
]).

-define(HANDLER_MODULES, [
    be_db_block,
    be_db_txn_actor,
    be_db_account,
    be_db_gateway,
    be_db_oracle_price,
    be_db_vars,
    be_db_stats,
    be_db_reward,
    be_db_packet,
    be_db_validator,
    be_db_oui,
    be_db_dc_burn
]).

-record(state, {
    handler_state :: [{Module :: atom(), State :: any()}]
}).

requires_ledger() -> true.

requires_sync() -> true.

init(_) ->
    Handlers = lists:map(
        fun(Mod) ->
            {ok, State} = Mod:init([]),
            {Mod, State}
        end,
        ?HANDLER_MODULES
    ),
    {ok, #state{handler_state = Handlers}}.

follower_height(State = #state{}) ->
    case lists:keyfind(be_db_block, 1, State#state.handler_state) of
        false ->
            error(no_db_block);
        {be_db_block, BlockState} ->
            be_db_block:block_height(BlockState)
    end.

load_chain(Chain, State = #state{handler_state = Handlers}) ->
    LoadFun = fun(Conn) ->
        States =
            lists:foldl(
                fun({Handler, HandlerState}, HandlerStates) ->
                        case erlang:function_exported(Handler, load_chain, 3) of
                            true ->
                                Start = erlang:monotonic_time(millisecond),
                                {ok, NewHandlerState} =
                                Handler:load_chain(Conn, Chain, HandlerState),
                                maybe_log_duration(Handler, Start),
                                [{Handler, NewHandlerState} | HandlerStates];
                            false ->
                                [{Handler, HandlerState} | HandlerStates]
                        end
                end,
                [],
                Handlers
            ),
        {ok, lists:reverse(States)}
    end,

    Start = erlang:monotonic_time(millisecond),
    lager:info("Storing chain: ~p", [blockchain:height(Chain)]),
    {ok, HandlerStates} = ?WITH_TRANSACTION(LoadFun),
    End = erlang:monotonic_time(millisecond),
    lager:info("Stored chain took: ~p ms", [End - Start]),
    {ok, State#state{handler_state = HandlerStates}}.

snap_loaded(Chain, State = #state{handler_state = Handlers}) ->
    LoadFun = fun(Conn) ->
        States =
            lists:foldl(
                fun({Handler, HandlerState}, HandlerStates) ->
                        case erlang:function_exported(Handler, snap_loaded, 3) of
                            true ->
                                Start = erlang:monotonic_time(millisecond),
                                {ok, NewHandlerState} =
                                Handler:snap_loaded(Conn, Chain, HandlerState),
                                maybe_log_duration(Handler, Start),
                                [{Handler, NewHandlerState} | HandlerStates];
                            false ->
                                [{Handler, HandlerState} | HandlerStates]
                        end
                end,
                [],
                Handlers
            ),
        {ok, lists:reverse(States)}
    end,

    Start = erlang:monotonic_time(millisecond),
    lager:info("Storing snap: ~p", [blockchain:height(Chain)]),
    {ok, HandlerStates} = ?WITH_TRANSACTION(LoadFun),
    End = erlang:monotonic_time(millisecond),
    lager:info("Stored snap took: ~p ms", [End - Start]),
    {ok, State#state{handler_state = HandlerStates}}.

load_block(Hash, Block, Sync, Ledger, State = #state{}) ->
    LoadFun = fun(Conn) ->
        States =
            lists:foldl(
                fun({Handler, HandlerState}, HandlerStates) ->
                    Start = erlang:monotonic_time(millisecond),
                    {ok, NewHandlerState} =
                        Handler:load_block(Conn, Hash, Block, Sync, Ledger, HandlerState),
                    maybe_log_duration(Handler, Start),
                    [{Handler, NewHandlerState} | HandlerStates]
                end,
                [],
                State#state.handler_state
            ),
        {ok, lists:reverse(States)}
    end,

    Start = erlang:monotonic_time(millisecond),
    lager:info("Storing block: ~p", [blockchain_block_v1:height(Block)]),
    {ok, HandlerStates} = ?WITH_TRANSACTION(LoadFun),
    End = erlang:monotonic_time(millisecond),
    lager:info("Stored block: ~p took: ~p ms", [blockchain_block_v1:height(Block), End - Start]),
    {ok, State#state{handler_state = HandlerStates}}.

terminate(_Reason, _State) ->
    ok.

%%
%% Utilities
%%

maybe_log_duration(Item, Start) ->
    case application:get_env(blockchain_etl, log_handler_duration, true) of
        true ->
            End = erlang:monotonic_time(millisecond),
            lager:info("~p took ~p ms", [Item, End - Start]);
        _ ->
            ok
    end.

fold_actors(Roles, Fun, InitAcc, Block) ->
    fold_actors(Roles, Fun, InitAcc, Block, blockchain_worker:blockchain()).

fold_actors(Roles, Fun, InitAcc, Block, Chain) ->
    Txns = blockchain_block_v1:transactions(Block),
    %% Fetch actor keys that relate to accounts from each transaction's actors
    FilteredActors =
        fun(Actors) ->
            lists:filter(fun({Role, _Key}) -> lists:member(Role, Roles) end, Actors)
        end,
    ActorList = lists:usort(
        lists:flatten(
            lists:map(fun(Txn) -> FilteredActors(be_db_txn_actor:to_actors(Txn, Chain)) end, Txns)
        )
    ),
    lists:foldl(fun({Role, Key}, Acc) -> Fun({Role, Key}, Acc) end, InitAcc, ActorList).

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

random_val_predicate(Peer) ->
    not libp2p_peer:is_stale(Peer, timer:minutes(360)) andalso
        maps:get(<<"release_version">>, libp2p_peer:signed_metadata(Peer), undefined) /= undefined.
