-module(be_db_follower).

-callback init(Args :: any()) -> {ok, State :: any()} | {error, term()}.
-callback load_block(
    Conn :: term(),
    Hash :: binary(),
    blockchain:block(),
    Sync :: boolean(),
    blockchain_ledger_v1:ledger(),
    State :: any()
) -> {ok, NewState :: any()}.

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
    load_block/5,
    terminate/2
]).

%% utilities
-export([
    fold_actors/4,
    maybe_undefined/1,
    maybe_fn/2,
    maybe_b64/1,
    maybe_b58/1,
    maybe_h3/1
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
    be_db_oui
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

load_chain(_Chain, State = #state{}) ->
    {ok, State}.

load_block(Hash, Block, Sync, Ledger, State = #state{}) ->
    LoadFun = fun(Conn) ->
        States =
            lists:foldl(
                fun({Handler, HandlerState}, HandlerStates) ->
                    {ok, NewHandlerState} =
                        Handler:load_block(Conn, Hash, Block, Sync, Ledger, HandlerState),
                    [{Handler, NewHandlerState} | HandlerStates]
                end,
                [],
                State#state.handler_state
            ),
        {ok, lists:reverse(States)}
    end,

    lager:info("Storing block: ~p", [blockchain_block_v1:height(Block)]),
    {ok, HandlerStates} = ?WITH_TRANSACTION(LoadFun),
    {ok, #state{handler_state = HandlerStates}}.

terminate(_Reason, _State) ->
    ok.

%%
%% Utilities
%%

fold_actors(Roles, Fun, InitAcc, Block) ->
    Txns = blockchain_block_v1:transactions(Block),
    %% Fetch actor keys that relate to accounts from each transaction's actors
    FilteredActors =
        fun(Actors) ->
            lists:filter(fun({Role, _Key}) -> lists:member(Role, Roles) end, Actors)
        end,
    ActorList = lists:usort(
        lists:flatten(
            lists:map(fun(Txn) -> FilteredActors(be_db_txn_actor:to_actors(Txn)) end, Txns)
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
