-module(be_block_handler).

-include("be_block_handler.hrl").

-callback init(epgsql:connection()) -> {ok, State::any()}.
-callback load(Conn::epgsql:connection(),
               Hash::binary(),
               blockchain:block(),
               Sync::boolean(),
               blockchain_ledger_v1:ledger(),
               State::any()) -> {ok, non_neg_integer(), NewState::any()}.

-type query() :: {Stmt::epgsql:statement(), Params::[any()]}.
-export_type([query/0]).

-export([run_queries/3, maybe_undefined/1, maybe_fn/2, maybe_b64/1, maybe_b58/1, maybe_h3/1]).

-spec run_queries(epgsql:connection(), [query()], State::any()) -> {ok, QueryCount::non_neg_integer(), State::any()}.
run_queries(Conn, Queries, State) ->
    Results = epgsql:execute_batch(Conn, Queries),
    %% Find any errors and throw an error
    %% to alloow roll back
    case lists:filter(fun({ok, _}) -> false;
                         ({error, _}) -> true
                      end, Results) of
        [] -> {ok, length(Queries), State};
        Errors ->
            throw({load_error, Errors})
    end.

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
