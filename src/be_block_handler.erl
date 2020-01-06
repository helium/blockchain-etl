-module(be_block_handler).

-include("be_block_handler.hrl").

-callback init(epgsql:connection()) -> {ok, State::any()}.
-callback load(Hash::binary(), blockchain:block(), blockchain_ledger_v1:ledger(), State::any()) -> {ok, non_neg_integer(), NewState::any()}.

-type query() :: {Stmt::epgsql:statement(), Params::[any()]}.
-export_type([query/0]).

-export([run_queries/3, null_or_value/1, null_or_fn/2, null_or_b64/1, null_or_b58/1, null_or_h3/1]).

-spec run_queries([query()], epgsql:connection(), State::any()) -> {ok, non_neg_integer(), State::any()}.
run_queries(Queries, Conn, State) ->
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

-spec null_or_value(undefined | any()) -> null | any().
null_or_value(undefined) ->
    null;
null_or_value(V) ->
    V.

-spec null_or_fn(fun((any()) -> any()), undefined | any()) -> null | any().
null_or_fn(_Fun, undefined) ->
    null;
null_or_fn(Fun, V) ->
    Fun(V).

-spec null_or_b64(undefined | binary()) -> null | string().
null_or_b64(V) ->
    null_or_fn(fun(Bin) -> ?BIN_TO_B64(Bin) end, V).

-spec null_or_b58(undefined | binary()) -> null | string().
null_or_b58(V) ->
    null_or_fn(fun(Bin) -> ?BIN_TO_B58(Bin) end, V).

-spec null_or_h3(undefined | h3:h3index()) -> null | string().
null_or_h3(V) ->
    null_or_fn(fun(I) -> list_to_binary(h3:to_string(I)) end, V).
