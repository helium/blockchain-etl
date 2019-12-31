-module(be_block_handler).

-callback init(epgsql:connection()) -> {ok, State::any()}.
-callback load(Hash::binary(), blockchain:block(), blockchain_ledger_v1:ledger(), State::any()) -> {ok, non_neg_integer(), NewState::any()}.

-type query() :: {Stmt::epgsql:statement(), Params::[any()]}.
-export_type([query/0]).

-export([run_queries/3]).

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
