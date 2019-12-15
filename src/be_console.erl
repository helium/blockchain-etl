%%%-------------------------------------------------------------------
%% @doc bf_console
%% @end
%%%-------------------------------------------------------------------
-module(be_console).

-export([command/1]).

-spec command([string()]) -> ok.
command(Cmd) ->
    clique:run(Cmd).
