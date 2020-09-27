-module(be_cli_registry).

-define(CLI_MODULES, [
    be_cli_info,
    be_cli_genesis,
    be_cli_backfill
]).

-export([register_cli/0]).

register_cli() ->
    clique:register(?CLI_MODULES).
