%%%-------------------------------------------------------------------
%% @doc bf_cli_backfill
%% @end
%%%-------------------------------------------------------------------
-module(be_cli_backfill).

-behavior(clique_handler).

-export([register_cli/0]).

register_cli() ->
    register_all_usage(),
    register_all_cmds().

register_all_usage() ->
    lists:foreach(
        fun(Args) ->
            apply(clique, register_usage, Args)
        end,
        [
            backfill_usage(),
            backfill_receipts_challenger_usage()
        ]
    ).

register_all_cmds() ->
    lists:foreach(
        fun(Cmds) ->
            [apply(clique, register_command, Cmd) || Cmd <- Cmds]
        end,
        [
            backfill_cmd(),
            backfill_receipts_challenger_cmd()
        ]
    ).

%%
%% backfill
%%

backfill_usage() ->
    [
        ["backfill"],
        [
            "backfill commands\n\n",
            "  backfill receipts_challeneger - Backfill the challenger for receipts transactions.\n"
        ]
    ].

backfill_cmd() ->
    [
        [["backfill"], [], [], fun(_, _, _) -> usage end]
    ].

%%
%% backfill receipts_challenger
%%

backfill_receipts_challenger_cmd() ->
    [
        [
            ["backfill", "receipts_challenger"],
            [
                {max, [{longname, "max"}, {datatype, integer}]},
                {min, [{longname, "min"}, {datatype, integer}]}
            ],
            [
                {batch, [{longname, "batch"}, {datatype, integer}]}
            ],
            fun backfill_receipts_challenger/3
        ]
    ].

backfill_receipts_challenger_usage() ->
    [
        ["backfill", "receipts_challenger"],
        [
            "backfill receipts_challenger max=<max_block> min=<min_block> [--batch <batch_size>]\n\n",
            "  Backfill the challenger as an actor for each poc_recipts_v1 transaction.\n\n"
            "  This backfills backwards from the gievn max block to the given min block.\n"
            "  Overlapping block ranges from previous runs will do no harm. \n"
            "  If you do not run this backfill ETL will collect challenger informationf or receipts\n"
            "  as of the block it started running version 1.1.44.\n\n"
            "Requires:\n\n"
            "  <max>\n"
            "    The maximum block to start search for transaction.\n"
            "  <min>\n"
            "    The block to end searches at. \n\n"
            "Options:\n\n"
            "  --batch <bath_size>"
            "    The batch size to break the block range into. This helps with database performance (default 5000)\n"
        ]
    ].

backfill_receipts_challenger(_CmdBase, [], []) ->
    usage;
backfill_receipts_challenger(_CmdBase, Keys, Flags) ->
    MinBlock = proplists:get_value(min, Keys),
    MaxBlock = proplists:get_value(max, Keys),
    %% Options
    BatchSize = proplists:get_value(batch, Flags, 5000),

    Inserted = be_db_backfill:receipts_challenger(
        MinBlock,
        MaxBlock,
        BatchSize,
        fun(LastMin, LastMax, LastInserted) ->
            io:format("Processed from ~p to ~p, batch ~p: ~p inserted~n", [
                LastMin,
                LastMax,
                BatchSize,
                LastInserted
            ])
        end
    ),
    [clique_status:text(io_lib:format("Inserted ~p", [Inserted]))].
