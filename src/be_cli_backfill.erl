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
            backfill_receipts_challenger_usage(),
            backfill_reversed_receipts_path_usage(),
            backfill_gateway_names_usage(),
            backfill_oui_subnets_usage(),
            backfill_location_geometry_usage(),
            backfill_reward_gateways_usage(),
            backfill_gateway_location_hex_usage()
        ]
    ).

register_all_cmds() ->
    lists:foreach(
        fun(Cmds) ->
            [apply(clique, register_command, Cmd) || Cmd <- Cmds]
        end,
        [
            backfill_cmd(),
            backfill_receipts_challenger_cmd(),
            backfill_reversed_receipts_path_cmd(),
            backfill_gateway_names_cmd(),
            backfill_oui_subnets_cmd(),
            backfill_location_geometry_cmd(),
            backfill_reward_gateways_cmd(),
            backfill_gateway_location_hex_cmd()
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
            "  backfill receipts_challenger    - Backfill the challenger for receipts transactions.\n",
            "  backfill reversed_receipts_path - Backfill fix reversed poc receipts paths.\n",
            "  backfill gateway_names          - Backfill names in gateway_inventory.\n"
            "  backfill oui_subnets            - Backfill OUI inventory subnets.\n"
            "  backfill location_geometry      - Backfill location postgis geometries.\n"
            "  backfill reward_gateways        - Backfill reward gateways.\n"
            "  backfill gateway_location_hex   - Backfill location hex in gateway_inventory.\n"
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

%%
%% backfill reversed_receipts_paty
%%

backfill_reversed_receipts_path_cmd() ->
    [
        [
            ["backfill", "reversed_receipts_path"],
            [
                {max, [{longname, "max"}, {datatype, integer}]},
                {min, [{longname, "min"}, {datatype, integer}]}
            ],
            [],
            fun backfill_reversed_receipts_path/3
        ]
    ].

backfill_reversed_receipts_path_usage() ->
    [
        ["backfill", "reversed_receipts_path"],
        [
            "backfill reversed_receipts_path max=<max_block> min=<min_block>\n\n",
            "  Fixes receipts transactions that may have their paths reversed.\n\n"
            "  Run this migration for the range of blocks that ran version 1.1.44.\n\n"
            "Requires:\n\n"
            "  <max>\n"
            "    The maximum block to start search for transaction.\n"
            "  <min>\n"
            "    The block to end searches at. \n\n"
        ]
    ].

backfill_reversed_receipts_path(_CmdBase, [], []) ->
    usage;
backfill_reversed_receipts_path(_CmdBase, Keys, _) ->
    MinBlock = proplists:get_value(min, Keys),
    MaxBlock = proplists:get_value(max, Keys),

    Updated = be_db_backfill:reversed_receipts_path(
        MinBlock,
        MaxBlock,
        fun(Block, Updated) ->
            io:format("Fixed ~p transactions in block ~p~n", [
                Updated,
                Block
            ])
        end
    ),
    [clique_status:text(io_lib:format("Updated ~p", [Updated]))].

%%
%% backfill gateway_names
%%

backfill_gateway_names_cmd() ->
    [
        [
            ["backfill", "gateway_names"],
            [],
            [],
            fun backfill_gateway_names/3
        ]
    ].

backfill_gateway_names_usage() ->
    [
        ["backfill", "gateway_names"],
        [
            "backfill gateway_names \n\n",
            "  Fixes NULL animal names in the gateway_inventory table.\n\n"
        ]
    ].

backfill_gateway_names(_CmdBase, [], _) ->
    Updated = be_db_backfill:gateway_names(),
    [clique_status:text(io_lib:format("Updated ~p", [Updated]))].

%%
%% backfill oui_subnets
%%

backfill_oui_subnets_cmd() ->
    [
        [
            ["backfill", "oui_subnets"],
            [],
            [],
            fun backfill_oui_subnets/3
        ]
    ].

backfill_oui_subnets_usage() ->
    [
        ["backfill", "oui_subnets"],
        [
            "backfill oui_subnets \n\n",
            "  Fills the OUI inventory table with the current ledger subnets.\n\n"
        ]
    ].

backfill_oui_subnets(_CmdBase, [], _) ->
    Updated = be_db_backfill:oui_subnets(),
    [clique_status:text(io_lib:format("Updated ~p", [Updated]))].

%%
%% backfill location_geometry
%%

backfill_location_geometry_cmd() ->
    [
        [
            ["backfill", "location_geometry"],
            [],
            [],
            fun backfill_location_geometry/3
        ]
    ].

backfill_location_geometry_usage() ->
    [
        ["backfill", "location_geometry"],
        [
            "backfill location_geometry \n\n",
            "  Updates the locations table with the postgis geometry for each location.\n\n"
        ]
    ].

backfill_location_geometry(_CmdBase, [], _) ->
    Updated = be_db_backfill:location_geometry(),
    [clique_status:text(io_lib:format("Updated ~p", [Updated]))].

%%
%% backfill reward_gateways
%%

backfill_reward_gateways_cmd() ->
    [
        [
            ["backfill", "reward_gateways"],
            [
                {max, [{longname, "max"}, {datatype, integer}]},
                {min, [{longname, "min"}, {datatype, integer}]}
            ],
            [],
            fun backfill_reward_gateways/3
        ]
    ].

backfill_reward_gateways_usage() ->
    [
        ["backfill", "reward_gateways"],
        [
            "backfill reward gateways in transacion actors for max=<max_block> min=<min_block>\n\n",
            "  Inserts missing reward_gateway actors for rewards_v1 transactions.\n\n"
            "  Run this migration for the range of blocks that ran version 1.1.77.\n\n"
            "Requires:\n\n"
            "  <max>\n"
            "    The maximum block to start search for transactions.\n"
            "  <min>\n"
            "    The block to end searches at. \n\n"
        ]
    ].

backfill_reward_gateways(_CmdBase, [], []) ->
    usage;
backfill_reward_gateways(_CmdBase, Keys, _) ->
    MinBlock = proplists:get_value(min, Keys),
    MaxBlock = proplists:get_value(max, Keys),

    Updated = be_db_backfill:reward_gateways(
        MinBlock,
        MaxBlock
    ),
    [clique_status:text(io_lib:format("Updated ~p", [Updated]))].

%%
%% backfill gateway_location_hex
%%

backfill_gateway_location_hex_cmd() ->
    [
        [
            ["backfill", "gateway_location_hex"],
            [],
            [],
            fun backfill_gateway_location_hex/3
        ]
    ].

backfill_gateway_location_hex_usage() ->
    [
        ["backfill", "gateway_location_hex"],
        [
            "backfill location_hex \n\n",
            "  Fixes NULL location_hex entries in the gateway_inventory table.\n\n"
        ]
    ].

backfill_gateway_location_hex(_CmdBase, [], _) ->
    Updated = be_db_backfill:gateway_location_hex(),
    [clique_status:text(io_lib:format("Updated ~p", [Updated]))].
