%%%-------------------------------------------------------------------
%% @doc bf_cli_info
%% @end
%%%-------------------------------------------------------------------
-module(be_cli_info).

-behavior(clique_handler).

-export([register_cli/0]).

-export([get_info/0]).

register_cli() ->
    register_all_usage(),
    register_all_cmds().

register_all_usage() ->
    lists:foreach(fun(Args) ->
                          apply(clique, register_usage, Args)
                  end,
                  [
                   info_usage(),
                   info_height_usage(),
                   info_name_usage()
                  ]).

register_all_cmds() ->
    lists:foreach(fun(Cmds) ->
                          [apply(clique, register_command, Cmd) || Cmd <- Cmds]
                  end,
                  [
                   info_cmd(),
                   info_height_cmd(),
                   info_name_cmd()
                  ]).
%%
%% info
%%

info_usage() ->
    [["info"],
     ["blockchain_follower info commands\n\n",
      "  info height - Get height of the blockchain for this blockchain_follower.\n",
      "  info in_consensus - Show if this blockchain_follower is in the consensus_group.\n"
      "  name - Shows the name of this blockchain_follower.\n"
      "  block_age - Get age of the latest block in the chain, in seconds.\n"
      "  p2p_status - Shows key peer connectivity status of this blockchain_follower.\n"
     ]
    ].

info_cmd() ->
    [
     [["info"], [], [], fun(_, _, _) -> usage end]
    ].


%%
%% info height
%%

info_height_cmd() ->
    [
     [["info", "height"], [], [], fun info_height/3]
    ].

info_height_usage() ->
    [["info", "height"],
     ["info height \n\n",
      "  Get height of the blockchain for this blockchain_follower.\n\n"
      "  The first number is the current election epoch, and the second is\n"
      "  the block height.  If the second number is displayed with an asterisk (*)\n"
      "  this node has yet to sync past the assumed valid hash in the node config.\n\n"
     ]
    ].

get_info() ->
    Chain = blockchain_worker:blockchain(),
    {ok, Height} = blockchain:height(Chain),
    {ok, SyncHeight} = blockchain:sync_height(Chain),
    {ok, HeadBlock} = blockchain:head_block(Chain),
    {Epoch, _} = blockchain_block_v1:election_info(HeadBlock),
    {Height, SyncHeight, Epoch}.

info_height(["info", "height"], [], []) ->
    {Height, SyncHeight, Epoch0} = get_info(),
    Epoch = integer_to_list(Epoch0),

    case SyncHeight == Height of
        true ->
            [clique_status:text(Epoch ++ "\t\t" ++ integer_to_list(Height))];
        false ->
            [clique_status:text([Epoch, "\t\t", integer_to_list(Height), "\t\t", integer_to_list(SyncHeight), "*"])]
    end;
info_height([_, _, _], [], []) ->
    usage.


%%
%% info name
%%

info_name_cmd() ->
    [
     [["info", "name"], [], [], fun info_name/3]
    ].

info_name_usage() ->
    [["info", "name"],
     ["info name \n\n",
      "  Get name for this blockchain_follower.\n\n"
     ]
    ].

info_name(["info", "name"], [], []) ->
    {ok, Name} = erl_angry_purple_tiger:animal_name(libp2p_crypto:bin_to_b58(blockchain_swarm:pubkey_bin())),
    [clique_status:text(Name)];
info_name([_, _, _], [], []) ->
    usage.
