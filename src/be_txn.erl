-module(be_txn).

-export([to_json/1, to_json/2, to_json/3]).
-export([actors_to_copy_list/2]).

-include("be_db_follower.hrl").

actors_to_copy_list(Height, Txns) ->
    RawList = [to_actors_list(Height, Txn) || Txn <- Txns],
    be_utils:flatten_once(RawList).

to_actors_list(Height, Txn) ->
    TxnHash = ?BIN_TO_B64(blockchain_txn:hash(Txn)),
    Actors = be_db_txn_actor:to_actors(Txn),
    [[?BIN_TO_B58(Key), list_to_binary(Role), TxnHash, Height] || {Role, Key} <- Actors].

to_json(T) ->
    to_json(T, []).

to_json(T, Opts) ->
    Type = blockchain_txn:json_type(T),
    to_json(Type, T, Opts).

to_json(<<"poc_request_v1">>, T, Opts) ->
    {ledger, Ledger} = lists:keyfind(ledger, 1, Opts),
    Json = #{challenger := Challenger} = blockchain_txn:to_json(T, Opts),
    {ok, ChallengerInfo} = blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Challenger), Ledger),
    ChallengerLoc = blockchain_ledger_gateway_v2:location(ChallengerInfo),
    Json#{
        challenger_owner => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(ChallengerInfo)),
        challenger_location => ?MAYBE_H3(ChallengerLoc)
    };
to_json(<<"poc_receipts_v1">>, T, Opts) ->
    {ledger, Ledger} = lists:keyfind(ledger, 1, Opts),
    Json = #{challenger := Challenger, path := Path} = blockchain_txn:to_json(T, Opts),
    UpdateWitness = fun(WitnessJson = #{gateway := Witness}) ->
        {ok, WitnessInfo} = blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Witness), Ledger),
        WitnessLoc = blockchain_ledger_gateway_v2:location(WitnessInfo),
        WitnessJson#{
            owner => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(WitnessInfo)),
            location => ?MAYBE_H3(WitnessLoc)
        }
    end,

    UpdatePath = fun(PathJson = #{challengee := Challengee, witnesses := Witnesses}) ->
        {ok, ChallengeeInfo} = blockchain_ledger_v1:find_gateway_info(
            ?B58_TO_BIN(Challengee),
            Ledger
        ),
        ChallengeeLoc = blockchain_ledger_gateway_v2:location(ChallengeeInfo),
        PathJson#{
            challengee_owner => ?BIN_TO_B58(
                blockchain_ledger_gateway_v2:owner_address(ChallengeeInfo)
            ),
            challengee_location => ?MAYBE_H3(ChallengeeLoc),
            witnesses => [UpdateWitness(W) || W <- Witnesses]
        }
    end,

    {ok, ChallengerInfo} = blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Challenger), Ledger),
    ChallengerLoc = blockchain_ledger_gateway_v2:location(ChallengerInfo),
    Json#{
        challenger_owner => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(ChallengerInfo)),
        challenger_location => ?MAYBE_H3(ChallengerLoc),
        path => [UpdatePath(E) || E <- Path]
    };
to_json(<<"state_channel_close_v1">>, T, Opts) ->
    {ledger, Ledger} = lists:keyfind(ledger, 1, Opts),
    Json = #{state_channel := SCJson} = blockchain_txn:to_json(T, Opts),
    UpdateSummary = fun(Summary = #{client := Client}) ->
        case blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Client), Ledger) of
            {error, _} ->
                Summary;
            {ok, ClientInfo} ->
                blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Client), Ledger),
                ClientLoc = blockchain_ledger_gateway_v2:location(ClientInfo),
                Summary#{
                    owner => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(ClientInfo)),
                    location => ?MAYBE_H3(ClientLoc)
                }
        end
    end,

    Json#{
        state_channel => SCJson#{
            summaries => [UpdateSummary(S) || S <- maps:get(summaries, SCJson)]
        }
    };
to_json(<<"rewards_v2">>, T, Opts) ->
    {chain, Chain} = lists:keyfind(chain, 1, Opts),
    Start = blockchain_txn_rewards_v2:start_epoch(T),
    End = blockchain_txn_rewards_v2:end_epoch(T),
    {ok, Metadata} = be_db_reward:calculate_rewards_metadata(Start, End, Chain),
    blockchain_txn:to_json(T, Opts ++ [{rewards_metadata, Metadata}]);
to_json(_Type, T, Opts) ->
    blockchain_txn:to_json(T, Opts).
