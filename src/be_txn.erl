-module(be_txn).
-export([to_json/2]).

-include("be_db_follower.hrl").

to_json(T, Ledger) ->
    Json=#{ type:= Type} = blockchain_txn:to_json(T, []),
    to_json(Type, Json, Ledger).

to_json(<<"poc_request_v1">>, Json=#{ challenger := Challenger }, Ledger) ->
    {ok, ChallengerInfo} = blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Challenger), Ledger),
    ChallengerLoc = blockchain_ledger_gateway_v2:location(ChallengerInfo),
    Json#{
          challenger_owner => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(ChallengerInfo)),
          challenger_location => ?MAYBE_H3(ChallengerLoc)
         };
to_json(<<"poc_receipts_v1">>, Json=#{challenger := Challenger, path := Path}, Ledger) ->
    UpdateWitness =
        fun(WitnessJson=#{ gateway := Witness }) ->
                {ok, WitnessInfo} = blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Witness), Ledger),
                WitnessLoc = blockchain_ledger_gateway_v2:location(WitnessInfo),
                WitnessJson#{
                             owner => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(WitnessInfo)),
                             location => ?MAYBE_H3(WitnessLoc)
                            }
        end,

    UpdatePath =
        fun(PathJson=#{ challengee := Challengee, witnesses := Witnesses}) ->
                {ok, ChallengeeInfo} = blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Challengee), Ledger),
                ChallengeeLoc = blockchain_ledger_gateway_v2:location(ChallengeeInfo),
                PathJson#{
                          challengee_owner => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(ChallengeeInfo)),
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
to_json(<<"state_channel_close_v1">>, Json=#{ state_channel := SCJson } , Ledger) ->

    UpdateSummary =
        fun(Summary=#{ client := Client }) ->
                case blockchain_ledger_v1:find_gateway_info(?B58_TO_BIN(Client), Ledger) of
                    {error, _} -> Summary;
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
           state_channel => SCJson#{ summaries => [UpdateSummary(S) || S <- maps:get(summaries, SCJson)]}
         };

to_json(_Type, Json, _Ledger) ->
    Json.
