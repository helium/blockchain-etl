-module(be_db_txn_actor).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-include_lib("helium_proto/include/blockchain_txn_rewards_v2_pb.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).
%% api
-export([to_actors/1]).

-define(S_INSERT_ACTOR, "insert_actor").

-record(state, {}).

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_INSERT_ACTOR,
            [
                "insert into transaction_actors (block, actor, actor_role, transaction_hash) ",
                "values ($1, $2, $3, $4) ",
                "on conflict do nothing"
            ],
            []
        ),

    #{
        ?S_INSERT_ACTOR => S1
    }.

%%
%% be_block_handler
%%

init(_) ->
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, _Ledger, State = #state{}) ->
    Queries = q_insert_transaction_actors(Block, [], State),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State}.

q_insert_transaction_actors(Block, Query, #state{}) ->
    Height = blockchain_block_v1:height(Block),
    Txns = blockchain_block_v1:transactions(Block),
    lists:foldl(
        fun(T, Acc) ->
            TxnHash = ?BIN_TO_B64(blockchain_txn:hash(T)),
            lists:foldl(
                fun({Role, Key}, ActorAcc) ->
                    [
                        {?S_INSERT_ACTOR, [Height, ?BIN_TO_B58(Key), Role, TxnHash]}
                        | ActorAcc
                    ]
                end,
                Acc,
                to_actors(T)
            )
        end,
        Query,
        Txns
    ).

-spec to_actors(blockchain_txn:txn()) -> [{string(), libp2p_crypto:pubkey_bin()}].
to_actors(T) ->
    to_actors(blockchain_txn:type(T), T).

to_actors(blockchain_txn_coinbase_v1, T) ->
    [{"payee", blockchain_txn_coinbase_v1:payee(T)}];
to_actors(blockchain_txn_security_coinbase_v1, T) ->
    [{"payee", blockchain_txn_security_coinbase_v1:payee(T)}];
to_actors(blockchain_txn_oui_v1, T) ->
    Routers = [{"router", R} || R <- blockchain_txn_oui_v1:addresses(T)],
    [
        {"owner", blockchain_txn_oui_v1:owner(T)},
        {"payer", blockchain_txn_oui_v1:payer(T)}
    ] ++ Routers;
to_actors(blockchain_txn_gen_gateway_v1, T) ->
    [
        {"gateway", blockchain_txn_gen_gateway_v1:gateway(T)},
        {"owner", blockchain_txn_gen_gateway_v1:owner(T)}
    ];
to_actors(blockchain_txn_routing_v1, T) ->
    Routers =
        case blockchain_txn_routing_v1:action(T) of
            {update_routers, Addrs} -> [{"router", R} || R <- Addrs];
            _ -> []
        end,
    [
        {"owner", blockchain_txn_routing_v1:owner(T)},
        {"payer", blockchain_txn_routing_v1:owner(T)}
    ] ++ Routers;
to_actors(blockchain_txn_payment_v1, T) ->
    [
        {"payer", blockchain_txn_payment_v1:payer(T)},
        {"payee", blockchain_txn_payment_v1:payee(T)}
    ];
to_actors(blockchain_txn_security_exchange_v1, T) ->
    [
        {"payer", blockchain_txn_security_exchange_v1:payer(T)},
        {"payee", blockchain_txn_security_exchange_v1:payee(T)}
    ];
to_actors(blockchain_txn_consensus_group_v1, T) ->
    [{"consensus_member", M} || M <- blockchain_txn_consensus_group_v1:members(T)];
to_actors(blockchain_txn_add_gateway_v1, T) ->
    Owner = blockchain_txn_add_gateway_v1:owner(T),
    Payer =
        case blockchain_txn_add_gateway_v1:payer(T) of
            undefined -> Owner;
            <<>> -> Owner;
            P -> P
        end,
    [
        {"gateway", blockchain_txn_add_gateway_v1:gateway(T)},
        {"owner", Owner},
        {"payer", Payer}
    ];
to_actors(blockchain_txn_assert_location_v1, T) ->
    Owner = blockchain_txn_assert_location_v1:owner(T),
    Payer =
        case blockchain_txn_assert_location_v1:payer(T) of
            undefined -> Owner;
            <<>> -> Owner;
            P -> P
        end,
    [
        {"gateway", blockchain_txn_assert_location_v1:gateway(T)},
        {"owner", Owner},
        {"payer", Payer}
    ];
to_actors(blockchain_txn_assert_location_v2, T) ->
    Owner = blockchain_txn_assert_location_v2:owner(T),
    Payer =
        case blockchain_txn_assert_location_v2:payer(T) of
            undefined -> Owner;
            <<>> -> Owner;
            P -> P
        end,
    [
        {"gateway", blockchain_txn_assert_location_v2:gateway(T)},
        {"owner", Owner},
        {"payer", Payer}
    ];
to_actors(blockchain_txn_create_htlc_v1, T) ->
    [
        {"payer", blockchain_txn_create_htlc_v1:payer(T)},
        {"payee", blockchain_txn_create_htlc_v1:payee(T)},
        {"escrow", blockchain_txn_create_htlc_v1:address(T)}
    ];
to_actors(blockchain_txn_redeem_htlc_v1, T) ->
    [
        {"payee", blockchain_txn_redeem_htlc_v1:payee(T)},
        {"escrow", blockchain_txn_redeem_htlc_v1:address(T)}
    ];
to_actors(blockchain_txn_poc_request_v1, T) ->
    [{"challenger", blockchain_txn_poc_request_v1:challenger(T)}];
to_actors(blockchain_txn_poc_receipts_v1, T) ->
    ToActors = fun
        (undefined, Acc0) ->
            Acc0;
        (Elem, {ChallengeeAcc0, WitnessAcc0}) ->
            ChallengeeAcc = [
                {"challengee", blockchain_poc_path_element_v1:challengee(Elem)}
                | ChallengeeAcc0
            ],
            WitnessAcc = lists:foldl(
                fun(W, WAcc) ->
                    [{"witness", blockchain_poc_witness_v1:gateway(W)} | WAcc]
                end,
                WitnessAcc0,
                blockchain_poc_path_element_v1:witnesses(Elem)
            ),
            {ChallengeeAcc, WitnessAcc}
    end,
    {Challengees, Witnesses} = lists:foldl(
        ToActors,
        {[], []},
        blockchain_txn_poc_receipts_v1:path(T)
    ),
    lists:usort(Challengees) ++
        lists:usort(Witnesses) ++
        [{"challenger", blockchain_txn_poc_receipts_v1:challenger(T)}];
to_actors(blockchain_txn_vars_v1, _T) ->
    [];
to_actors(blockchain_txn_rewards_v1, T) ->
    ToActors = fun(R, {PayeeAcc0, GatewayAcc0}) ->
        PayeeAcc = [{"payee", blockchain_txn_reward_v1:account(R)} | PayeeAcc0],
        GatewayAcc =
            case blockchain_txn_reward_v1:gateway(R) of
                undefined -> GatewayAcc0;
                G -> [{"reward_gateway", G} | GatewayAcc0]
            end,
        {PayeeAcc, GatewayAcc}
    end,
    {Payees, Gateways} = lists:foldl(
        ToActors,
        {[], []},
        blockchain_txn_rewards_v1:rewards(T)
    ),
    lists:usort(Payees) ++ lists:usort(Gateways);
to_actors(blockchain_txn_rewards_v2, T) ->
    Start = blockchain_txn_rewards_v2:start_epoch(T),
    End = blockchain_txn_rewards_v2:end_epoch(T),
    %% rewards_v2 violates to_actor conventions by requiring the chain and
    %% ledger to construct it actors
    Chain = blockchain_worker:blockchain(),
    {ok, Ledger} = blockchain:ledger_at(End, Chain),
    {ok, Metadata} = blockchain_txn_rewards_v2:calculate_rewards_metadata(
        Start,
        End,
        Chain
    ),
    %% Take a rewards map for a category and add payees and reward_gateways to
    %% one or both of the payee or gateway accumulators
    ToActors = fun(Rewards, Acc) ->
        maps:fold(
            fun
                ({owner, _Type, O}, _Amt, {PayeeAcc, GatewayAcc}) ->
                    {[{"payee", O} | PayeeAcc], GatewayAcc};
                ({gateway, _Type, G}, _Amt, {PayeeAcc, GatewayAcc}) ->
                    case blockchain_ledger_v1:find_gateway_owner(G, Ledger) of
                        {error, _Error} ->
                            {PayeeAcc, [{"reward_gateway", G} | GatewayAcc]};
                        {ok, GwOwner} ->
                            {[{"payee", GwOwner} | PayeeAcc], [{"reward_gateway", G} | GatewayAcc]}
                    end;
                ({validator, _Type, V}, _Amt, {PayeeAcc, GatewayAcc}) ->
                    case blockchain_ledger_v1:get_validator(V, Ledger) of
                        {error, _Error} ->
                            {PayeeAcc, [{"validator", V} | GatewayAcc]};
                        {ok, Validator} ->
                            Owner = blockchain_ledger_validator_v1:owner_address(Validator),
                            {[{"payee", Owner} | PayeeAcc], [{"validator", V} | GatewayAcc]}
                    end
            end,
            Acc,
            maps:iterator(Rewards)
        )
    end,
    %% Now fold over the metadata and call ToActors for each reward category
    {Payees, Gateways} = maps:fold(
        fun
            (overages, _Amount, Acc) ->
                Acc;
            (_RewardCategory, Rewards, Acc) ->
                ToActors(Rewards, Acc)
        end,
        {[], []},
        Metadata
    ),
    lists:usort(Payees) ++ lists:usort(Gateways);
to_actors(blockchain_txn_token_burn_v1, T) ->
    [
        {"payer", blockchain_txn_token_burn_v1:payer(T)},
        {"payee", blockchain_txn_token_burn_v1:payee(T)}
    ];
to_actors(blockchain_txn_dc_coinbase_v1, T) ->
    [{"payee", blockchain_txn_dc_coinbase_v1:payee(T)}];
to_actors(blockchain_txn_token_burn_exchange_rate_v1, _T) ->
    [];
to_actors(blockchain_txn_payment_v2, T) ->
    ToActors = fun(Payment, Acc) ->
        [{"payee", blockchain_payment_v2:payee(Payment)} | Acc]
    end,
    lists:foldl(
        ToActors,
        [{"payer", blockchain_txn_payment_v2:payer(T)}],
        blockchain_txn_payment_v2:payments(T)
    );
to_actors(blockchain_txn_state_channel_open_v1, T) ->
    %% TODO: In v1 state channels we're assuminig the the opener is
    %% the payer of the DC in the state channel.
    Opener = blockchain_txn_state_channel_open_v1:owner(T),
    [{"sc_opener", Opener}, {"payer", Opener}, {"owner", Opener}];
to_actors(blockchain_txn_state_channel_close_v1, T) ->
    %% NOTE: closer can be one of the clients of the state channel or the owner of the router
    %% if the state_channel expires
    SummaryToActors = fun(Summary, Acc) ->
        Receiver = blockchain_state_channel_summary_v1:client_pubkeybin(Summary),
        [{"packet_receiver", Receiver} | Acc]
    end,
    Closer = blockchain_txn_state_channel_close_v1:closer(T),
    lists:foldl(
        SummaryToActors,
        %% TODO: In v1 state channels we're assuminig the the
        %% closer is the payee of any remaining DC in the
        %% state channel. This is not totally true since any
        %% client in the state channel can cause it to close
        %% to, but for v1 we expect this assumption to hold.
        [
            {"sc_closer", Closer},
            {"payee", Closer},
            {"owner", blockchain_txn_state_channel_close_v1:state_channel_owner(T)}
        ],
        blockchain_state_channel_v1:summaries(
            blockchain_txn_state_channel_close_v1:state_channel(T)
        )
    );
to_actors(blockchain_txn_gen_price_oracle_v1, _T) ->
    [];
to_actors(blockchain_txn_price_oracle_v1, T) ->
    [{"oracle", blockchain_txn_price_oracle_v1:public_key(T)}];
to_actors(blockchain_txn_transfer_hotspot_v1, T) ->
    [
        {"gateway", blockchain_txn_transfer_hotspot_v1:gateway(T)},
        {"payee", blockchain_txn_transfer_hotspot_v1:seller(T)},
        {"payer", blockchain_txn_transfer_hotspot_v1:buyer(T)},
        {"owner", blockchain_txn_transfer_hotspot_v1:buyer(T)}
    ];
to_actors(blockchain_txn_gen_validator_v1, T) ->
    [
        {"validator", blockchain_txn_gen_validator_v1:address(T)},
        {"payer", blockchain_txn_gen_validator_v1:owner(T)},
        {"owner", blockchain_txn_gen_validator_v1:owner(T)}
    ];
to_actors(blockchain_txn_stake_validator_v1, T) ->
    [
        {"validator", blockchain_txn_stake_validator_v1:validator(T)},
        {"payer", blockchain_txn_stake_validator_v1:owner(T)},
        {"owner", blockchain_txn_stake_validator_v1:owner(T)}
    ];
to_actors(blockchain_txn_unstake_validator_v1, T) ->
    [
        {"validator", blockchain_txn_unstake_validator_v1:address(T)},
        {"payee", blockchain_txn_unstake_validator_v1:owner(T)},
        {"owner", blockchain_txn_unstake_validator_v1:owner(T)}
    ];
to_actors(blockchain_txn_transfer_validator_stake_v1, T) ->
    OldOwner = blockchain_txn_transfer_validator_stake_v1:old_owner(T),
    NewOwner = blockchain_txn_transfer_validator_stake_v1:new_owner(T),
    Owners =
        case NewOwner of
            OldOwner -> [{"owner", OldOwner}];
            <<>> -> [{"owner", OldOwner}];
            _ -> [{"owner", NewOwner}, {"owner", OldOwner}]
        end,
    [
        {"validator", blockchain_txn_transfer_validator_stake_v1:old_validator(T)},
        {"validator", blockchain_txn_transfer_validator_stake_v1:new_validator(T)},
        {"payer", blockchain_txn_transfer_validator_stake_v1:new_owner(T)},
        {"payee", blockchain_txn_transfer_validator_stake_v1:old_owner(T)}
    ] ++ Owners;
to_actors(blockchain_txn_validator_heartbeat_v1, T) ->
    [
        {"validator", blockchain_txn_validator_heartbeat_v1:address(T)}
    ];
to_actors(blockchain_txn_consensus_group_failure_v1, T) ->
    Members = [
        {"consensus_member", M}
     || M <- blockchain_txn_consensus_group_failure_v1:members(T)
    ],
    FailedMembers = [
        {"consensus_member", M}
     || M <- blockchain_txn_consensus_group_failure_v1:failed_members(T)
    ],
    Members ++ FailedMembers.
