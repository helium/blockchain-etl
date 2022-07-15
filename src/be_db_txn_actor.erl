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
-export([to_actors/1, q_insert_transaction_actors/2]).

-define(S_INSERT_ACTOR, "insert_actor").
-define(S_INSERT_ACTOR_10, "insert_actor_10").
-define(S_INSERT_ACTOR_100, "insert_actor_100").

-record(state, {}).

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    MkQueryFun = fun(Rows) ->
        epgsql:parse(
            Conn,
            ?S_INSERT_ACTOR ++ "_" ++ integer_to_list(Rows),
            [
                "insert into transaction_actors (block, actor, actor_role, transaction_hash) ",
                "values  ",
                be_utils:make_values_list(4, Rows),
                "on conflict do nothing"
            ],
            []
        )
    end,
    {ok, S1} = MkQueryFun(1),
    {ok, S10} = MkQueryFun(10),
    {ok, S100} = MkQueryFun(100),
    #{
        ?S_INSERT_ACTOR => S1,
        ?S_INSERT_ACTOR_10 => S10,
        ?S_INSERT_ACTOR_100 => S100
    }.

%%
%% be_block_handler
%%

init(_) ->
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, _Ledger, State = #state{}) ->
    Queries = q_insert_block_transaction_actors(Block),
    execute_queries(Conn, Queries),
    {ok, State}.

execute_queries(Conn, Queries) when length(Queries) > 100 ->
    lists:foreach(
        fun
            (Q) when length(Q) == 100 ->
                %% Can't match 100 in the success case since conflicts are ignored
                {ok, _} = ?PREPARED_QUERY(Conn, ?S_INSERT_ACTOR_100, lists:flatten(Q));
            (Q) ->
                execute_queries(Conn, Q)
        end,
        be_utils:split_list(Queries, 100)
    );
execute_queries(Conn, Queries) when length(Queries) > 10 ->
    lists:foreach(
        fun
            (Q) when length(Q) == 10 ->
                %% Can't match 10 in the success case since conflicts are ignored
                {ok, _} = ?PREPARED_QUERY(Conn, ?S_INSERT_ACTOR_10, lists:flatten(Q));
            (Q) ->
                execute_queries(Conn, Q)
        end,
        be_utils:split_list(Queries, 10)
    );
execute_queries(Conn, Queries) ->
    ok = ?BATCH_QUERY(Conn, [{?S_INSERT_ACTOR, I} || I <- Queries]).

q_insert_transaction_actors(Height, Txn) ->
    TxnHash = ?BIN_TO_B64(blockchain_txn:hash(Txn)),
    lists:map(
        fun({Role, Key}) ->
            [Height, ?BIN_TO_B58(Key), list_to_binary(Role), TxnHash]
        end,
        to_actors(Txn)
    ).

q_insert_block_transaction_actors(Block) ->
    Height = blockchain_block_v1:height(Block),
    Txns = blockchain_block_v1:transactions(Block),
    lists:flatmap(
        fun(Txn) ->
            q_insert_transaction_actors(Height, Txn)
        end,
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
to_actors(blockchain_txn_poc_receipts_v2, T) ->
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
        blockchain_txn_poc_receipts_v2:path(T)
    ),
    lists:usort(Challengees) ++
        lists:usort(Witnesses) ++
        [
            {"challenger", blockchain_txn_poc_receipts_v2:challenger(T)},
            {"validator", blockchain_txn_poc_receipts_v2:challenger(T)}
        ];
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
    {ok, Metadata} = be_db_reward:calculate_rewards_metadata(
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
to_actors(blockchain_txn_transfer_hotspot_v2, T) ->
    [
        {"gateway", blockchain_txn_transfer_hotspot_v2:gateway(T)},
        {"owner", blockchain_txn_transfer_hotspot_v2:new_owner(T)},
        % the seller (owner) pays the transaction fees, so use it to ensure it
        % shows up in the sellers activity feed.
        {"payer", blockchain_txn_transfer_hotspot_v2:owner(T)}
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
        {"consensus_failure_member", M}
     || M <- blockchain_txn_consensus_group_failure_v1:members(T)
    ],
    FailedMembers = [
        {"consensus_failure_failed_member", M}
     || M <- blockchain_txn_consensus_group_failure_v1:failed_members(T)
    ],
    Members ++ FailedMembers;
to_actors(blockchain_txn_add_subnetwork_v1, T) ->
    RewardServers = [
        {"reward_server", M}
     || M <- blockchain_txn_add_subnetwork_v1:reward_server_keys(T)
    ],
    SubnetworkKey = [{"subnetwork_key", blockchain_txn_add_subnetwork_v1:subnetwork_key(T)}],
    SubnetworkKey ++ RewardServers;
to_actors(blockchain_txn_subnetwork_rewards_v1, T) ->
    lists:map(
        fun(R) -> {"payee", blockchain_txn_subnetwork_rewards_v1:reward_account(R)} end,
        blockchain_txn_subnetwork_rewards_v1:rewards(T)
    ).
