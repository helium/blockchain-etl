-module(be_txn).
-export([to_type/1, to_json/2]).

-include("be_follower.hrl").

-spec to_type(atom()) -> string().
to_type(blockchain_txn_coinbase_v1) ->
    "coinbase_v1";
to_type(blockchain_txn_security_coinbase_v1) ->
    "security_coinbase_v1";
to_type(blockchain_txn_oui_v1) ->
    "oui_v1";
to_type(blockchain_txn_gen_gateway_v1) ->
    "gen_gateway_v1";
to_type(blockchain_txn_routing_v1) ->
    "routing_v1";
to_type(blockchain_txn_payment_v1) ->
    "payment_v1";
to_type(blockchain_txn_security_exchange_v1) ->
    "security_exchange_v1";
to_type(blockchain_txn_consensus_group_v1) ->
    "consensus_group_v1";
to_type(blockchain_txn_add_gateway_v1) ->
    "add_gateway_v1";
to_type(blockchain_txn_assert_location_v1) ->
    "assert_location_v1";
to_type(blockchain_txn_create_htlc_v1) ->
    "create_htlc_v1";
to_type(blockchain_txn_redeem_htlc_v1) ->
    "redeem_htlc_v1";
to_type(blockchain_txn_poc_request_v1) ->
    "poc_request_v1";
to_type(blockchain_txn_poc_receipts_v1) ->
    "poc_receipts_v1";
to_type(blockchain_txn_vars_v1) ->
    "vars_v1";
to_type(blockchain_txn_rewards_v1) ->
    "rewards_v1";
to_type(blockchain_txn_token_burn_v1) ->
    "token_burn_v1";
to_type(blockchain_txn_dc_coinbase_v1) ->
    "dc_coinbase_v1";
to_type(blockchain_txn_token_burn_exchange_rate_v1) ->
    "token_burn_exchange_rate_v1";
to_type(blockchain_txn_payment_v2) ->
    "payment_v2";
to_type(blockchain_txn_state_channel_open_v1) ->
    "state_channel_open_v1";
to_type(blockchain_txn_state_channel_close_v1) ->
    "state_channel_close_v1".

to_json(T, Ledger) ->
    to_json(blockchain_txn:type(T), T, Ledger).

to_json(blockchain_txn_coinbase_v1, T, _Ledger) ->
    #{<<"payee">> => ?BIN_TO_B58(blockchain_txn_coinbase_v1:payee(T)),
      <<"amount">> => blockchain_txn_coinbase_v1:amount(T)};
to_json(blockchain_txn_security_coinbase_v1, T, _Ledger) ->
    #{<<"payee">> => ?BIN_TO_B58(blockchain_txn_security_coinbase_v1:payee(T)),
      <<"amount">> => blockchain_txn_security_coinbase_v1:amount(T)};
to_json(blockchain_txn_oui_v1, T, _Ledger) ->
    #{<<"owner">> => ?BIN_TO_B58(blockchain_txn_oui_v1:owner(T)),
      <<"addresses">> => [?BIN_TO_B58(Addr) || Addr <- blockchain_txn_oui_v1:addresses(T)],
      <<"payer">> => ?BIN_TO_B58(blockchain_txn_oui_v1:payer(T)),
      <<"staking_fee" >> => blockchain_txn_oui_v1:staking_fee(T),
      <<"fee" >> => blockchain_txn_oui_v1:fee(T),
      <<"filter">> => ?MAYBE_B64(blockchain_txn_oui_v1:filter(T)),
      <<"requested_subnet_size">> => blockchain_txn_oui_v1:requested_subnet_size(T),
      <<"owner_signature">> => ?BIN_TO_B64(blockchain_txn_oui_v1:owner_signature(T)),
      <<"payer_signature">> => ?BIN_TO_B64(blockchain_txn_oui_v1:payer_signature(T))};
to_json(blockchain_txn_gen_gateway_v1, T, _Ledger) ->
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_txn_gen_gateway_v1:gateway(T)),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_gen_gateway_v1:owner(T)),
      <<"location">> => ?MAYBE_H3(blockchain_txn_gen_gateway_v1:location(T)),
      <<"nonce">> => blockchain_txn_gen_gateway_v1:nonce(T) };
to_json(blockchain_txn_routing_v1, T, _Ledger) ->

    ActionJson = fun(Action) ->
                         case Action of
                             {update_routers, RouterAddresses} ->
                                 #{<<"action">> => <<"update_routers">>,
                                   <<"addresses">> =>[?BIN_TO_B58(A) || A <- RouterAddresses]};
                             {new_xor, Filter} ->
                                 #{<<"action">> => <<"new_xor">>,
                                   <<"filter">> => ?BIN_TO_B64(Filter)};
                             {update_xor, Index, Filter} ->
                                 #{<<"action">> => <<"update_xor">>,
                                   <<"index">> => Index,
                                   <<"filter">> => ?BIN_TO_B64(Filter)};
                             {request_subnet, SubnetSize} ->
                                 #{<<"action">> => <<"request_subnet">>,
                                   <<"subnet_size">> => SubnetSize}
                         end
                 end,

    #{<<"oui">> => blockchain_txn_routing_v1:oui(T),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_routing_v1:owner(T)),
      <<"fee" >> => blockchain_txn_routing_v1:fee(T),
      <<"action">> => ActionJson(blockchain_txn_routing_v1:action(T)),
      <<"nonce">> => blockchain_txn_routing_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_routing_v1:signature(T)) };
to_json(blockchain_txn_payment_v1, T, _Ledger) ->
    #{<<"payer">> => ?BIN_TO_B58(blockchain_txn_payment_v1:payer(T)),
      <<"payee">> => ?BIN_TO_B58(blockchain_txn_payment_v1:payee(T)),
      <<"amount">> => blockchain_txn_payment_v1:amount(T),
      <<"fee" >> => blockchain_txn_payment_v1:fee(T),
      <<"nonce">> => blockchain_txn_payment_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_payment_v1:signature(T)) };
to_json(blockchain_txn_security_exchange_v1, T, _Ledger) ->
    #{<<"payer">> => ?BIN_TO_B58(blockchain_txn_security_exchange_v1:payer(T)),
      <<"payee">> => ?BIN_TO_B58(blockchain_txn_security_exchange_v1:payee(T)),
      <<"amount">> => blockchain_txn_security_exchange_v1:amount(T),
      <<"fee" >> => blockchain_txn_security_exchange_v1:fee(T),
      <<"nonce">> => blockchain_txn_security_exchange_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_security_exchange_v1:signature(T)) };
to_json(blockchain_txn_consensus_group_v1, T, _Ledger) ->
    #{<<"members">> => [?BIN_TO_B58(M) || M <- blockchain_txn_consensus_group_v1:members(T)],
      <<"proof">> => ?BIN_TO_B64(blockchain_txn_consensus_group_v1:proof(T)),
      <<"height">> => blockchain_txn_consensus_group_v1:height(T),
      <<"delay">> => blockchain_txn_consensus_group_v1:delay(T) };
to_json(blockchain_txn_add_gateway_v1, T, _Ledger) ->
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_txn_add_gateway_v1:gateway(T)),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_add_gateway_v1:owner(T)),
      <<"owner_signature">> => ?BIN_TO_B64(blockchain_txn_add_gateway_v1:owner_signature(T)),
      <<"gateway_signature">> => ?BIN_TO_B64(blockchain_txn_add_gateway_v1:gateway_signature(T)),
      <<"payer">> => ?BIN_TO_B58(blockchain_txn_add_gateway_v1:payer(T)),
      <<"payer_signature">> => ?BIN_TO_B64(blockchain_txn_add_gateway_v1:payer_signature(T)),
      <<"staking_fee">> => blockchain_txn_add_gateway_v1:staking_fee(T),
      <<"fee">> => blockchain_txn_add_gateway_v1:fee(T) };
to_json(blockchain_txn_assert_location_v1, T, _Ledger) ->
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_txn_assert_location_v1:gateway(T)),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_assert_location_v1:owner(T)),
      <<"owner_signature">> => ?BIN_TO_B64(blockchain_txn_assert_location_v1:owner_signature(T)),
      <<"gateway_signature">> => ?BIN_TO_B64(blockchain_txn_assert_location_v1:gateway_signature(T)),
      <<"payer">> => ?BIN_TO_B58(blockchain_txn_assert_location_v1:payer(T)),
      <<"payer_signature">> => ?BIN_TO_B64(blockchain_txn_assert_location_v1:payer_signature(T)),
      <<"location">> => ?MAYBE_H3(blockchain_txn_assert_location_v1:location(T)),
      <<"nonce">> => blockchain_txn_assert_location_v1:nonce(T),
      <<"staking_fee">> => blockchain_txn_assert_location_v1:staking_fee(T),
      <<"fee">> => blockchain_txn_assert_location_v1:fee(T) };
to_json(blockchain_txn_create_htlc_v1, T, _Ledger) ->
    #{<<"payer">> => ?BIN_TO_B58(blockchain_txn_create_htlc_v1:payer(T)),
      <<"payee">> => ?BIN_TO_B58(blockchain_txn_create_htlc_v1:payee(T)),
      <<"address">> => ?BIN_TO_B58(blockchain_txn_create_htlc_v1:address(T)),
      <<"hashlock">> => ?BIN_TO_B64(blockchain_txn_create_htlc_v1:hashlock(T)),
      <<"timelock">> => blockchain_txn_create_htlc_v1:timelock(T),
      <<"amount">> => blockchain_txn_create_htlc_v1:amount(T),
      <<"fee">> => blockchain_txn_create_htlc_v1:fee(T),
      <<"nonce">> => blockchain_txn_create_htlc_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_create_htlc_v1:signature(T)) };
to_json(blockchain_txn_redeem_htlc_v1, T, _Ledger) ->
    #{<<"payee">> => ?BIN_TO_B58(blockchain_txn_redeem_htlc_v1:payee(T)),
      <<"address">> => ?BIN_TO_B58(blockchain_txn_redeem_htlc_v1:address(T)),
      <<"preimage">> => ?BIN_TO_B64(blockchain_txn_redeem_htlc_v1:preimage(T)),
      <<"fee">> => blockchain_txn_redeem_htlc_v1:fee(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_redeem_htlc_v1:signature(T)) };
to_json(blockchain_txn_poc_request_v1, T, Ledger) ->
    Challenger = blockchain_txn_poc_request_v1:challenger(T),
    {ok, ChallengerInfo} = blockchain_ledger_v1:find_gateway_info(Challenger, Ledger),
    ChallengerLoc = blockchain_ledger_gateway_v2:location(ChallengerInfo),
    #{<<"challenger">> => ?BIN_TO_B58(Challenger),
      <<"owner">> => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(ChallengerInfo)),
      <<"location">> => ?MAYBE_H3(ChallengerLoc),
      <<"secret_hash">> => ?BIN_TO_B64(blockchain_txn_poc_request_v1:secret_hash(T)),
      <<"onion_key_hash">> => ?BIN_TO_B64(blockchain_txn_poc_request_v1:onion_key_hash(T)),
      <<"block_hash">> => ?BIN_TO_B64(blockchain_txn_poc_request_v1:block_hash(T)),
      <<"version">> => blockchain_txn_poc_request_v1:version(T),
      <<"fee">> => blockchain_txn_poc_request_v1:fee(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_poc_request_v1:signature(T)) };
to_json(blockchain_poc_receipt_v1, undefined, _Ledger) ->
    null;
to_json(blockchain_poc_receipt_v1, Receipt, _Ledger) ->
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_poc_receipt_v1:gateway(Receipt)),
      <<"timestamp">> => blockchain_poc_receipt_v1:timestamp(Receipt),
      <<"signal">> => blockchain_poc_receipt_v1:signal(Receipt),
      <<"data">> => ?BIN_TO_B64(blockchain_poc_receipt_v1:data(Receipt)),
      <<"origin">> => blockchain_poc_receipt_v1:origin(Receipt),
      <<"signature">> => ?BIN_TO_B64(blockchain_poc_receipt_v1:signature(Receipt)) };
to_json(blockchain_poc_witness_v1, Witness, _Ledger) ->
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_poc_witness_v1:gateway(Witness)),
      <<"timestamp">> => blockchain_poc_witness_v1:timestamp(Witness),
      <<"signal">> => blockchain_poc_witness_v1:signal(Witness),
      <<"packet_hash">> => ?BIN_TO_B64(blockchain_poc_witness_v1:packet_hash(Witness))};
to_json(blockchain_poc_path_element_v1, Elem, Ledger) ->
    #{<<"challengee">> => ?BIN_TO_B58(blockchain_poc_path_element_v1:challengee(Elem)),
      <<"receipt">> => to_json(blockchain_poc_receipt_v1, blockchain_poc_path_element_v1:receipt(Elem), Ledger),
      <<"witnesses">> => [to_json(blockchain_poc_witness_v1, W, Ledger) ||
                             W <- blockchain_poc_path_element_v1:witnesses(Elem)] };
to_json(blockchain_txn_poc_receipts_v1, T, Ledger) ->
    Challenger = blockchain_txn_poc_receipts_v1:challenger(T),
    {ok, ChallengerInfo} = blockchain_ledger_v1:find_gateway_info(Challenger, Ledger),
    ChallengerLoc = blockchain_ledger_gateway_v2:location(ChallengerInfo),
    #{<<"secret">> => ?BIN_TO_B64(blockchain_txn_poc_receipts_v1:secret(T)),
      <<"onion_key_hash">> => ?BIN_TO_B64(blockchain_txn_poc_receipts_v1:onion_key_hash(T)),
      <<"path">> => [to_json(blockchain_poc_path_element_v1, E, Ledger) || E <- blockchain_txn_poc_receipts_v1:path(T)],
      <<"fee">> => blockchain_txn_poc_receipts_v1:fee(T),
      <<"challenger">> => ?BIN_TO_B58(Challenger),
      <<"challenger_owner">> => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(ChallengerInfo)),
      <<"challenger_loc">> => ?MAYBE_H3(ChallengerLoc),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_poc_receipts_v1:signature(T)) };
to_json(blockchain_txn_vars_v1, T, _Ledger) ->
    #{<<"vars">> => blockchain_txn_vars_v1:decoded_vars(T),
      <<"version_predicate">> => blockchain_txn_vars_v1:version_predicate(T),
      <<"proof">> => ?BIN_TO_B64(blockchain_txn_vars_v1:proof(T)),
      <<"master_key">> => ?BIN_TO_B58(blockchain_txn_vars_v1:master_key(T)),
      <<"key_proof">> => ?BIN_TO_B64(blockchain_txn_vars_v1:key_proof(T)),
      <<"cancels">> => blockchain_txn_vars_v1:cancels(T),
      <<"unsets">> => blockchain_txn_vars_v1:unsets(T),
      <<"nonce">> => blockchain_txn_vars_v1:nonce(T) };
to_json(blockchain_txn_rewards_v1, T, _Ledger) ->
    RewardJson = fun(R) ->
                         #{<<"account">> => ?BIN_TO_B58(blockchain_txn_reward_v1:account(R)),
                           <<"gateway">> => ?MAYBE_B58(blockchain_txn_reward_v1:gateway(R)),
                           <<"amount">> => blockchain_txn_reward_v1:amount(R),
                           <<"type">> => blockchain_txn_reward_v1:type(R) }
                 end,
    #{ <<"start_epoch">> => blockchain_txn_rewards_v1:start_epoch(T),
       <<"end_epoch">> => blockchain_txn_rewards_v1:end_epoch(T),
       <<"rewards">> => [RewardJson(R) || R <- blockchain_txn_rewards_v1:rewards(T)] };
to_json(blockchain_txn_token_burn_v1, T, _Ledger) ->
    #{<<"payer">> => ?BIN_TO_B58(blockchain_txn_token_burn_v1:payer(T)),
      <<"amount">> => blockchain_txn_token_burn_v1:amount(T),
      <<"nonce">> => blockchain_txn_token_burn_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_token_burn_v1:signature(T)) };
to_json(blockchain_txn_dc_coinbase_v1, T, _Ledger) ->
    #{ <<"payee">> => ?BIN_TO_B58(blockchain_txn_dc_coinbase_v1:payee(T)),
       <<"amount">> => blockchain_txn_dc_coinbase_v1:amount(T) };
to_json(blockchain_txn_token_burn_exchange_rate_v1, T, _Ledger) ->
    #{<<"rate">> => blockchain_txn_token_burn_exchange_rate_v1:rate(T) };
to_json(blockchain_txn_payment_v2, T, _Ledger) ->

    PaymentJson = fun(P) ->
                          #{<<"payee">> => ?BIN_TO_B58(blockchain_payment_v2:payee(P)),
                            <<"amount">> => blockchain_payment_v2:amount(P) }
                  end,

    #{<<"payer">> => ?BIN_TO_B58(blockchain_txn_payment_v2:payer(T)),
      <<"payments">> => [PaymentJson(Payment) || Payment <- blockchain_txn_payment_v2:payments(T)],
      <<"fee" >> => blockchain_txn_payment_v2:fee(T),
      <<"nonce">> => blockchain_txn_payment_v2:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_payment_v2:signature(T)) };
to_json(blockchain_txn_state_channel_open_v1, T, _Ledger) ->
    #{<<"id">> => ?BIN_TO_B64(blockchain_txn_state_channel_open_v1:id(T)),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_state_channel_open_v1:owner(T)),
      <<"oui">> => blockchain_txn_state_channel_open_v1:oui(T),
      <<"fee" >> => blockchain_txn_state_channel_open_v1:fee(T),
      <<"nonce">> => blockchain_txn_state_channel_open_v1:nonce(T),
      <<"expire_within">> => blockchain_txn_state_channel_open_v1:expire_within(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_state_channel_open_v1:signature(T)) };
to_json(blockchain_txn_state_channel_close_v1, T, _Ledger) ->

    SummaryJson = fun(Summary) ->
                          #{ <<"client">> => ?BIN_TO_B58(blockchain_state_channel_summary_v1:client_pubkeybin(Summary)),
                             <<"num_dcs">> => blockchain_state_channel_summary_v1:num_dcs(Summary),
                             <<"num_packets">> => blockchain_state_channel_summary_v1:num_packets(Summary)
                           }
                  end,

    StateJson = fun(State) ->
                        case State of
                            open -> <<"open">>;
                            closed -> <<"closed">>
                        end
                end,

    SCJson = fun(SC) ->
                     #{<<"id">> => ?BIN_TO_B64(blockchain_state_channel_v1:id(SC)),
                       <<"owner">> => ?BIN_TO_B58(blockchain_state_channel_v1:owner(SC)),
                       <<"nonce">> => blockchain_state_channel_v1:nonce(SC),
                       <<"summaries">> => [SummaryJson(B) || B <- blockchain_state_channel_v1:summaries(SC)],
                       <<"root_hash">> => ?BIN_TO_B64(blockchain_state_channel_v1:root_hash(SC)),
                       <<"state">> => StateJson(blockchain_state_channel_v1:state(SC)),
                       <<"expire_at_block">> => blockchain_state_channel_v1:expire_at_block(SC) }
             end,

    #{<<"closer">> => ?BIN_TO_B58(blockchain_txn_state_channel_close_v1:closer(T)),
      <<"state_channel">> => SCJson(blockchain_txn_state_channel_close_v1:state_channel(T)),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_state_channel_close_v1:signature(T)) }.
