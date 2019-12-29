-module(be_txn).
-export([to_type/1, to_json/1]).

-define (BIN_TO_B58(B), list_to_binary(libp2p_crypto:bin_to_b58((B)))).
-define (BIN_TO_B64(B), list_to_binary(base64:encode_to_string((B)))).

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
    "token_burn_exchange_rate_v1".

to_json(T) ->
    to_json(blockchain_txn:type(T), T).

to_json(blockchain_txn_coinbase_v1, T) ->
    #{<<"payee">> => ?BIN_TO_B58(blockchain_txn_coinbase_v1:payee(T)),
      <<"amount">> => blockchain_txn_coinbase_v1:amount(T)};
to_json(blockchain_txn_security_coinbase_v1, T) ->
    #{<<"payee">> => ?BIN_TO_B58(blockchain_txn_security_coinbase_v1:payee(T)),
      <<"amount">> => blockchain_txn_security_coinbase_v1:amount(T)};
to_json(blockchain_txn_oui_v1, T) ->
    #{<<"owner">> => ?BIN_TO_B58(blockchain_txn_oui_v1:owner(T)),
      <<"addresses">> => [binary_to_list(Addr) || Addr <- blockchain_txn_oui_v1:addresses(T)],
      <<"oui" >> => blockchain_txn_oui_v1:oui(T),
      <<"payer">> => ?BIN_TO_B58(blockchain_txn_oui_v1:payer(T)),
      <<"staking_fee" >> => blockchain_txn_oui_v1:staking_fee(T),
      <<"fee" >> => blockchain_txn_oui_v1:fee(T),
      <<"owner_signature">> => ?BIN_TO_B64(blockchain_txn_oui_v1:owner_signature(T)),
      <<"payer_signature">> => ?BIN_TO_B64(blockchain_txn_oui_v1:payer_signature(T))};
to_json(blockchain_txn_gen_gateway_v1, T) ->
    Location = case blockchain_txn_gen_gateway_v1:location(T) of
                   undefined -> null;
                   L -> list_to_binary(h3:to_string(L))
               end,
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_txn_gen_gateway_v1:gateway(T)),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_gen_gateway_v1:owner(T)),
      <<"location">> => Location,
      <<"nonce">> => blockchain_txn_gen_gateway_v1:nonce(T) };
to_json(blockchain_txn_routing_v1, T) ->
    #{<<"oui">> => blockchain_txn_routing_v1:oui(T),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_routing_v1:owner(T)),
      <<"addresses">> => [binary_to_list(Addr) || Addr <- blockchain_txn_routing_v1:addresses(T)],
      <<"fee" >> => blockchain_txn_routing_v1:fee(T),
      <<"nonce">> => blockchain_txn_routing_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_routing_v1:signature(T)) };
to_json(blockchain_txn_payment_v1, T) ->
    #{<<"payer">> => ?BIN_TO_B58(blockchain_txn_payment_v1:payer(T)),
      <<"payeee">> => ?BIN_TO_B58(blockchain_txn_payment_v1:payee(T)),
      <<"amount">> => blockchain_txn_payment_v1:amount(T),
      <<"fee" >> => blockchain_txn_payment_v1:fee(T),
      <<"nonce">> => blockchain_txn_payment_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_payment_v1:signature(T)) };
to_json(blockchain_txn_security_exchange_v1, T) ->
    #{<<"payer">> => ?BIN_TO_B58(blockchain_txn_security_exchange_v1:payer(T)),
      <<"payeee">> => ?BIN_TO_B58(blockchain_txn_security_exchange_v1:payee(T)),
      <<"amount">> => blockchain_txn_security_exchange_v1:amount(T),
      <<"fee" >> => blockchain_txn_security_exchange_v1:fee(T),
      <<"nonce">> => blockchain_txn_security_exchange_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_security_exchange_v1:signature(T)) };
to_json(blockchain_txn_consensus_group_v1, T) ->
    #{<<"members">> => [?BIN_TO_B58(M) || M <- blockchain_txn_consensus_group_v1:members(T)],
      <<"proof">> => ?BIN_TO_B64(blockchain_txn_consensus_group_v1:proof(T)),
      <<"height">> => blockchain_txn_consensus_group_v1:height(T),
      <<"delay">> => blockchain_txn_consensus_group_v1:delay(T) };
to_json(blockchain_txn_add_gateway_v1, T) ->
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_txn_add_gateway_v1:gateway(T)),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_add_gateway_v1:owner(T)),
      <<"owner_signature">> => ?BIN_TO_B64(blockchain_txn_add_gateway_v1:owner_signature(T)),
      <<"gateway_signature">> => ?BIN_TO_B64(blockchain_txn_add_gateway_v1:gateway_signature(T)),
      <<"payer">> => ?BIN_TO_B58(blockchain_txn_add_gateway_v1:payer(T)),
      <<"payer_signature">> => ?BIN_TO_B64(blockchain_txn_add_gateway_v1:payer_signature(T)),
      <<"staking_fee">> => blockchain_txn_add_gateway_v1:staking_fee(T),
      <<"fee">> => blockchain_txn_add_gateway_v1:fee(T) };
to_json(blockchain_txn_assert_location_v1, T) ->
    Location = case blockchain_txn_assert_location_v1:location(T) of
                   undefined -> null;
                   L -> list_to_binary(h3:to_string(L))
               end,
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_txn_assert_location_v1:gateway(T)),
      <<"owner">> => ?BIN_TO_B58(blockchain_txn_assert_location_v1:owner(T)),
      <<"owner_signature">> => ?BIN_TO_B64(blockchain_txn_assert_location_v1:owner_signature(T)),
      <<"gateway_signature">> => ?BIN_TO_B64(blockchain_txn_assert_location_v1:gateway_signature(T)),
      <<"payer">> => ?BIN_TO_B58(blockchain_txn_assert_location_v1:payer(T)),
      <<"payer_signature">> => ?BIN_TO_B64(blockchain_txn_assert_location_v1:payer_signature(T)),
      <<"location">> => Location,
      <<"nonce">> => blockchain_txn_assert_location_v1:nonce(T),
      <<"staking_fee">> => blockchain_txn_assert_location_v1:staking_fee(T),
      <<"fee">> => blockchain_txn_assert_location_v1:fee(T) };
to_json(blockchain_txn_create_htlc_v1, T) ->
    #{<<"payer">> => ?BIN_TO_B58(blockchain_txn_create_htlc_v1:payer(T)),
      <<"payee">> => ?BIN_TO_B58(blockchain_txn_create_htlc_v1:payee(T)),
      <<"address">> => ?BIN_TO_B58(blockchain_txn_create_htlc_v1:address(T)),
      <<"hashlock">> => ?BIN_TO_B64(blockchain_txn_create_htlc_v1:hashlock(T)),
      <<"timelock">> => blockchain_txn_create_htlc_v1:timelock(T),
      <<"amount">> => blockchain_txn_create_htlc_v1:amount(T),
      <<"fee">> => blockchain_txn_create_htlc_v1:fee(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_create_htlc_v1:signature(T)) };
to_json(blockchain_txn_redeem_htlc_v1, T) ->
    #{<<"payee">> => ?BIN_TO_B58(blockchain_txn_redeem_htlc_v1:payee(T)),
      <<"address">> => ?BIN_TO_B58(blockchain_txn_redeem_htlc_v1:address(T)),
      <<"preimage">> => ?BIN_TO_B64(blockchain_txn_redeem_htlc_v1:preimage(T)),
      <<"fee">> => blockchain_txn_redeem_htlc_v1:fee(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_redeem_htlc_v1:signature(T)) };
to_json(blockchain_txn_poc_request_v1, T) ->
    #{<<"challenger">> => ?BIN_TO_B58(blockchain_txn_poc_request_v1:challenger(T)),
      <<"secret_hash">> => ?BIN_TO_B64(blockchain_txn_poc_request_v1:secret_hash(T)),
      <<"onion_key_hash">> => ?BIN_TO_B64(blockchain_txn_poc_request_v1:onion_key_hash(T)),
      <<"block_hash">> => ?BIN_TO_B64(blockchain_txn_poc_request_v1:block_hash(T)),
      <<"version">> => blockchain_txn_poc_request_v1:version(T),
      <<"fee">> => blockchain_txn_poc_request_v1:fee(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_poc_request_v1:signature(T)) };
to_json(blockchain_poc_receipt_v1, undefined) ->
    null;
to_json(blockchain_poc_receipt_v1, Receipt) ->
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_poc_receipt_v1:gateway(Receipt)),
      <<"timestamp">> => blockchain_poc_receipt_v1:timestamp(Receipt),
      <<"signal">> => blockchain_poc_receipt_v1:signal(Receipt),
      <<"data">> => ?BIN_TO_B64(blockchain_poc_receipt_v1:data(Receipt)),
      <<"origin">> => blockchain_poc_receipt_v1:origin(Receipt),
      <<"signature">> => ?BIN_TO_B64(blockchain_poc_receipt_v1:signature(Receipt)) };
to_json(blockchain_poc_witness_v1, Witness) ->
    #{<<"gateway">> => ?BIN_TO_B58(blockchain_poc_witness_v1:gateway(Witness)),
      <<"timestamp">> => blockchain_poc_witness_v1:timestamp(Witness),
      <<"signal">> => blockchain_poc_witness_v1:signal(Witness),
      <<"packet_hash">> => ?BIN_TO_B64(blockchain_poc_witness_v1:packet_hash(Witness))};
to_json(blockchain_poc_path_element_v1, Elem) ->
    #{<<"challengee">> => ?BIN_TO_B58(blockchain_poc_path_element_v1:challengee(Elem)),
      <<"receipt">> => to_json(blockchain_poc_receipt_v1, blockchain_poc_path_element_v1:receipt(Elem)),
      <<"witnesses">> => [to_json(blockchain_poc_witness_v1, W) ||
                             W <- blockchain_poc_path_element_v1:witnesses(Elem)] };
to_json(blockchain_txn_poc_receipts_v1, T) ->
    #{<<"secret">> => ?BIN_TO_B64(blockchain_txn_poc_receipts_v1:secret(T)),
      <<"onion_key_hash">> => ?BIN_TO_B64(blockchain_txn_poc_receipts_v1:onion_key_hash(T)),
      <<"path">> => [to_json(blockchain_poc_path_element_v1, E) || E <- blockchain_txn_poc_receipts_v1:path(T)],
      <<"fee">> => blockchain_txn_poc_receipts_v1:fee(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_poc_receipts_v1:signature(T)) };
to_json(blockchain_txn_vars_v1, T) ->
    #{<<"vars">> => blockchain_txn_vars_v1:decoded_vars(T),
      <<"version_predicate">> => blockchain_txn_vars_v1:version_predicate(T),
      <<"proof">> => ?BIN_TO_B64(blockchain_txn_vars_v1:proof(T)),
      <<"master_key">> => ?BIN_TO_B58(blockchain_txn_vars_v1:master_key(T)),
      <<"key_proof">> => ?BIN_TO_B64(blockchain_txn_vars_v1:key_proof(T)),
      <<"cancels">> => blockchain_txn_vars_v1:cancels(T),
      <<"unsets">> => blockchain_txn_vars_v1:unsets(T),
      <<"nonce">> => blockchain_txn_vars_v1:nonce(T) };
to_json(blockchain_txn_rewards_v1, T) ->
    RewardJson = fun(R) ->
                         Gateway = case blockchain_txn_reward_v1:gateway(R) of
                                       undefined -> null;
                                       G -> ?BIN_TO_B58(G)
                                   end,
                         #{<<"account">> => ?BIN_TO_B58(blockchain_txn_reward_v1:account(R)),
                           <<"gateway">> => Gateway,
                           <<"amount">> => blockchain_txn_reward_v1:amount(R),
                           <<"type">> => blockchain_txn_reward_v1:type(R) }
                 end,
    #{ <<"start_epoch">> => blockchain_txn_rewards_v1:start_epoch(T),
       <<"end_epoch">> => blockchain_txn_rewards_v1:end_epoch(T),
       <<"rewards">> => [RewardJson(R) || R <- blockchain_txn_rewards_v1:rewards(T)] };
to_json(blockchain_txn_token_burn_v1, T) ->
    Key = case blockchain_txn_token_burn_v1:key(T) of
              undefined -> null;
              K -> ?BIN_TO_B58(K)
          end,
    #{<<"type">> => blockchain_txn_token_burn_v1:type(T),
      <<"payer">> => ?BIN_TO_B58(blockchain_txn_token_burn_v1:payer(T)),
      <<"key">> => Key,
      <<"amount">> => blockchain_txn_token_burn_v1:amount(T),
      <<"nonce">> => blockchain_txn_token_burn_v1:nonce(T),
      <<"signature">> => ?BIN_TO_B64(blockchain_txn_token_burn_v1:signature(T)) };
to_json(blockchain_txn_dc_coinbase_v1, T) ->
    #{ <<"payee">> => ?BIN_TO_B58(blockchain_txn_dc_coinbase_v1:payee(T)),
       <<"amount">> => blockchain_txn_dc_coinbase_v1:amount(T) };
to_json(blockchain_txn_token_burn_exchange_rate_v1, T) ->
    #{<<"rate">> => blockchain_txn_token_burn_exchange_rate_v1:rate(T) }.
