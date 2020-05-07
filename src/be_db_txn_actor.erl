-module(be_db_txn_actor).

-include("be_follower.hrl").
-include("be_db_worker.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/0, load/6, to_actors/1]).

-define(S_INSERT_ACTOR, "insert_actor").

-record(state, {}).

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(Conn, ?S_INSERT_ACTOR,
                     ["insert into transaction_actors (block, actor, actor_role, transaction_hash) ",
                      "values ($1, $2, $3, $4)"],
                     []),

    #{
      ?S_INSERT_ACTOR => S1
     }.

%%
%% be_block_handler
%%

init() ->
    {ok, #state{}}.

load(Conn, _Hash, Block, _Sync, _Ledger, State=#state{}) ->
    Queries = q_insert_transaction_actors(Block, [], State),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State}.

q_insert_transaction_actors(Block, Query, #state{}) ->
    Height = blockchain_block_v1:height(Block),
    Txns = blockchain_block_v1:transactions(Block),
    lists:foldl(fun(T, Acc) ->
                        TxnHash = ?BIN_TO_B64(blockchain_txn:hash(T)),
                        lists:foldl(fun({Role, Key}, ActorAcc) ->
                                            [{?S_INSERT_ACTOR,
                                              [Height, ?BIN_TO_B58(Key), Role, TxnHash]} | ActorAcc]
                                    end, Acc, to_actors(T))
                end, Query, Txns).


to_actors(T) ->
    to_actors(blockchain_txn:type(T), T).

to_actors(blockchain_txn_coinbase_v1, T) ->
    [{"payee", blockchain_txn_coinbase_v1:payee(T)}];
to_actors(blockchain_txn_security_coinbase_v1, T) ->
    [{"payee", blockchain_txn_security_coinbase_v1:payee(T)}];
to_actors(blockchain_txn_oui_v1, T) ->
    [{"owner", blockchain_txn_oui_v1:owner(T)},
     {"payer", blockchain_txn_oui_v1:payer(T)} ];
to_actors(blockchain_txn_gen_gateway_v1, T) ->
    [{"gateway", blockchain_txn_gen_gateway_v1:gateway(T)},
     {"owner", blockchain_txn_gen_gateway_v1:owner(T)} ];
to_actors(blockchain_txn_routing_v1, T) ->
    [{"owner", blockchain_txn_routing_v1:owner(T)}];
to_actors(blockchain_txn_payment_v1, T) ->
    [{"payer", blockchain_txn_payment_v1:payer(T)},
     {"payee", blockchain_txn_payment_v1:payee(T)} ];
to_actors(blockchain_txn_security_exchange_v1, T) ->
    [{"payer", blockchain_txn_security_exchange_v1:payer(T)},
     {"payee", blockchain_txn_security_exchange_v1:payee(T)} ];
to_actors(blockchain_txn_consensus_group_v1, T) ->
    [{"consensus_member", M} || M <- blockchain_txn_consensus_group_v1:members(T)];
to_actors(blockchain_txn_add_gateway_v1, T) ->
    [{"gateway", blockchain_txn_add_gateway_v1:gateway(T)},
     {"owner", blockchain_txn_add_gateway_v1:owner(T)},
     {"payer", blockchain_txn_add_gateway_v1:payer(T)} ];
to_actors(blockchain_txn_assert_location_v1, T) ->
    [{"gateway", blockchain_txn_assert_location_v1:gateway(T)},
     {"owner", blockchain_txn_assert_location_v1:owner(T)},
     {"payer", blockchain_txn_assert_location_v1:payer(T)} ];
to_actors(blockchain_txn_create_htlc_v1, T) ->
    [{"payer", blockchain_txn_create_htlc_v1:payer(T)},
     {"payee", blockchain_txn_create_htlc_v1:payee(T)},
     {"escrow", blockchain_txn_create_htlc_v1:address(T)} ];
to_actors(blockchain_txn_redeem_htlc_v1, T) ->
    [{"payee", blockchain_txn_redeem_htlc_v1:payee(T)},
     {"escrow", blockchain_txn_redeem_htlc_v1:address(T)} ];
to_actors(blockchain_txn_poc_request_v1, T) ->
    [{"challenger", blockchain_txn_poc_request_v1:challenger(T)} ];
to_actors(blockchain_txn_poc_receipts_v1, T) ->
    ToActors = fun(undefined, Acc0) ->
                       Acc0;
                   (Elem, {ChallengeeAcc0, WitnessAcc0}) ->
                       ChallengeeAcc =
                           [{"challengee",
                             blockchain_poc_path_element_v1:challengee(Elem)} | ChallengeeAcc0],
                       WitnessAcc = lists:foldl(fun(W, WAcc) ->
                                                        [{"witness",
                                                          blockchain_poc_witness_v1:gateway(W)} | WAcc]
                                                end, WitnessAcc0, blockchain_poc_path_element_v1:witnesses(Elem)),
                       {ChallengeeAcc, WitnessAcc}
                   end,
    {Challengees, Witnesses} = lists:foldl(ToActors, {[], []}, blockchain_txn_poc_receipts_v1:path(T)),
    lists:usort(Challengees) ++ lists:usort(Witnesses);
to_actors(blockchain_txn_vars_v1, _T) ->
   [];
to_actors(blockchain_txn_rewards_v1, T) ->
    ToActors = fun(R, {PayeeAcc0, GatewayAcc0}) ->
                       PayeeAcc = [{"payee", blockchain_txn_reward_v1:account(R)} | PayeeAcc0],
                       GatewayAcc = case blockchain_txn_reward_v1:gateway(R) of
                                        undefined -> GatewayAcc0;
                                        G -> [{"reward_gateway", G} | GatewayAcc0]
                                    end,
                       {PayeeAcc, GatewayAcc}
               end,
    {Payees, Gateways} = lists:foldl(ToActors, {[], []}, blockchain_txn_rewards_v1:rewards(T)),
    lists:usort(Payees) ++ lists:usort(Gateways);
to_actors(blockchain_txn_token_burn_v1, T) ->
    [{"payer", blockchain_txn_token_burn_v1:payer(T)} ];
to_actors(blockchain_txn_dc_coinbase_v1, T) ->
    [{"payee", blockchain_txn_dc_coinbase_v1:payee(T)} ];
to_actors(blockchain_txn_token_burn_exchange_rate_v1, _T) ->
    [];
to_actors(blockchain_txn_payment_v2, T) ->
    ToActors = fun(Payment, Acc) ->
                       [{"payee", blockchain_payment_v2:payee(Payment)} | Acc]
               end,
    lists:foldl(ToActors, [{"payer", blockchain_txn_payment_v2:payer(T)}],
                blockchain_txn_payment_v2:payments(T));
to_actors(blockchain_txn_state_channel_open_v1, T) ->
    %% NOTE: owner => sc_opener, renamed for better reasoning
    [{"sc_opener", blockchain_txn_state_channel_open_v1:owner(T)} ];
to_actors(blockchain_txn_state_channel_close_v1, T) ->
    %% NOTE: closer can be one of the clients of the state channel or the owner of the router
    %% if the state_channel expires
    [{"sc_closer", blockchain_txn_state_channel_close_v1:closer(T)} ].
