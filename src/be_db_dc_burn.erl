-module(be_db_dc_burn).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).
%% api
-export([collect_burns/3]).

-behavior(be_db_worker).
-behavior(be_db_follower).

-record(state, {}).

-define(S_INSERT_BURN, "insert_burn").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} = epgsql:parse(
        Conn,
        ?S_INSERT_BURN,
        [
            "insert into dc_burns (block, time, transaction_hash, actor, type, amount, oracle_price) ",
            "values ($1, $2, $3, $4, $5, $6, $7) "
            "on conflict do nothing"
        ],
        []
    ),
    #{?S_INSERT_BURN => S1}.

%%
%% be_block_handler
%%

init(_) ->
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State = #state{}) ->
    {ok, OraclePrice} = blockchain_ledger_v1:current_oracle_price(Ledger),
    Queries = lists:map(
        fun q_insert_burn/1,
        collect_burns(Block, OraclePrice, Ledger)
    ),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State}.

collect_burns(Block, OraclePrice, Ledger) ->
    Height = blockchain_block_v1:height(Block),
    BlockTime = blockchain_block_v1:time(Block),
    lists:foldl(
        fun(T, Acc) ->
            TxnHash = ?BIN_TO_B64(blockchain_txn:hash(T)),
            lists:foldl(
                fun({Type, Actor, Amount}, TAcc) ->
                    [
                        {Height, BlockTime, TxnHash, ?BIN_TO_B58(Actor), Type, Amount, OraclePrice}
                        | TAcc
                    ]
                end,
                Acc,
                collect_burns(blockchain_txn:type(T), T, Ledger, [])
            )
        end,
        [],
        blockchain_block_v1:transactions(Block)
    ).

%% Collect burn_type, actor and amount data for each
collect_burns(blockchain_txn_oui_v1, Txn, Ledger, Acc) ->
    collect_fee(Txn, Ledger, [
        {oui, blockchain_txn_oui_v1:fee_payer(Txn, Ledger), blockchain_txn_oui_v1:staking_fee(Txn)}
        | Acc
    ]);
collect_burns(blockchain_txn_routing_v1, Txn, Ledger, Acc) ->
    collect_fee(Txn, Ledger, [
        {routing, blockchain_txn_routing_v1:owner(Txn), blockchain_txn_routing_v1:staking_fee(Txn)}
        | Acc
    ]);
collect_burns(blockchain_txn_add_gateway_v1, Txn, Ledger, Acc) ->
    collect_fee(Txn, Ledger, [
        {add_gateway, blockchain_txn_add_gateway_v1:fee_payer(Txn, Ledger),
            blockchain_txn_add_gateway_v1:staking_fee(Txn)}
        | Acc
    ]);
collect_burns(blockchain_txn_assert_location_v1, Txn, Ledger, Acc) ->
    collect_fee(Txn, Ledger, [
        {assert_location, blockchain_txn_assert_location_v1:fee_payer(Txn, Ledger),
            blockchain_txn_assert_location_v1:staking_fee(Txn)}
        | Acc
    ]);
collect_burns(blockchain_txn_assert_location_v2, Txn, Ledger, Acc) ->
    collect_fee(Txn, Ledger, [
        {assert_location, blockchain_txn_assert_location_v2:fee_payer(Txn, Ledger),
            blockchain_txn_assert_location_v2:staking_fee(Txn)}
        | Acc
    ]);
collect_burns(blockchain_txn_state_channel_close_v1, Txn, Ledger, Acc) ->
    PacketMap = be_db_packet:collect_packets(
        blockchain_state_channel_v1:summaries(
            blockchain_txn_state_channel_close_v1:state_channel(Txn)
        ),
        #{}
    ),
    Burns = maps:fold(
        fun(Gateway, {_NumPackets, NumDCs}, Acc1) ->
            [{state_channel, Gateway, NumDCs} | Acc1]
        end,
        Acc,
        maps:iterator(PacketMap)
    ),
    collect_fee(Txn, Ledger, Burns);
collect_burns(_, Txn, Ledger, Acc) ->
    collect_fee(Txn, Ledger, Acc).

collect_fee(Txn, Ledger, Acc) ->
    case {blockchain_txn:fee_payer(Txn, Ledger), blockchain_txn:fee(Txn)} of
        {undefined, _} -> Acc;
        {_, 0} -> Acc;
        {Actor, Fee} -> [{fee, Actor, Fee} | Acc]
    end.

q_insert_burn({BlockHeight, BlockTime, TxnHash, Actor, Type, Amount, OraclePrice}) ->
    Params = [
        BlockHeight,
        BlockTime,
        TxnHash,
        Actor,
        Type,
        Amount,
        OraclePrice
    ],
    {?S_INSERT_BURN, Params}.
