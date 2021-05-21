-module(be_db_reward).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-behavior(be_db_worker).
-behavior(be_db_follower).

-record(state, {}).

-define(S_INSERT_REWARD, "insert_reward").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} = epgsql:parse(
        Conn,
        ?S_INSERT_REWARD,
        [
            "insert into rewards (block, transaction_hash, time, account, gateway, amount) ",
            "values ($1, $2, $3, $4, $5, $6) "
        ],
        []
    ),
    #{?S_INSERT_REWARD => S1}.

%%
%% be_block_handler
%%

init(_) ->
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, _Ledger, State = #state{}) ->
    BlockHeight = blockchain_block_v1:height(Block),
    BlockTime = blockchain_block_v1:time(Block),
    Txns = lists:filter(
        fun(T) ->
            blockchain_txn:type(T) == blockchain_txn_rewards_v1 orelse
                blockchain_txn:type(T) == blockchain_txn_rewards_v2
        end,
        blockchain_block_v1:transactions(Block)
    ),
    Queries = lists:foldl(
        fun(T, TAcc) ->
            TxnHash = ?BIN_TO_B64(blockchain_txn:hash(T)),
            RewardMap = collect_rewards(
                blockchain_txn:type(T),
                blockchain_worker:blockchain(),
                T,
                #{}
            ),
            lists:foldl(
                fun(Entry, RAcc) ->
                    q_insert_reward(
                        BlockHeight,
                        TxnHash,
                        BlockTime,
                        Entry,
                        RAcc
                    )
                end,
                TAcc,
                maps:to_list(RewardMap)
            )
        end,
        [],
        Txns
    ),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State}.

-type reward_map() :: #{
    {Account :: libp2p_crypto:pubkey_bin(), Gateway :: libp2p_crypto:pubkey_bin() | undefined} =>
        Reward :: pos_integer()
}.

collect_rewards(blockchain_txn_rewards_v1, _Chain, Txn, RewardMap) ->
    collect_v1_rewards(blockchain_txn_rewards_v1:rewards(Txn), RewardMap);
collect_rewards(blockchain_txn_rewards_v2, Chain, Txn, RewardMap) ->
    Start = blockchain_txn_rewards_v2:start_epoch(Txn),
    End = blockchain_txn_rewards_v2:end_epoch(Txn),
    {ok, Ledger} = blockchain:ledger_at(End, Chain),
    {ok, Metadata} = blockchain_txn_rewards_v2:calculate_rewards_metadata(
        Start,
        End,
        Chain
    ),
    maps:fold(
        fun
            (overages, _Amount, Acc) ->
                Acc;
            (_RewardCategory, Rewards, Acc) ->
                collect_v2_rewards(Rewards, Ledger, Acc)
        end,
        RewardMap,
        Metadata
    ).

collect_v2_rewards(Rewards, Ledger, RewardMap) ->
    maps:fold(
        fun
            ({owner, _Type, O}, Amt, Acc) ->
                maps:update_with(
                    {O, <<>>},
                    fun(Balance) -> Balance + Amt end,
                    Amt,
                    Acc
                );
            ({gateway, _Type, G}, Amt, Acc) ->
                case blockchain_ledger_v1:find_gateway_owner(G, Ledger) of
                    {error, _Error} ->
                        Acc;
                    {ok, GwOwner} ->
                        maps:update_with(
                            {GwOwner, G},
                            fun(Balance) -> Balance + Amt end,
                            Amt,
                            Acc
                        )
                end;
            ({validator, _Type, V}, Amt, Acc) ->
                case blockchain_ledger_v1:get_validator(V, Ledger) of
                    {error, _Error} ->
                        Acc;
                    {ok, Validator} ->
                        maps:update_with(
                            {blockchain_ledger_validator_v1:owner_address(Validator), V},
                            fun(Balance) -> Balance + Amt end,
                            Amt,
                            Acc
                        )
                end
        end,
        RewardMap,
        maps:iterator(Rewards)
    ).

-spec collect_v1_rewards(blockchain_txn_reward_v1:rewards(), reward_map()) -> reward_map().
collect_v1_rewards([], RewardMap) ->
    RewardMap;
collect_v1_rewards([Reward | Rest], RewardMap) ->
    Key = {blockchain_txn_reward_v1:account(Reward), blockchain_txn_reward_v1:gateway(Reward)},
    Amount = blockchain_txn_reward_v1:amount(Reward),
    collect_v1_rewards(
        Rest,
        maps:update_with(Key, fun(Balance) -> Balance + Amount end, Amount, RewardMap)
    ).

q_insert_reward(BlockHeight, TxnHash, BlockTime, {{Account, Gateway}, Amount}, Queries) ->
    Params = [
        BlockHeight,
        TxnHash,
        BlockTime,
        ?BIN_TO_B58(Account),
        ?BIN_TO_B58(Gateway),
        Amount
    ],
    [{?S_INSERT_REWARD, Params} | Queries].
