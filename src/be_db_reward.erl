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
            blockchain_txn:type(T) == blockchain_txn_rewards_v1
        end,
        blockchain_block_v1:transactions(Block)
    ),
    Queries = lists:foldl(
        fun(T, TAcc) ->
            TxnHash = ?BIN_TO_B64(blockchain_txn:hash(T)),
            RewardMap = collect_rewards(blockchain_txn_rewards_v1:rewards(T), #{}),
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
    {Account :: libp2p_crypto:pubkey_bin(), Gateway :: libp2p_crypto:pubkey_bin()} =>
        Reward :: pos_integer()
}.

-spec collect_rewards(blockchain_txn_reward_v1:rewards(), reward_map()) -> reward_map().
collect_rewards([], RewardMap) ->
    RewardMap;
collect_rewards([Reward | Rest], Map) ->
    Key = {blockchain_txn_reward_v1:account(Reward), blockchain_txn_reward_v1:gateway(Reward)},
    Amount = maps:get(Key, Map, 0),
    collect_rewards(Rest, maps:put(Key, Amount + blockchain_txn_reward_v1:amount(Reward), Map)).

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
