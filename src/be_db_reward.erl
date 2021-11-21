-module(be_db_reward).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).
%% api
-export([calculate_rewards_metadata/3]).

-behavior(be_db_worker).
-behavior(be_db_follower).

-record(state, {}).

-define(S_INSERT_REWARD, "insert_reward").
-define(S_INSERT_REWARD_10, "insert_reward_10").
-define(S_INSERT_REWARD_100, "insert_reward_100").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    MkQueryFun = fun(Rows) ->
        epgsql:parse(
            Conn,
            ?S_INSERT_REWARD ++ "_" ++ integer_to_list(Rows),
            [
                "insert into rewards (block, transaction_hash, time, account, gateway, amount, type) ",
                "values  ",
                be_utils:make_values_list(7, Rows)
            ],
            []
        )
    end,
    {ok, S1} = MkQueryFun(1),
    {ok, S10} = MkQueryFun(10),
    {ok, S100} = MkQueryFun(100),
    #{
        ?S_INSERT_REWARD => S1,
        ?S_INSERT_REWARD_10 => S10,
        ?S_INSERT_REWARD_100 => S100
    }.

%%
%% be_block_handler
%%

init(_) ->
    ets:new(?MODULE, [public, named_table]),
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

    StartMkQuery = erlang:monotonic_time(millisecond),
    Queries = lists:flatmap(
        fun(T) ->
            TxnHash = ?BIN_TO_B64(blockchain_txn:hash(T)),
            RewardMap = collect_rewards(
                blockchain_txn:type(T),
                blockchain_worker:blockchain(),
                T,
                #{}
            ),
            lists:map(
                fun(Entry) ->
                    q_insert_reward(
                        BlockHeight,
                        TxnHash,
                        BlockTime,
                        Entry
                    )
                end,
                maps:to_list(RewardMap)
            )
        end,
        Txns
    ),
    be_db_follower:maybe_log_duration(db_reward_query_make, StartMkQuery),

    StartQuery = erlang:monotonic_time(millisecond),
    execute_queries(Conn, Queries),
    be_db_follower:maybe_log_duration(db_reward_query_exec, StartQuery),

    {ok, State}.

execute_queries(Conn, Queries) when length(Queries) > 100 ->
    lists:foreach(
        fun
            (Q) when length(Q) == 100 ->
                {ok, 100} = ?PREPARED_QUERY(Conn, ?S_INSERT_REWARD_100, lists:flatten(Q));
            (Q) ->
                execute_queries(Conn, Q)
        end,
        be_utils:split_list(Queries, 100)
    );
execute_queries(Conn, Queries) when length(Queries) > 10 ->
    lists:foreach(
        fun
            (Q) when length(Q) == 10 ->
                {ok, 10} = ?PREPARED_QUERY(Conn, ?S_INSERT_REWARD_10, lists:flatten(Q));
            (Q) ->
                execute_queries(Conn, Q)
        end,
        be_utils:split_list(Queries, 10)
    );
execute_queries(Conn, Queries) ->
    ok = ?BATCH_QUERY(Conn, [{?S_INSERT_REWARD, I} || I <- Queries]).

-type reward_type() ::
    poc_challenger
    | poc_challengee
    | poc_witness
    | dc_rewards
    | consensus_rewards
    | securities_reward.

-type reward_map() :: #{
    {
        Account :: libp2p_crypto:pubkey_bin(),
        Gateway :: libp2p_crypto:pubkey_bin() | undefined,
        Type :: reward_type()
    } =>
        Reward :: pos_integer()
}.

calculate_rewards_metadata(Start, End, Chain) ->
    % Check if someone else already constructed the metadata for the given start
    % block.
    case ets:lookup(?MODULE, metadata) of
        [{metadata, Start, Metadata}] ->
            {ok, Metadata};
        _ ->
            % Otherwise delete the cache entry and construct a new one
            ets:delete(?MODULE, metadata),
            StartTime = erlang:monotonic_time(millisecond),
            {ok, Metadata} = blockchain_txn_rewards_v2:calculate_rewards_metadata(
                Start,
                End,
                Chain
            ),
            EndTime = erlang:monotonic_time(millisecond),
            lager:info("Calculated rewards metadata took: ~p ms", [EndTime - StartTime]),
            ets:insert(?MODULE, {metadata, Start, Metadata}),
            {ok, Metadata}
    end.

collect_rewards(blockchain_txn_rewards_v1, _Chain, Txn, RewardMap) ->
    collect_v1_rewards(blockchain_txn_rewards_v1:rewards(Txn), RewardMap);
collect_rewards(blockchain_txn_rewards_v2, Chain, Txn, RewardMap) ->
    Start = blockchain_txn_rewards_v2:start_epoch(Txn),
    End = blockchain_txn_rewards_v2:end_epoch(Txn),
    {ok, Ledger} = blockchain:ledger_at(End, Chain),
    {ok, Metadata} = ?MODULE:calculate_rewards_metadata(
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
            ({owner, Type, O}, Amt, Acc) ->
                maps:update_with(
                    {O, <<>>, Type},
                    fun(Balance) -> Balance + Amt end,
                    Amt,
                    Acc
                );
            ({gateway, Type, G}, Amt, Acc) ->
                case blockchain_ledger_v1:find_gateway_owner(G, Ledger) of
                    {error, _Error} ->
                        Acc;
                    {ok, GwOwner} ->
                        maps:update_with(
                            {GwOwner, G, Type},
                            fun(Balance) -> Balance + Amt end,
                            Amt,
                            Acc
                        )
                end;
            ({validator, Type, V}, Amt, Acc) ->
                case blockchain_ledger_v1:get_validator(V, Ledger) of
                    {error, _Error} ->
                        Acc;
                    {ok, Validator} ->
                        maps:update_with(
                            {blockchain_ledger_validator_v1:owner_address(Validator), V, Type},
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
    Key = {
        blockchain_txn_reward_v1:account(Reward),
        blockchain_txn_reward_v1:gateway(Reward),
        blockchain_txn_reward_v1:type(Reward)
    },
    Amount = blockchain_txn_reward_v1:amount(Reward),
    collect_v1_rewards(
        Rest,
        maps:update_with(Key, fun(Balance) -> Balance + Amount end, Amount, RewardMap)
    ).

%% ranslate from a chain reward type to a database reward. As of this writing the
%% set of names for the database reward_type matches the rewards_v2 rewards types.
-spec to_reward_type(atom()) -> reward_type().
to_reward_type(securities) -> securities_reward;
to_reward_type(data_credits) -> dc_rewards;
to_reward_type(poc_challengees) -> poc_challengee;
to_reward_type(poc_challengers) -> poc_challenger;
to_reward_type(poc_witnesses) -> poc_witness;
to_reward_type(consensus) -> consensus_rewards.

q_insert_reward(BlockHeight, TxnHash, BlockTime, {{Account, Gateway, RewardType}, Amount}) ->
    [
        BlockHeight,
        TxnHash,
        BlockTime,
        ?BIN_TO_B58(Account),
        ?BIN_TO_B58(Gateway),
        Amount,
        to_reward_type(RewardType)
    ].
