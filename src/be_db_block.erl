-module(be_db_block).

-behavior(be_db_worker).
-behavior(be_db_follower).

-include("be_follower.hrl").
-include("be_db_worker.hrl").
-include_lib("stdlib/include/assert.hrl").

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/0, load/6]).

-define(S_BLOCK_HEIGHT, "block_height").
-define(S_INSERT_BLOCK, "insert_block").
-define(S_INSERT_BLOCK_SIG, "insert_block_signature").
-define(S_INSERT_TXN, "insert_transaction").

-record(state,
       {
        height :: non_neg_integer(),

        base_secs=calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) :: pos_integer()
       }).


%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S0} =
        epgsql:parse(Conn, ?S_BLOCK_HEIGHT,
                     "select max(height) from blocks",
                     []),

    {ok, S1} =
        epgsql:parse(Conn, ?S_INSERT_BLOCK,
                     "insert into blocks (height, time, timestamp, prev_hash, block_hash, transaction_count, hbbft_round, election_epoch, epoch_start, rescue_signature) values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);",
                     []),
    {ok, S2} =
        epgsql:parse(Conn, ?S_INSERT_BLOCK_SIG,
                     "insert into block_signatures (block, signer, signature) values ($1, $2, $3)",
                     []),
    {ok, S3} =
        epgsql:parse(Conn, ?S_INSERT_TXN,
                     "insert into transactions (block, time, hash, type, fields) values ($1, $2, $3, $4, $5)",
                     []),

    #{
      ?S_BLOCK_HEIGHT => S0,
      ?S_INSERT_BLOCK => S1,
      ?S_INSERT_BLOCK_SIG => S2,
      ?S_INSERT_TXN => S3
     }.

%%
%% be_block_handler
%%

init() ->
    {ok, _, [{Value}]} = ?PREPARED_QUERY(?S_BLOCK_HEIGHT, []),
    Height = case Value of
                 null -> 0;
                 _ -> Value
             end,
    lager:info("Block database at height: ~p", [Height]),
    ets:insert(?CACHE, [{?CACHE_HEIGHT, Height}]),
    {ok, #state{
            height = Height
            }}.

load(Conn, Hash, Block, _Sync, Ledger, State=#state{}) ->
    BlockHeight = blockchain_block_v1:height(Block),
    ?assertEqual(BlockHeight, State#state.height + 1,
                 "New block must line up with stored height"),
    %% Seperate the queries to avoid the batches getting too big
    BlockQueries = q_insert_block(Hash, Block, Ledger, [], State),
    ok = ?BATCH_QUERY(Conn, BlockQueries),
    ets:insert(?CACHE, [{?CACHE_HEIGHT, BlockHeight}]),
    {ok, State#state{height=BlockHeight}}.

q_insert_block(Hash, Block, Ledger, Queries, State=#state{base_secs=BaseSecs}) ->
    {ElectionEpoch, EpochStart} = blockchain_block_v1:election_info(Block),
    BlockTime = blockchain_block_v1:time(Block),
    BlockDate = calendar:gregorian_seconds_to_datetime(BaseSecs + BlockTime),
    Params = [blockchain_block_v1:height(Block),
              BlockTime,
              BlockDate,
              ?BIN_TO_B64(blockchain_block_v1:prev_hash(Block)),
              ?BIN_TO_B64(Hash),
              length(blockchain_block_v1:transactions(Block)),
              blockchain_block_v1:hbbft_round(Block),
              ElectionEpoch,
              EpochStart,
              ?BIN_TO_B64(blockchain_block_v1:rescue_signature(Block))],
    [{?S_INSERT_BLOCK, Params}
     | q_insert_signatures(Block, q_insert_transactions(Block, Queries, Ledger, State), State)].

q_insert_signatures(Block, Queries, #state{}) ->
    Height = blockchain_block_v1:height(Block),
    Signatures = blockchain_block_v1:signatures(Block),
    lists:foldl(fun({Signer, Signature}, Acc) ->
                        [{?S_INSERT_BLOCK_SIG,
                          [
                           Height,
                           ?BIN_TO_B58(Signer),
                           ?BIN_TO_B64(Signature)
                          ]} | Acc]
                         end, Queries, Signatures).

q_insert_transactions(Block, Queries, Ledger, #state{}) ->
    Height = blockchain_block_v1:height(Block),
    Time = blockchain_block_v1:time(Block),
    Txns = blockchain_block_v1:transactions(Block),
    lists:foldl(fun(T, Acc) ->
                        [{?S_INSERT_TXN,
                          [Height,
                           Time,
                           ?BIN_TO_B64(blockchain_txn:hash(T)),
                           be_txn:to_type(blockchain_txn:type(T)),
                           be_txn:to_json(T, Ledger)
                          ]} | Acc]
                end, Queries, Txns).
