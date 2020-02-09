-module(be_block).

-behavior(be_db_worker).
-behavior(be_block_handler).

-include("be_block_handler.hrl").
-include_lib("stdlib/include/assert.hrl").

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load/6]).

-define(Q_INSERT_BLOCK, "insert_block").
-define(Q_INSERT_BLOCK_SIG, "insert_block_signature").
-define(Q_INSERT_TXN, "insert_transaction").

-record(state,
       {
        height :: non_neg_integer(),

        base_secs=calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) :: pos_integer(),
        s_insert_block :: epgsql:statement(),
        s_insert_txn :: epgsql:statement(),
        s_insert_block_sig :: epgsql:statement()
       }).

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, _} =
        epgsql:parse(Conn, ?Q_INSERT_BLOCK,
                     "insert into blocks (height, time, timestamp, prev_hash, block_hash, transaction_count, hbbft_round, election_epoch, epoch_start, rescue_signature) values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);", []),
    {ok, _} =
        epgsql:parse(Conn, ?Q_INSERT_BLOCK_SIG,
                     "insert into block_signatures (block, signer, signature) values ($1, $2, $3)", []),
    {ok, _} =
        epgsql:parse(Conn, ?Q_INSERT_TXN,
                     "insert into transactions (block, hash, type, fields) values ($1, $2, $3, $4)", []),

    ok.

%%
%% be_block_handler
%%

init(Conn) ->
    {ok, InsertBlock} = epgsql:describe(Conn, statement, ?Q_INSERT_BLOCK),
    {ok, InsertBlockSig} = epgsql:describe(Conn, statement, ?Q_INSERT_BLOCK_SIG),
    {ok, InsertTxn} = epgsql:describe(Conn, statement, ?Q_INSERT_TXN),

    {ok, _, [{HeightStr}]} = epgsql:squery(Conn, "select max(height) from blocks"),
    Height = case HeightStr of
                 null -> 0;
                 _ -> binary_to_integer(HeightStr)
             end,
    {ok, #state{
            height = Height,
            s_insert_block = InsertBlock,
            s_insert_block_sig = InsertBlockSig,
            s_insert_txn = InsertTxn
           }}.

load(Conn, Hash, Block, _Sync, Ledger, State=#state{}) ->
    BlockHeight = blockchain_block_v1:height(Block),
    ?assertEqual(BlockHeight, State#state.height + 1,
                 "New block must line up with stored height"),
    %% Seperate the queries to avoid the batches getting too big
    BlockQueries = q_insert_block(Hash, Block, Ledger, [], State),
    be_block_handler:run_queries(Conn, BlockQueries, State#state{height=BlockHeight}).

q_insert_block(Hash, Block, Ledger, Queries, State=#state{s_insert_block=Stmt, base_secs=BaseSecs}) ->
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
    [{Stmt, Params} | q_insert_signatures(Block, q_insert_transactions(Block, Queries, Ledger, State), State)].

q_insert_signatures(Block, Queries, #state{s_insert_block_sig=Stmt}) ->
    Height = blockchain_block_v1:height(Block),
    Signatures = blockchain_block_v1:signatures(Block),
    lists:foldl(fun({Signer, Signature}, Acc) ->
                        [{Stmt,
                          [
                           Height,
                           ?BIN_TO_B58(Signer),
                           ?BIN_TO_B64(Signature)
                          ]} | Acc]
                         end, Queries, Signatures).

q_insert_transactions(Block, Queries, Ledger, #state{s_insert_txn=Stmt}) ->
    Height = blockchain_block_v1:height(Block),
    Txns = blockchain_block_v1:transactions(Block),
    lists:foldl(fun(T, Acc) ->
                        [{Stmt,
                          [Height,
                           ?BIN_TO_B64(blockchain_txn:hash(T)),
                           be_txn:to_type(blockchain_txn:type(T)),
                           be_txn:to_json(T, Ledger)
                          ]} | Acc]
                end, Queries, Txns).
