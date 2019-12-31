-module(be_block).

-behavior(be_block_handler).

-include("be_block_handler.hrl").

-export([init/1, load/4]).

-define(Q_INSERT_BLOCK, "insert_block").
-define(Q_INSERT_BLOCK_SIG, "insert_block_signature").
-define(Q_INSERT_TXN, "insert_transaction").

-record(state,
       {
        conn :: epgsql:connection(),
        s_insert_block :: epgsql:statement(),
        s_insert_txn :: epgsql:statement(),
        s_insert_block_sig :: epgsql:statement()
       }).

init(Conn) ->
    {ok, _} = epgsql:update_type_cache(Conn, [{epgsql_codec_json, jsone}]),
    {ok, InsertBlock} =
        epgsql:parse(Conn, ?Q_INSERT_BLOCK,
                     "insert into blocks (height, prev_hash, time, hbbft_round, election_epoch, epoch_start, rescue_signature) values ($1, $2, $3, $4, $5, $6, $7);", []),
    {ok, InsertBlockSig} =
        epgsql:parse(Conn, ?Q_INSERT_BLOCK_SIG,
                     "insert into block_signatures (block, signer, signature) values ($1, $2, $3)", []),
    {ok, InsertTxn} =
        epgsql:parse(Conn, ?Q_INSERT_TXN,
                     "insert into transactions (block, hash, type, fields) values ($1, $2, $3, $4)", []),
    {ok, #state{
            conn = Conn,
            s_insert_block = InsertBlock,
            s_insert_block_sig = InsertBlockSig,
            s_insert_txn = InsertTxn
           }}.

load(_Hash, Block, _Ledger, State=#state{}) ->
    %% Seperate the queries to avoid the batches getting too big
    BlockQueries = q_insert_block(Block, [], State),
    be_block_handler:run_queries(BlockQueries, State#state.conn, State).

q_insert_block(Block, Query, State=#state{s_insert_block=Stmt}) ->
    {ElectionEpoch, EpochStart} = blockchain_block_v1:election_info(Block),
    Params = [blockchain_block_v1:height(Block),
              ?BIN_TO_B64(blockchain_block_v1:prev_hash(Block)),
              blockchain_block_v1:time(Block),
              blockchain_block_v1:hbbft_round(Block),
              ElectionEpoch,
              EpochStart,
              ?BIN_TO_B64(blockchain_block_v1:rescue_signature(Block))],
    [{Stmt, Params} | q_insert_signatures(Block, q_insert_transactions(Block, Query, State), State)].

q_insert_signatures(Block, Query, #state{s_insert_block_sig=Stmt}) ->
    Height = blockchain_block_v1:height(Block),
    Signatures = blockchain_block_v1:signatures(Block),
    lists:foldl(fun({Signer, Signature}, Acc) ->
                        [{Stmt,
                          [
                           Height,
                           ?BIN_TO_B58(Signer),
                           ?BIN_TO_B64(Signature)
                          ]} | Acc]
                         end, Query, Signatures).

q_insert_transactions(Block, Query, #state{s_insert_txn=Stmt}) ->
    Height = blockchain_block_v1:height(Block),
    Txns = blockchain_block_v1:transactions(Block),
    lists:foldl(fun(T, Acc) ->
                        [{Stmt,
                          [Height,
                           ?BIN_TO_B64(blockchain_txn:hash(T)),
                           be_txn:to_type(blockchain_txn:type(T)),
                           be_txn:to_json(T)
                          ]} | Acc]
                end, Query, Txns).
