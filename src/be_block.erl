-module(be_block).

-include_lib("epgsql/include/epgsql.hrl").

-export([init/1, current_height/1, load/2]).

-define (BIN_TO_B58(B), list_to_binary(libp2p_crypto:bin_to_b58((B)))).
-define (BIN_TO_B64(B), list_to_binary(base64:encode_to_string((B)))).
-define(Q_CURRENT_HEIGHT, "current_height").
-define(Q_INSERT_BLOCK, "insert_block").
-define(Q_INSERT_BLOCK_SIG, "insert_block_signature").
-define(Q_INSERT_TXN, "insert_transaction").
-define(Q_INSERT_ACTOR, "insert_actor").

-record(state,
       {
        conn :: epgsql:connection(),
        s_insert_block :: epgsql:statement(),
        s_current_height :: epgsql:statement(),
        s_insert_txn :: epgsql:statement(),
        s_insert_block_sig :: epgsql:statement(),
        s_insert_actor :: epgsql:statement()
       }).

-type state() :: #state{}.
-export_type([state/0]).

-spec init(epgsql:connection()) -> #state{}.
init(Conn) ->
    {ok, _} = epgsql:update_type_cache(Conn, [{epgsql_codec_json, jsone}]),
    {ok, InsertBlock} =
        epgsql:parse(Conn, ?Q_INSERT_BLOCK,
                     "insert into blocks (height, prev_hash, time, hbbft_round, election_epoch, epoch_start, rescue_signature) values ($1, $2, $3, $4, $5, $6, $7);", []),
    {ok, CurrentHeight} =
        epgsql:parse(Conn, ?Q_CURRENT_HEIGHT,
                     "select MAX(height) from blocks", []),
    {ok, InsertBlockSig} =
        epgsql:parse(Conn, ?Q_INSERT_BLOCK_SIG,
                     "insert into block_signatures (block, signer, signature) values ($1, $2, $3)", []),
    {ok, InsertTxn} =
        epgsql:parse(Conn, ?Q_INSERT_TXN,
                     "insert into transactions (block, hash, type, fields) values ($1, $2, $3, $4)", []),
    {ok, InsertActor} =
        epgsql:parse(Conn, ?Q_INSERT_ACTOR,
                     "insert into transaction_actors (actor, actor_role, transaction_hash) values ($1, $2, $3)", []),
    #state{
       conn = Conn,
       s_insert_block = InsertBlock,
       s_current_height = CurrentHeight,
       s_insert_block_sig = InsertBlockSig,
       s_insert_txn = InsertTxn,
       s_insert_actor = InsertActor
      }.

current_height(#state{conn=Conn, s_current_height=Stmt}) ->
    {ok, _, [{V}]} = epgsql:prepared_query(Conn, Stmt#statement.name, []),
    case V of
        null -> {ok, 0};
        _ -> {ok, V}
    end.

load(Block, State=#state{conn=Conn}) ->
    %% Seperate the queries to avoid the batches getting too big
    BlockQueries = q_insert_block(Block, [], State),
    ActorQueries = q_insert_transaction_actors(Block, [], State),
    epgsql:with_transaction(Conn, fun(_) ->
                                          run_queries(BlockQueries, State),
                                          run_queries(ActorQueries, State)
                                  end).

run_queries(Queries, #state{conn=Conn}) ->
    Results = epgsql:execute_batch(Conn, Queries),
    %% Find any errors and throw an error
    %% to roll back the batch
    case lists:filter(fun({ok, _}) -> false;
                         ({error, _}) -> true
                      end, Results) of
        [] -> ok;
        Errors ->
            throw({load_error, Errors})
    end.


-type query() :: [{Stmt::epgsql:statement(), Params::[any()]}].

-spec q_insert_block(blockchain:block(), query(), #state{}) -> query().
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

-spec q_insert_signatures(blockchain:block(), query(), #state{}) -> query().
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

-spec q_insert_transactions(blockchain:block(), query(), #state{}) -> query().
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

-spec q_insert_transaction_actors(blockchain:block(), query(), #state{}) -> query().
q_insert_transaction_actors(Block, Query, #state{s_insert_actor=Stmt}) ->
    Txns = blockchain_block_v1:transactions(Block),
    lists:foldl(fun(T, Acc) ->
                        TxnHash = ?BIN_TO_B64(blockchain_txn:hash(T)),
                        lists:foldl(fun({Role, Key}, ActorAcc) ->
                                            [{Stmt, [Key, Role, TxnHash]} | ActorAcc]
                                    end, Acc, be_txn:to_actors(T))
                end, Query, Txns).
