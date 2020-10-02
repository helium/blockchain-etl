-module(be_db_backfill).

-include("be_db_worker.hrl").
-include("be_db_follower.hrl").

-export([receipts_challenger/4, reversed_receipts_path/3]).

-define(INSERT_RECEIPTS_CHALLENGERS, [
    "insert into transaction_actors ",
    "(select fields->>'challenger' as actor, 'challenger' as actor_role, hash as transaction_hash, block ",
    " from transactions ",
    " where type = 'poc_receipts_v1' and block between $1 and $2) ",
    "on conflict do nothing"
]).

receipts_challenger(MinBlock, MaxBlock, BatchSize, _Fun) when
    MinBlock >= MaxBlock orelse BatchSize == 0
->
    0;
receipts_challenger(MinBlock, MaxBlock, BatchSize, Fun) ->
    BatchMinBlock = max(MinBlock, MaxBlock - BatchSize),
    {ok, Inserted} = receipts_challenger(BatchMinBlock, MaxBlock),
    Fun(BatchMinBlock, MaxBlock, Inserted),
    timer:sleep(100),
    Inserted + receipts_challenger(MinBlock, BatchMinBlock, BatchSize, Fun).

receipts_challenger(MinBlock, MaxBlock) ->
    ?EQUERY(?INSERT_RECEIPTS_CHALLENGERS, [MinBlock, MaxBlock]).

%%
%% Fixes up reversed poc paths which happened due to fold error in
%% poc_receipts_v1:to_json
%%
-define(TARGET_BLOCK_RECEIPTS_TXNS, [
    "select t.hash, t.fields ",
    "from blocks b inner join transactions t on b.height = t.block "
    "where b.height = $1 and t.type = 'poc_receipts_v1'\n"
    "and jsonb_array_length(t.fields #> '{path}') > 1"
]).

-define(UPDATE_RECEIPTS_TXN, [
    "update transactions set "
    " fields = $3",
    "where block = $1 and hash = $2"
]).

reversed_receipts_path(MinBlock, MaxBlock, Fun) ->
    Chain = blockchain_worker:blockchain(),
    Ledger = blockchain:ledger(Chain),
    %% Returns a map receipts_txn:hash() -> receipts_txn
    ReceiptsTxnsForBlock = fun(Height) ->
        case blockchain:get_block(Height, Chain) of
            {error, not_found} ->
                #{};
            {ok, Block} ->
                BlockReceiptTxns = lists:filtermap(
                    fun(Txn) ->
                        case blockchain_txn:type(Txn) == blockchain_txn_poc_receipts_v1 of
                            true -> {true, {blockchain_txn:hash(Txn), Txn}};
                            _ -> false
                        end
                    end,
                    blockchain_block:transactions(Block)
                ),
                maps:from_list(BlockReceiptTxns)
        end
    end,
    %% returns the B58 addresses for the challengees in a path for a given
    %% receipts txn
    ValidPathForReceiptsTxn = fun(Txn) ->
        lists:map(
            fun(PathElem) ->
                ?BIN_TO_B58(blockchain_poc_path_element_v1:challengee(PathElem))
            end,
            blockchain_txn_poc_receipts_v1:path(Txn)
        )
    end,
    StoredPathForReceiptsTxn = fun(#{<<"path">> := Path}) ->
        lists:map(
            fun(PathElem) ->
                maps:get(<<"challengee">>, PathElem)
            end,
            Path
        )
    end,
    lists:foldl(
        fun(Height, Acc) ->
            BlockTxns = ReceiptsTxnsForBlock(Height),
            {ok, _, StoredTxns} = ?EQUERY(?TARGET_BLOCK_RECEIPTS_TXNS, [Height]),
            BlockUpdated = lists:foldl(
                fun({TxnHashStr, TxnFields}, BlockAcc) ->
                    TxnHash = ?B64_TO_BIN(TxnHashStr),
                    StoredPath = StoredPathForReceiptsTxn(TxnFields),
                    BlockTxn = maps:get(TxnHash, BlockTxns),
                    ValidPath = ValidPathForReceiptsTxn(BlockTxn),
                    case lists:reverse(StoredPath) == ValidPath of
                        true ->
                            %% Reversed path, re-store json for transaction
                            Fields = be_txn:to_json(BlockTxn, Ledger, Chain),
                            {ok, 1} = ?EQUERY(?UPDATE_RECEIPTS_TXN, [Height, TxnHashStr, Fields]),
                            BlockAcc + 1;
                        false ->
                            %% Correct path order, leave json alone
                            BlockAcc
                    end
                end,
                0,
                StoredTxns
            ),
            Fun(Height, BlockUpdated),
            BlockUpdated + Acc
        end,
        0,
        lists:seq(MinBlock, MaxBlock)
    ).
