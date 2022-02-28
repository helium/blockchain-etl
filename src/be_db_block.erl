-module(be_db_block).

-behavior(be_db_worker).
-behavior(be_db_follower).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").
-include_lib("stdlib/include/assert.hrl").

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).
%% api
-export([block_height/1, maybe_write_snapshot/2]).

-define(S_BLOCK_HEIGHT, "block_height").
-define(S_INSERT_BLOCK, "insert_block").
-define(S_INSERT_BLOCK_SIG, "insert_block_signature").
-define(S_INSERT_TXN, "insert_transaction").

-record(state, {
    height :: non_neg_integer(),

    base_secs = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) :: pos_integer()
}).

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S0} =
        epgsql:parse(
            Conn,
            ?S_BLOCK_HEIGHT,
            "select max(height) from blocks",
            []
        ),

    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_INSERT_BLOCK,
            [
                "insert into blocks ",
                "(created_at, height, time, timestamp, prev_hash, block_hash, transaction_count, ",
                " hbbft_round, election_epoch, epoch_start, rescue_signature, snapshot_hash) ",
                "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12);"
            ],
            []
        ),
    {ok, S2} =
        epgsql:parse(
            Conn,
            ?S_INSERT_BLOCK_SIG,
            "insert into block_signatures (block, signer, signature) values ($1, $2, $3)",
            []
        ),
    {ok, S3} =
        epgsql:parse(
            Conn,
            ?S_INSERT_TXN,
            [
                "insert into transactions (block, time, hash, type, fields) values ($1, $2, $3, $4, $5) ",
                "on conflict do nothing"
            ],
            []
        ),

    #{
        ?S_BLOCK_HEIGHT => S0,
        ?S_INSERT_BLOCK => S1,
        ?S_INSERT_BLOCK_SIG => S2,
        ?S_INSERT_TXN => S3
    }.

%%
%% be_block_handler
%%

init(_) ->
    {ok, _, [{Value}]} = ?PREPARED_QUERY(?S_BLOCK_HEIGHT, []),
    Height =
        case Value of
            null -> 0;
            _ -> Value
        end,
    lager:info("Block database at height: ~p", [Height]),
    {ok, #state{
        height = Height
    }}.

load_block(Conn, Hash, Block, _Sync, Ledger, State = #state{}) ->
    BlockHeight = blockchain_block_v1:height(Block),
    ?assertEqual(
        BlockHeight,
        State#state.height + 1,
        "New block must line up with stored height"
    ),
    %% Seperate the queries to avoid the batches getting too big
    BlockQueries = q_insert_block(Hash, Block, Ledger, State),
    ok = ?BATCH_QUERY(Conn, BlockQueries),
    maybe_write_snapshot(Block, blockchain_worker:blockchain()),
    {ok, State#state{height = BlockHeight}}.

%%
%% API
%%

block_height(#state{height = Height}) ->
    Height.

%%
%% Internal
%%

maybe_write_snapshot(Height, Chain) when is_number(Height) ->
    {ok, Block} = blockchain:get_block(Height, Chain),
    maybe_write_snapshot(Block, Chain);
maybe_write_snapshot(Block, Chain) ->
    Height = blockchain_block_v1:height(Block),
    try
        maybe_write_snapshot(
            Height,
            blockchain_block_v1:snapshot_hash(Block),
            os:getenv("SNAPSHOT_DIR"),
            Chain
        )
    catch
        What:Why:Where ->
            lager:warning("Failed to write snapshot ~p: ~p", [Height, {What, Why, Where}])
    end.

maybe_write_snapshot(_, _, false, _Chain) ->
    ok;
maybe_write_snapshot(_, <<>>, _, _Chain) ->
    ok;
maybe_write_snapshot(Height, SnapshotHash, SnapshotDir, Chain) ->
    {ok, BinSnap} =
        case blockchain:get_snapshot(SnapshotHash, Chain) of
            {error, not_found} -> blockchain:get_snapshot(Height, Chain);
            Other -> Other
        end,
    Filename = filename:join([SnapshotDir, io_lib:format("snap-~p", [Height])]),
    ok = blockchain:save_bin_snapshot(Filename, BinSnap),
    ok = blockchain:save_compressed_bin_snapshot(Filename, BinSnap), %% function adds ".gz"
    {ok, FileSHA} = blockchain:hash_bin_snapshot(BinSnap),
    Size = blockchain:size_bin_snapshot(BinSnap),
    LatestMap0 = #{
        height => Height,
        file_size => Size,
        file_hash => base64url:encode(FileSHA),
        hash => base64url:encode(SnapshotHash)
    },
    LatestFilePath = filename:join([SnapshotDir, "latest-snap.json"]),
    LatestMap = case blockchain:maybe_get_compressed_snapdata(Filename) of
                    undefined -> LatestMap0;
                    {ok, CSz, CHash} -> LatestMap0#{compressed_hash => base64url:encode(CHash),
                                                    compressed_size => CSz }
                end,
    ok = file:write_file(LatestFilePath, jsone:encode(LatestMap)).

q_insert_block(Hash, Block, Ledger, State = #state{base_secs = BaseSecs}) ->
    {ElectionEpoch, EpochStart} = blockchain_block_v1:election_info(Block),
    BlockTime = blockchain_block_v1:time(Block),
    BlockDate = calendar:gregorian_seconds_to_datetime(BaseSecs + BlockTime),
    CurrentDate = calendar:universal_time(),
    Params = [
        CurrentDate,
        blockchain_block_v1:height(Block),
        BlockTime,
        BlockDate,
        ?BIN_TO_B64(blockchain_block_v1:prev_hash(Block)),
        ?BIN_TO_B64(Hash),
        length(blockchain_block_v1:transactions(Block)),
        blockchain_block_v1:hbbft_round(Block),
        ElectionEpoch,
        EpochStart,
        ?BIN_TO_B64(blockchain_block_v1:rescue_signature(Block)),
        ?MAYBE_B64(blockchain_block_v1:snapshot_hash(Block))
    ],
    [
        {?S_INSERT_BLOCK, Params}
        | q_insert_signatures(Block, State) ++
            q_insert_transactions(Block, Ledger, State)
    ].

q_insert_signatures(Block, #state{}) ->
    Height = blockchain_block_v1:height(Block),
    Signatures = blockchain_block_v1:signatures(Block),
    lists:map(
        fun({Signer, Signature}) ->
            {?S_INSERT_BLOCK_SIG, [
                Height,
                ?BIN_TO_B58(Signer),
                ?BIN_TO_B64(Signature)
            ]}
        end,
        Signatures
    ).

q_insert_transactions(Block, Ledger, #state{}) ->
    Height = blockchain_block_v1:height(Block),
    Time = blockchain_block_v1:time(Block),
    Txns = blockchain_block_v1:transactions(Block),
    JsonOpts = [{ledger, Ledger}, {chain, blockchain_worker:blockchain()}],
    be_utils:pmap(
        fun(T) ->
            Json = #{type := Type} = be_txn:to_json(T, JsonOpts),
            {?S_INSERT_TXN, [
                Height,
                Time,
                ?BIN_TO_B64(blockchain_txn:hash(T)),
                Type,
                Json
            ]}
        end,
        Txns
    ).
