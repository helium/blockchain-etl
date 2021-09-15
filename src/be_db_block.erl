-module(be_db_block).

-behavior(be_db_worker).
-behavior(be_db_follower).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").
-include_lib("stdlib/include/assert.hrl").

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, snap_loaded/3, load_chain/3, load_block/6]).
%% api
-export([block_height/1, maybe_write_snapshot/2]).

-define(S_BLOCK_HEIGHT, "block_height").
-define(S_INSERT_BLOCK, "insert_block").
-define(S_INSERT_BLOCK_SIG, "insert_block_signature").
-define(S_INSERT_TXN, "insert_transaction").

-record(state, {
    height :: non_neg_integer(),
    chain :: undefined |  blockchain:blockchain(),
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

snap_loaded(Conn, Chain, State) ->
    Ledger = blockchain:ledger(Chain),
    {ok, Height} = blockchain_ledger_v1:current_height(Ledger),
    {ok, HeadBlock} = blockchain:get_block(Height, Chain),
    {ok, Genesis} = blockchain:genesis_block(Chain),
    Blocks = blockchain:fold_chain(fun(Block, Acc) ->
                                  [Block|Acc]
                          end, [], HeadBlock, Chain),
    lager:info("loading ~p blocks", [length(Blocks)]),
    Heights = lists:map(fun(Block) ->
                          Hash = blockchain_block:hash_block(Block),
                          BHeight = blockchain_block:height(Block),
                          lager:info("loading block ~p", [blockchain_block:height(Block)]),
                          BlockQueries = q_insert_block(Hash, Block, Ledger, undefined, [], State),
                          ok = ?BATCH_QUERY(Conn, BlockQueries),
                          BHeight
                  end, [Genesis|Blocks]),
    stub_missing_blocks(Conn, 2, Heights, Ledger, State),
    lager:info("setting block height to ~p", [Height]),
    {ok, State#state{height=Height, chain=Chain}}.

load_chain(_Conn, Chain, State) ->
    {ok, State#state{chain=Chain}}.

stub_missing_blocks(Conn, Height, Heights, Ledger, State) ->
    case lists:member(Height, Heights) of
        true -> ok;
        false ->
            Block = blockchain_block_v1:new(#{prev_hash => <<>>, height => Height, time => 0,
                                      hbbft_round => 0, transactions => [], signatures => [],
                                      election_epoch => 0, epoch_start => 0, seen_votes => [],
                                      bba_completion => <<>>}),
            Hash = blockchain_block:hash_block(Block),
            lager:info("stubbing block ~p", [blockchain_block:height(Block)]),
            BlockQueries = q_insert_block(Hash, Block, Ledger, undefined, [], State),
            ok = ?BATCH_QUERY(Conn, BlockQueries),
            stub_missing_blocks(Conn, Height +1, Heights, Ledger, State)
    end.

load_block(Conn, Hash, Block, _Sync, Ledger, State = #state{chain=Chain}) ->
    BlockHeight = blockchain_block_v1:height(Block),
    ?assertEqual(
        BlockHeight,
        State#state.height + 1,
        "New block must line up with stored height"
    ),
    %% Seperate the queries to avoid the batches getting too big
    BlockQueries = q_insert_block(Hash, Block, Ledger, Chain, [], State),
    ok = ?BATCH_QUERY(Conn, BlockQueries),
    maybe_write_snapshot(Block, Chain),
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
    LatestBin = jsone:encode(#{height => Height,
                               hash => base64url:encode(SnapshotHash)}),
    Latest = filename:join([SnapshotDir, "latest-snap.json"]),
    Filename = filename:join([SnapshotDir, io_lib:format("snap-~p", [Height])]),
    ok = file:write_file(Filename, BinSnap),
    ok = file:write_file(Latest, LatestBin).

q_insert_block(Hash, Block, Ledger, Chain, Queries, State = #state{base_secs = BaseSecs}) ->
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
        | q_insert_signatures(
            Block,
            q_insert_transactions(Block, Queries, Ledger, Chain, State),
            State
        )
    ].

q_insert_signatures(Block, Queries, #state{}) ->
    Height = blockchain_block_v1:height(Block),
    Signatures = blockchain_block_v1:signatures(Block),
    lists:foldl(
        fun({Signer, Signature}, Acc) ->
            [
                {?S_INSERT_BLOCK_SIG, [
                    Height,
                    ?BIN_TO_B58(Signer),
                    ?BIN_TO_B64(Signature)
                ]}
                | Acc
            ]
        end,
        Queries,
        Signatures
    ).

q_insert_transactions(Block, Queries, Ledger, Chain, #state{}) ->
    Height = blockchain_block_v1:height(Block),
    Time = blockchain_block_v1:time(Block),
    Txns = blockchain_block_v1:transactions(Block),
    JsonOpts = [{ledger, Ledger}, {chain, Chain}],
    lists:foldl(
        fun(T, Acc) ->
            Json = #{type := Type} = be_txn:to_json(T, JsonOpts),
            [
                {?S_INSERT_TXN, [
                    Height,
                    Time,
                    ?BIN_TO_B64(blockchain_txn:hash(T)),
                    Type,
                    Json
                ]}
                | Acc
            ]
        end,
        Queries,
        Txns
    ).
