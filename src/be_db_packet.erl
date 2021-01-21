-module(be_db_packet).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-behavior(be_db_worker).
-behavior(be_db_follower).

-record(state, {}).

-define(S_INSERT_PACKET, "insert_packet").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} = epgsql:parse(
        Conn,
        ?S_INSERT_PACKET,
        [
            "insert into packets (block, transaction_hash, time, gateway, num_packets, num_dcs) ",
            "values ($1, $2, $3, $4, $5, $6) ",
            "on conflict do nothing "
        ],
        []
    ),
    #{?S_INSERT_PACKET => S1}.

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
            blockchain_txn:type(T) == blockchain_txn_state_channel_close_v1
        end,
        blockchain_block_v1:transactions(Block)
    ),
    Queries = lists:foldl(
        fun(T, TAcc) ->
            TxnHash = ?BIN_TO_B64(blockchain_txn:hash(T)),
            PacketMap = collect_packets(blockchain_state_channel_v1:summaries(blockchain_txn_state_channel_close_v1:state_channel(T)), #{}),
            lists:foldl(
                fun(Entry, RAcc) ->
                    q_insert_packet(
                        BlockHeight,
                        TxnHash,
                        BlockTime,
                        Entry,
                        RAcc
                    )
                end,
                TAcc,
                maps:to_list(PacketMap)
            )
        end,
        [],
        Txns
    ),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State}.

-type packet_map() :: #{
    Gateway :: libp2p_crypto:pubkey_bin() => {NumPackets :: pos_integer(), NumDCs :: pos_integer()}
}.

-spec collect_packets(blockchain_state_channel_close_v1:summaries(), packet_map()) -> packet_map().
collect_packets([], PacketMap) ->
    PacketMap;
collect_packets([Summary | Rest], Map) ->
    Key = blockchain_state_channel_summary_v1:client_pubkeybin(Summary),
    {NumPackets, NumDCs} = maps:get(Key, Map, {0, 0}),
    collect_packets(Rest, 
        maps:put(Key, 
                {
                    NumPackets + blockchain_state_channel_summary_v1:num_packets(Summary), 
                    NumDCs + blockchain_state_channel_summary_v1:num_dcs(Summary)
                }, 
                Map)).

q_insert_packet(BlockHeight, TxnHash, BlockTime, {Gateway, {NumPackets, NumDCs}}, Queries) ->
    Params = [
        BlockHeight,
        TxnHash,
        BlockTime,
        ?BIN_TO_B58(Gateway),
        NumPackets,
        NumDCs
    ],
    [{?S_INSERT_PACKET, Params} | Queries].
