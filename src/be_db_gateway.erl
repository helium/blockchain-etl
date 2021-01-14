-module(be_db_gateway).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-behavior(be_db_worker).
-behavior(be_db_follower).

-type gateway_db_hash() :: {AtBlock :: binary(), AtElection :: binary()}.
-type gateway_chain_hash() :: binary().
-type gateway_cache() :: #{
    libp2p_crypto:pubkey_bin() => gateway_db_hash()
}.

-record(state, {
    gateways = #{} :: gateway_cache()
}).

-define(S_INSERT_GATEWAY, "insert_gateway").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_INSERT_GATEWAY,
            [
                "insert into gateways (block, time, address, owner, location, last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce, name, reward_scale) select ",
                "$1 as block, ",
                "$2 as time, ",
                "$3 as address, ",
                "$4 as owner, ",
                "$5 as location, ",
                "$6 as last_poc_challenge, ",
                "$7 as last_poc_onion_key_hash, ",
                "$8 as witnesses, ",
                "$9 as nonce, ",
                "$10 as name, ",
                "$11 as reward_scale; "
            ],
            []
        ),

    #{
        ?S_INSERT_GATEWAY => S1
    }.

%%
%% be_block_handler
%%

init(_) ->
    {ok, Hashes} = mk_gateway_cache(),
    {ok, #state{gateways = Hashes}}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State = #state{gateways = GatewayHashes}) ->
    Active = blockchain_ledger_v1:snapshot_gateways(Ledger),
    BlockHeight = blockchain_block_v1:height(Block),
    BlockTime = blockchain_block_v1:time(Block),
    ChangeType =
        case block_contains_election(Block) of
            true -> election;
            false -> block
        end,
    %% collect the changes in parallel
    ChangeStart = erlang:monotonic_time(millisecond),
    GatewayChanges = blockchain_utils:pmap(
        fun ({Key, GW}) ->
            {Key, GW, gateway_changed(Key, GW, Ledger, ChangeType, GatewayHashes)}
        end,
        Active
    ),
    %% then map the changed entries to their queries
    {Queries, Hashes} =
        lists:foldl(
            fun
                ({_, _, false}, {Queries, Hashes}) ->
                    {Queries, Hashes};
                ({Key, GW, {true, NewDbHash}}, {Queries, Hashes}) ->
                    %% Changed or new
                    NewQueries = [
                        q_insert_gateway(
                            BlockHeight,
                            BlockTime,
                            Key,
                            GW,
                            ChangeType,
                            Ledger
                        )
                        | Queries
                    ],
                    {NewQueries, Hashes#{Key => NewDbHash}}
            end,
            {[], GatewayHashes},
            GatewayChanges
        ),
    ChangeEnd = erlang:monotonic_time(millisecond),
    lager:info("gateways changed: ~p type: ~p block: ~p time: ~p", [
        length(Queries),
        ChangeType,
        BlockHeight,
        ChangeEnd - ChangeStart
    ]),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State#state{gateways = Hashes}}.

q_insert_gateway(BlockHeight, BlockTime, Address, GW, ChangeType, Ledger) ->
    B58Address = ?BIN_TO_B58(Address),
    {ok, Name} = erl_angry_purple_tiger:animal_name(B58Address),
    Location = blockchain_ledger_gateway_v2:location(GW),
    RewardScale =
        case ChangeType of
            block ->
                undefined;
            election ->
                ?MAYBE_FN(
                    fun (L) ->
                        case blockchain_hex:scale(L, Ledger) of
                            {ok, V} -> blockchain_utils:normalize_float(V);
                            {error, _} -> undefined
                        end
                    end,
                    Location
                )
        end,
    Params = [
        BlockHeight,
        BlockTime,
        B58Address,
        ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(GW)),
        ?MAYBE_H3(Location),
        ?MAYBE_UNDEFINED(blockchain_ledger_gateway_v2:last_poc_challenge(GW)),
        ?MAYBE_B64(blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GW)),
        witnesses_to_json(blockchain_ledger_gateway_v2:witnesses(GW)),
        blockchain_ledger_gateway_v2:nonce(GW),
        Name,
        RewardScale
    ],
    {?S_INSERT_GATEWAY, Params}.

witnesses_to_json(Witnesses) ->
    maps:fold(
        fun (Key, Witness, Acc) ->
            Acc#{?BIN_TO_B58(Key) => witness_to_json(Witness)}
        end,
        #{},
        Witnesses
    ).

witness_to_json(Witness) ->
    #{
        <<"histogram">> => blockchain_ledger_gateway_v2:witness_hist(Witness),
        <<"first_time">> => ?MAYBE_UNDEFINED(
            blockchain_ledger_gateway_v2:witness_first_time(Witness)
        ),
        <<"recent_time">> => ?MAYBE_UNDEFINED(
            blockchain_ledger_gateway_v2:witness_recent_time(Witness)
        )
    }.

-spec block_contains_election(blockchain_block:block()) -> boolean().
block_contains_election(Block) ->
    lists:any(
        fun (Txn) -> blockchain_txn:type(Txn) == blockchain_txn_consensus_group_v1 end,
        blockchain_block:transactions(Block)
    ).

%%
%% Gateway Cache
%%

-spec mk_gateway_cache() -> {ok, gateway_cache()} | {error, term()}.
mk_gateway_cache() ->
    lager:info("Constructing gateway cache"),
    {ok, _, GWList} = ?EQUERY(
        "select address, owner, location, last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce, reward_scale from gateway_inventory",
        []
    ),
    Result = maps:from_list(
        blockchain_utils:pmap(
            fun (
                Entry =
                    {Address, _Owner, _Location, _LastPocChallenge, _LastPocOnionKeyHash,
                        _Witnesses, _Nonce, _RewardScale}
            ) ->
                {?B58_TO_BIN(Address), mk_gateway_db_hash(Entry)}
            end,
            GWList
        )
    ),
    lager:info("Constructed gateway cache: ~p entries", [map_size(Result)]),
    {ok, Result}.

-spec build_hash(fun((Key :: atom()) -> iolist()), Keys :: [atom()]) -> binary().
build_hash(Fun, Keys) ->
    crypto:hash_final(
        lists:foldl(
            fun (Key, H) ->
                crypto:hash_update(H, [Fun(Key)])
            end,
            crypto:hash_init(sha256),
            Keys
        )
    ).

-define(BLOCK_HASH_KEYS, [
    owner,
    location,
    last_poc_challenge,
    last_poc_onion_hash,
    witnesses,
    nonce
]).

-define(ELECTION_HASH_KEYS, [reward_scale]).

to_bin(undefined) ->
    <<>>;
to_bin(null) ->
    <<>>;
to_bin(V) when is_binary(V) ->
    V;
to_bin(V) when is_integer(V) ->
    <<(V)>>;
to_bin(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 8}, compact]).

to_bin(_Fun, undefined) ->
    <<>>;
to_bin(_Fun, null) ->
    <<>>;
to_bin(Fun, V) ->
    to_bin(Fun(V)).

-spec mk_gateway_db_hash(tuple()) -> gateway_db_hash().
mk_gateway_db_hash(
    {_Address, Owner, Location, LastPocChallenge, LastPocOnionKeyHash, Witnesses, Nonce,
        RewardScale}
) ->
    BlockHashData = fun
        (owner) ->
            ?B58_TO_BIN(Owner);
        (location) ->
            to_bin(fun (L) -> h3:from_string(binary_to_list(L)) end, Location);
        (last_poc_challenge) ->
            to_bin(LastPocChallenge);
        (last_poc_onion_hash) ->
            to_bin(
                ?MAYBE_FN(
                    fun (Hash) -> ?B64_TO_BIN(Hash) end,
                    LastPocOnionKeyHash
                )
            );
        (witnesses) ->
            MkWitnessData = fun (
                Key,
                #{
                    <<"first_time">> := FirstTime,
                    <<"recent_time">> := RecentTime,
                    <<"histogram">> := Histogram
                }
            ) ->
                FixedHistogram =
                    lists:map(
                        fun ({K, V}) ->
                            [to_bin(binary_to_integer(K)), to_bin(V)]
                        end,
                        lists:keysort(1, maps:to_list(Histogram))
                    ),
                [
                    ?B58_TO_BIN(Key),
                    to_bin(FirstTime),
                    to_bin(RecentTime),
                    FixedHistogram
                ]
            end,
            SortedWitnesses = lists:keysort(1, maps:to_list(Witnesses)),
            lists:foldl(
                fun ({Key, Witness}, Acc) ->
                    [MkWitnessData(Key, Witness) | Acc]
                end,
                [],
                SortedWitnesses
            );
        (nonce) ->
            to_bin(Nonce)
    end,

    ElectionHashData = fun (reward_scale) ->
        to_bin(RewardScale)
    end,

    {build_hash(BlockHashData, ?BLOCK_HASH_KEYS),
        build_hash(ElectionHashData, ?ELECTION_HASH_KEYS)}.

-spec mk_gateway_chain_hash(
    blockchain_ledger_gateway_v2:gateway(),
    block | election,
    blockchain_ledger_v1:ledger()
) ->
    gateway_chain_hash().
mk_gateway_chain_hash(GW, block, _Ledger) ->
    BlockHashData = fun
        (owner) ->
            to_bin(blockchain_ledger_gateway_v2:owner_address(GW));
        (location) ->
            to_bin(blockchain_ledger_gateway_v2:location(GW));
        (last_poc_challenge) ->
            to_bin(blockchain_ledger_gateway_v2:last_poc_challenge(GW));
        (last_poc_onion_hash) ->
            to_bin(blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GW));
        (witnesses) ->
            MkWitnessData = fun (Key, Witness) ->
                FirstTime = blockchain_ledger_gateway_v2:witness_first_time(Witness),
                RecentTime = blockchain_ledger_gateway_v2:witness_recent_time(Witness),
                Histogram = lists:keysort(
                    1,
                    maps:to_list(blockchain_ledger_gateway_v2:witness_hist(Witness))
                ),
                FixedHistogram = lists:map(
                    fun ({K, V}) ->
                        [to_bin(K), to_bin(V)]
                    end,
                    Histogram
                ),
                [Key, to_bin(FirstTime), to_bin(RecentTime), FixedHistogram]
            end,
            SortedWitnesses = lists:keysort(
                1,
                maps:to_list(blockchain_ledger_gateway_v2:witnesses(GW))
            ),
            lists:foldl(
                fun ({Key, Witness}, Acc) ->
                    [MkWitnessData(Key, Witness) | Acc]
                end,
                [],
                SortedWitnesses
            );
        (nonce) ->
            to_bin(blockchain_ledger_gateway_v2:nonce(GW))
    end,
    build_hash(BlockHashData, ?BLOCK_HASH_KEYS);
mk_gateway_chain_hash(GW, election, Ledger) ->
    ElectionHashData = fun (reward_scale) ->
        to_bin(
            fun (L) ->
                case blockchain_hex:scale(L, Ledger) of
                    {ok, V} -> blockchain_utils:normalize_float(V);
                    {error, _} -> undefined
                end
            end,
            blockchain_ledger_gateway_v2:location(GW)
        )
    end,
    build_hash(ElectionHashData, ?ELECTION_HASH_KEYS).

-spec gateway_changed(
    lib2p2p_crypto:pubkey_bin(),
    blockchain_ledger_gateway_v2:gateway(),
    blockchain_ledger_v1:ledger() | undefined,
    block | election,
    gateway_cache()
) ->
    false | {true, gateway_db_hash()}.
gateway_changed(Key, GW, Ledger, block, Gateways) ->
    GWBlockHash = mk_gateway_chain_hash(GW, block, Ledger),
    case maps:get(Key, Gateways, false) of
        {GWBlockHash, _} ->
            %% block hash has not changed. We ignore election changes
            false;
        {_, AtElectionHash} ->
            %% Block has changed.. keep cached election hash
            {true, {GWBlockHash, AtElectionHash}};
        false ->
            %% Unknown hotspot, use block hash and çalculate election hash
            {true, {GWBlockHash, mk_gateway_chain_hash(GW, election, Ledger)}}
    end;
gateway_changed(Key, GW, Ledger, election, Gateways) ->
    GWBlockHash = mk_gateway_chain_hash(GW, block, Ledger),
    GWElectionHash = mk_gateway_chain_hash(GW, election, Ledger),
    case maps:get(Key, Gateways, false) of
        {GWBlockHash, GWElectionHash} ->
            %% No change for block and election hash
            false;
        _ ->
            %% Either block or election hash has changeed or the gateway was not
            %% found. 
            {true, {GWBlockHash, GWElectionHash}}
    end.
