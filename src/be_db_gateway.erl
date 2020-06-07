-module(be_db_gateway).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-behavior(be_db_worker).
-behavior(be_db_follower).

-type gateway_cache() :: #{libp2p_crypto:pubkey_bin() => binary()}.

-record(state,
       {
        gateways=#{} :: gateway_cache()
       }).

-define(S_INSERT_GATEWAY, "insert_gateway").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(Conn, ?S_INSERT_GATEWAY,
                     ["insert into gateways (block, address, owner, location, alpha, beta, delta, score, last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce) select ",
                      "$1 as block, ",
                      "$2 as address, ",
                      "$3 as owner, ",
                      "$4 as location, ",
                      "$5 as alpha, ",
                      "$6 as beta, ",
                      "$7 as delta, ",
                      "$8 as score, ",
                      "$9 as last_poc_challenge, ",
                      "$10 as last_poc_onion_key_hash, ",
                      "$11 as witnesses, ",
                      "$12 as nonce;"],
                      []),

    #{
      ?S_INSERT_GATEWAY => S1
     }.

%%
%% be_block_handler
%%

init(_) ->
    {ok, init_gateway_cache(#state{})}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State=#state{}) ->
    Active = blockchain_ledger_v1:active_gateways(Ledger),
    BlockHeight = blockchain_block_v1:height(Block),
    {Queries, Hashes} =
        maps:fold(fun(Key, GW, {Queries, Hashes}) ->
                          case gateway_changed(Key, GW, Hashes) of
                              false ->
                                  %% Found and not changed
                                  {Queries, Hashes};
                              {true, NewHash} ->
                                  %% Changed or new
                                  NewQueries = q_insert_gateway(BlockHeight, Key, GW, Ledger, Queries, State),
                                  {NewQueries, Hashes#{Key => NewHash}}
                          end
                  end, {[], State#state.gateways}, Active),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State#state{gateways=Hashes}}.

q_insert_gateway(BlockHeight, Address, GW, Ledger, Queries, #state{}) ->
    {_, _, Score}  = blockchain_ledger_gateway_v2:score(Address, GW, BlockHeight, Ledger),
    Params =
        [BlockHeight,
         ?BIN_TO_B58(Address),
         ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(GW)),
         ?MAYBE_H3(blockchain_ledger_gateway_v2:location(GW)),
         blockchain_ledger_gateway_v2:alpha(GW),
         blockchain_ledger_gateway_v2:beta(GW),
         blockchain_ledger_gateway_v2:delta(GW),
         Score,
         ?MAYBE_UNDEFINED(blockchain_ledger_gateway_v2:last_poc_challenge(GW)),
         ?MAYBE_B64(blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GW)),
         witnesses_to_json(blockchain_ledger_gateway_v2:witnesses(GW)),
         blockchain_ledger_gateway_v2:nonce(GW)
        ],
    [{?S_INSERT_GATEWAY, Params} | Queries].

witnesses_to_json(Witnesses) ->
    maps:fold(fun(Key, Witness, Acc) ->
                      Acc#{?BIN_TO_B58(Key) => witness_to_json(Witness)}
             end, #{}, Witnesses).

witness_to_json(Witness) ->
    #{
      <<"histogram">> =>   blockchain_ledger_gateway_v2:witness_hist(Witness),
      <<"first_time">> =>  ?MAYBE_UNDEFINED(blockchain_ledger_gateway_v2:witness_first_time(Witness)),
      <<"recent_time">> => ?MAYBE_UNDEFINED(blockchain_ledger_gateway_v2:witness_recent_time(Witness))
     }.

%%
%% Gateway Cache
%%

-spec init_gateway_cache(#state{}) -> #state{}.
init_gateway_cache(State=#state{}) ->
    lager:info("Constructing gateway cache"),
    {ok, _, GWList} = ?EQUERY("select address, owner, location, alpha, beta, delta, last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce from gateway_inventory", []),
    Result = maps:from_list(lists:map(fun(Entry={Address,
                                                 _Owner, _Location,
                                                 _Alpha, _Beta, _Delta,
                                                 _LastPocChallenge, _LastPocOnionKeyHash,
                                                 _Witnesses,
                                                 _Nonce}) ->
                                              GWHash = mk_gateway_hash({db, Entry}),
                                              {?B58_TO_BIN(Address), GWHash}
                                      end, GWList)),
    lager:info("Constructed gateway cache: ~p entries", [map_size(Result)]),
    State#state{gateways=Result}.



-spec mk_gateway_hash({db | chain, term()}) -> binary().
mk_gateway_hash(GWDesc) ->
    CacheGW = mk_gateway(GWDesc),
    crypto:hash(sha256, erlang:term_to_binary(CacheGW)).

-spec gateway_changed(lib2p2p_crypto:pubkey_bin(), blockchain_ledger_gateway_v2:gateway(), gateway_cache())
                     -> false | {true, binary()}.
gateway_changed(Key, GW, Gateways) ->
    GWHash = mk_gateway_hash({chain, GW}),
    case maps:get(Key, Gateways, false) of
        GWHash -> false;
        _ -> {true, GWHash}
    end.

-record(gateway_witness,
        {
         address :: libp2p_crypto:pubkey_bin(),
         first_time :: non_neg_integer(),
         recent_time :: non_neg_integer(),
         histogram :: [{integer(), integer()}]
        }).

-record(gateway,
        {
         owner :: libp2p_crypto:pubkey_bin(),
         location :: undefined | h3:h3_index(),
         alpha :: float(),
         beta :: float(),
         delta :: integer(),
         last_poc_challenge :: undefined | non_neg_integer(),
         last_poc_onion_hash :: undefined | binary(),
         witnesses :: [#gateway_witness{}],
         nonce :: pos_integer()
        }).

mk_gateway_witness(Key, {db, #{
                               <<"first_time">> := FirstTime,
                               <<"recent_time">> := RecentTime,
                               <<"histogram">> := Histogram
                              }}) ->
    FixedHistogram = lists:keysort(1, lists:map(fun({K, V}) ->
                                                        {binary_to_integer(K), V}
                                                end, maps:to_list(Histogram))),
    #gateway_witness{
       address = ?B58_TO_BIN(Key),
       first_time = FirstTime,
       recent_time = RecentTime,
       histogram = FixedHistogram
      };
mk_gateway_witness(Key, {chain, Witness}) ->
    #gateway_witness{
       address = Key,
       first_time = blockchain_ledger_gateway_v2:witness_first_time(Witness),
       recent_time = blockchain_ledger_gateway_v2:witness_recent_time(Witness),
       histogram = lists:keysort(1, maps:to_list(blockchain_ledger_gateway_v2:witness_hist(Witness)))
      }.

mk_gateway_witnesses({db, Witnesses}) ->
    SortedWitnesses = lists:keysort(1, maps:to_list(Witnesses)),
    lists:foldl(fun({Key, Witness}, Acc) ->
                        [mk_gateway_witness(Key, {db, Witness}) | Acc]
                end, [], SortedWitnesses);
mk_gateway_witnesses({chain, Witnesses}) ->
    SortedWitnesses = lists:keysort(1, maps:to_list(Witnesses)),
    lists:foldl(fun({Key, Witness}, Acc) ->
                        [mk_gateway_witness(Key, {chain, Witness}) | Acc]
                end, [], SortedWitnesses).

mk_gateway({db, {_Address, Owner, Location,
                 Alpha, Beta, Delta,
                 LastPocChallenge, LastPocOnionKeyHash,
                 Witnesses, Nonce}}) ->
    #gateway{
       owner = ?B58_TO_BIN(Owner),
       location = ?MAYBE_FN(fun(Bin) -> h3:from_string(binary_to_list(Bin)) end,  Location),
       alpha = Alpha,
       beta = Beta,
       delta = Delta,
       last_poc_challenge = ?MAYBE_UNDEFINED(LastPocChallenge),
       last_poc_onion_hash = ?MAYBE_UNDEFINED(LastPocOnionKeyHash),

       witnesses = mk_gateway_witnesses({db, Witnesses}),
       nonce = Nonce
      };
mk_gateway({chain, GW}) ->
    #gateway{
       owner = blockchain_ledger_gateway_v2:owner_address(GW),
       location = blockchain_ledger_gateway_v2:location(GW),
       alpha = blockchain_ledger_gateway_v2:alpha(GW),
       beta = blockchain_ledger_gateway_v2:beta(GW),
       delta = blockchain_ledger_gateway_v2:delta(GW),
       last_poc_challenge = blockchain_ledger_gateway_v2:last_poc_challenge(GW),
       last_poc_onion_hash = blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GW),

       witnesses = mk_gateway_witnesses({chain, blockchain_ledger_gateway_v2:witnesses(GW)}),
       nonce = blockchain_ledger_gateway_v2:nonce(GW)
      }.
