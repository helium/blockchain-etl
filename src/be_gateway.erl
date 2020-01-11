-module(be_gateway).

-include("be_block_handler.hrl").

-export([init/1, load/4]).

-behavior(be_block_handler).

-type gateway_cache() :: #{libp2p_crypto:pubkey_bin() => binary()}.

-record(state,
       {
        conn :: epgsql:connection(),
        s_insert_gateawy :: epgsql:statement(),

        gateways=#{} :: gateway_cache()
       }).

-define(Q_INSERT_GATEWAY, "insert_gateway").

init(Conn) ->
    {ok, InsertGateway} =
        epgsql:parse(Conn, ?Q_INSERT_GATEWAY,
                     "insert into gateways (block, address, owner, location, alpha, beta, delta, last_poc_challenge, last_poc_onion_key_hash, witnesses) values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);", []),


    {ok, #state{
            conn=Conn,
            gateways=init_gateway_cache(Conn),
            s_insert_gateawy=InsertGateway
           }}.

load(_Hash, Block, Ledger, State=#state{}) ->
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
                                  NewQueries = q_insert_gateway(BlockHeight, Key, GW, Queries, State),
                                  {NewQueries, Hashes#{Key => NewHash}}
                          end
                  end, {[], State#state.gateways}, Active),
    %% Seperate the queries to avoid the batches getting too big
    be_block_handler:run_queries(Queries, State#state.conn, State#state{gateways=Hashes}).

q_insert_gateway(BlockHeight, Address, GW, Queries, #state{s_insert_gateawy=Stmt}) ->
    Params =
        [BlockHeight,
         ?BIN_TO_B58(Address),
         ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(GW)),
         ?MAYBE_H3(blockchain_ledger_gateway_v2:location(GW)),
         blockchain_ledger_gateway_v2:alpha(GW),
         blockchain_ledger_gateway_v2:beta(GW),
         blockchain_ledger_gateway_v2:delta(GW),
         ?MAYBE_UNDEFINED(blockchain_ledger_gateway_v2:last_poc_challenge(GW)),
         ?MAYBE_B64(blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GW)),
         witnesses_to_json(blockchain_ledger_gateway_v2:witnesses(GW))
        ],
    [{Stmt, Params} | Queries].

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

-spec init_gateway_cache(epgsql:connection()) -> gateway_cache().
init_gateway_cache(Conn) ->
    lager:info("Constructing gateway cache"),
    {ok, _, GWList} =
        epgsql:equery(Conn,
                      "select address, owner, location, alpha, beta, delta, last_poc_challenge, last_poc_onion_key_hash, witnesses from gateways where (block, address) in  (select max(block) as block, address from gateways group by address)"
                     ),
    Result = maps:from_list(lists:map(fun(Entry={Address,
                                                 _Owner, _Location,
                                                 _Alpha, _Beta, _Delta,
                                                 _LastPocChallenge, _LastPocOnionKeyHash,
                                                 _Witnesses}) ->
                                              GWHash = mk_gateway_hash({db, Entry}),
                                              {?B58_TO_BIN(Address), GWHash}
                                      end, GWList)),
    lager:info("Constructed gateway cache: ~p entries", [map_size(Result)]),
    Result.



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
         witnesses :: [#gateway_witness{}]
        }).

mk_gateway_witness(Key, {db, #{
                               <<"first_time">> := FirstTime,
                               <<"recent_time">> := RecentTime,
                               <<"histogram">> := Histogram
                              }}) ->
    FixedHistogram = lists:keysort(1, lists:map(fun({K, V}) ->
                                                        {binary_to_integer(K), V}
                                                end, Histogram)),
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
                 Witnesses}}) ->
    #gateway{
       owner = ?B58_TO_BIN(Owner),
       location = ?MAYBE_FN(fun(Bin) -> h3:from_string(binary_to_list(Bin)) end,  Location),
       alpha = Alpha,
       beta = Beta,
       delta = Delta,
       last_poc_challenge = ?MAYBE_UNDEFINED(LastPocChallenge),
       last_poc_onion_hash = ?MAYBE_UNDEFINED(LastPocOnionKeyHash),

       witnesses = mk_gateway_witnesses({db, Witnesses})
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

       witnesses = mk_gateway_witnesses({chain, blockchain_ledger_gateway_v2:witnesses(GW)})
      }.
