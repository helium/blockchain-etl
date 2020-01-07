-module(be_gateway).

-include("be_block_handler.hrl").

-export([init/1, load/4]).

-behavior(be_block_handler).

-type gateway_hash() :: {BaseHash::binary(), WitnessHash::binary()}.

-record(state,
       {
        conn :: epgsql:connection(),
        s_insert_gateawy :: epgsql:statement(),

        gateways=#{} :: #{libp2p_crypto:pubkey_bin() => gateway_hash()}
       }).

-define(Q_INSERT_GATEWAY, "insert_gateway").

init(Conn) ->
    {ok, InsertGateway} =
        epgsql:parse(Conn, ?Q_INSERT_GATEWAY,
                     "insert into gateways (block, address, owner, location, alpha, beta, delta, last_poc_challenge, last_poc_onion_key_hash, witnesses) values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);", []),
    {ok, #state{
            conn=Conn,
            s_insert_gateawy=InsertGateway
           }}.

-spec mk_gateway_hash(blockchain_ledger_gateway_v2:gateway()) -> gateway_hash().
mk_gateway_hash(GW) ->
    GWBaseBin = blockchain_ledger_gateway_v2:serialize(blockchain_ledger_gateway_v2:clear_witnesses(GW)),
    GWWitnesses = lists:keysort(1, maps:to_list(blockchain_ledger_gateway_v2:witnesses(GW))),
    GWWitnessBin = erlang:term_to_binary(GWWitnesses),
    {crypto:hash(sha256, GWBaseBin), crypto:hash(sha256, GWWitnessBin)}.

load(_Hash, Block, Ledger, State=#state{}) ->
    Active = blockchain_ledger_v1:active_gateways(Ledger),
    BlockHeight = blockchain_block_v1:height(Block),
    {Queries, Hashes} =
        maps:fold(fun(Key, GW, {Queries, Hashes}) ->
                          GWHash = mk_gateway_hash(GW),
                          case maps:get(Key, Hashes, false) of
                              GWHash ->
                                  %% Found and not changed
                                  {Queries, Hashes};
                              _ ->
                                  %% Changed or new
                                  NewQueries = q_insert_gateway(BlockHeight, Key, GW, Queries, State),
                                  {NewQueries, Hashes#{Key => GWHash}}
                          end
                  end, {[], State#state.gateways}, Active),
    %% Seperate the queries to avoid the batches getting too big
    be_block_handler:run_queries(Queries, State#state.conn, State#state{gateways=Hashes}).

q_insert_gateway(BlockHeight, Address, GW, Queries, #state{s_insert_gateawy=Stmt}) ->
    Params =
        [BlockHeight,
         ?BIN_TO_B58(Address),
         ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(GW)),
         ?NULL_OR_H3(blockchain_ledger_gateway_v2:location(GW)),
         blockchain_ledger_gateway_v2:alpha(GW),
         blockchain_ledger_gateway_v2:beta(GW),
         blockchain_ledger_gateway_v2:delta(GW),
         ?NULL_OR_VALUE(blockchain_ledger_gateway_v2:last_poc_challenge(GW)),
         ?NULL_OR_B64(blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GW)),
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
      <<"first_time">> =>  ?NULL_OR_VALUE(blockchain_ledger_gateway_v2:witness_first_time(Witness)),
      <<"recent_time">> => ?NULL_OR_VALUE(blockchain_ledger_gateway_v2:witness_recent_time(Witness))
     }.
