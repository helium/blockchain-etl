-module(be_db_backfill).

-include("be_db_worker.hrl").
-include("be_db_follower.hrl").

-export([
    receipts_challenger/4,
    reversed_receipts_path/3,
    gateway_names/0,
    oui_subnets/0,
    location_geometry/0,
    reward_gateways/2,
    gateway_location_hex/0,
    dc_burn/2,
    oracle_price_at/1
]).

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
                            {ok, 1} =
                                ?EQUERY(?UPDATE_RECEIPTS_TXN, [Height, TxnHashStr, Fields]),
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

%%
%% Fill in gateway names if they're null
%%

gateway_names() ->
    {ok, _, NoNames} =
        ?EQUERY(["select address from gateway_inventory where name is null"], []),

    lists:foreach(
        fun({Addr}) ->
            {ok, Name} = erl_angry_purple_tiger:animal_name(Addr),
            {ok, _} =
                ?EQUERY("update gateway_inventory set name = $2 where address = $1", [
                    Addr,
                    Name
                ])
        end,
        NoNames
    ),
    length(NoNames).

%%
%% Backfill oui entries
%%

oui_subnets() ->
    Chain = blockchain_worker:blockchain(),
    Ledger = blockchain:ledger(Chain),

    {ok, Routes} = blockchain_ledger_v1:get_routes(Ledger),
    lists:foreach(
        fun(Route) ->
            Oui = blockchain_ledger_routing_v1:oui(Route),
            Owner = ?BIN_TO_B58(blockchain_ledger_routing_v1:owner(Route)),
            Nonce = blockchain_ledger_routing_v1:nonce(Route),
            Subnets = [
                be_db_oui:subnet_to_list(S)
             || S <- blockchain_ledger_routing_v1:subnets(Route)
            ],
            Addresses = [?BIN_TO_B58(A) || A <- blockchain_ledger_routing_v1:addresses(Route)],
            {ok, _} =
                ?EQUERY(
                    "update oui_inventory set subnets = $2, nonce = $3, owner = $4, addresses = $5 where oui = $1",
                    [
                        Oui,
                        Subnets,
                        Nonce,
                        Owner,
                        Addresses
                    ]
                )
        end,
        Routes
    ),
    length(Routes).

%%
%% Backfill location geometry
%%

location_geometry() ->
    {ok, _, Locations} = ?EQUERY("select location from locations", []),
    lists:foreach(
        fun({Location}) ->
            {Lat, Lon} = h3:to_geo(h3:from_string(binary_to_list(Location))),
            {ok, _} = ?EQUERY(
                "update locations set geometry = ST_SetSRID(ST_MakePoint($2, $3), 4326) where location = $1",
                [Location, Lon, Lat]
            )
        end,
        Locations
    ),
    length(Locations).

%%
%% Backfill reward_gateway
%%

-define(INSERT_REWARD_GATEWAYS, [
    "with data as ( ",
    "select ",
    "    block, ",
    "    hash, ",
    "    (r.reward->'gateway')::text as address ",
    "from transactions t1 ",
    "left join jsonb_array_elements(t1.fields->'rewards') as r(reward) on true ",
    "where type = 'rewards_v1'  ",
    "and block between $1 and $2 ",
    ") ",
    "insert into transaction_actors (actor, actor_role, transaction_hash, block) ",
    "    select address, 'reward_gateway', hash, block from data ",
    "on conflict do nothing"
]).

reward_gateways(MinBlock, MaxBlock) ->
    {ok, Inserted} = ?EQUERY(?INSERT_REWARD_GATEWAYS, [MinBlock, MaxBlock]),
    Inserted.

%%
%% Fill in gateway location_hes if they're null
%%

gateway_location_hex() ->
    {ok, _, NoLocs} =
        ?EQUERY(
            [
                "select address, location from gateway_inventory where location is not null and location_hex is null"
            ],
            []
        ),

    lists:foreach(
        fun({Addr, LocationBin}) ->
            Location = h3:from_string(binary_to_list(LocationBin)),
            {ok, _} =
                ?EQUERY("update gateway_inventory set location_hex = $2 where address = $1", [
                    Addr,
                    ?MAYBE_H3(?MAYBE_FN(fun be_db_gateway:calculate_location_hex/1, Location))
                ])
        end,
        NoLocs
    ),
    length(NoLocs).

%%
%% Backfill dc_burn
%%

-define(INSERT_DC_BURN, [
    "insert into dc_burns (block, time, transaction_hash, actor, type, amount, oracle_price) ",
    "values ($1, $2, $3, $4, $5, $6, $7) ",
    "on conflict do nothing"
]).

-define(ORACLE_PRICE_AT, [
    "select p.block, p.price ",
    "from oracle_prices p ",
    "where p.block <= $1 ",
    "order by p.block desc limit 1"
]).

oracle_price_at(Height) ->
    case ?EQUERY(?ORACLE_PRICE_AT, [Height]) of
        {ok, _, [{_, Price}]} -> Price;
        {ok, _, []} -> undefined
    end.

dc_burn(MinBlock, MaxBlock) ->
    Chain = blockchain_worker:blockchain(),
    Ledger = blockchain:ledger(Chain),

    lager:info("backfill starting dc_burns(~p, ~p)", [MinBlock, MaxBlock]),
    Inserted = lists:sum(
        blockchain_utils:pmap(
            fun(Height) ->
                {ok, Block} = blockchain:get_block(Height, Chain),
                OraclePrice = oracle_price_at(Height),
                Burns = be_db_dc_burn:collect_burns(Block, OraclePrice, Ledger),
                lists:sum(
                    blockchain_utils:pmap(
                        fun(Burn) ->
                            {ok, N} = ?EQUERY(?INSERT_DC_BURN, tuple_to_list(Burn)),
                            N
                        end,
                        Burns
                    )
                )
            end,
            lists:seq(MinBlock, MaxBlock)
        )
    ),
    lager:info("backfill complete: dc_burns(~p, ~p)", [MinBlock, MaxBlock]),
    Inserted.
