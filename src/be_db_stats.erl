-module(be_db_stats).

-include("be_db_worker.hrl").
-include("be_db_follower.hrl").

-beheviour(bh_db_worker).

-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-record(state, {
    stats = #{} :: #{Name :: binary() => Value :: integer()}
}).

%% %%
%% %% be_db_worker
%% %%

-define(S_STATS_LOAD, "stats_load").
-define(S_STATS_INSERT, "stats_insert").

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_STATS_LOAD,
            "select name, value from stats_inventory",
            []
        ),

    {ok, S2} =
        epgsql:parse(
            Conn,
            ?S_STATS_INSERT,
            [
                "insert into stats_inventory (name, value) values ($1, $2) ",
                "on conflict (name) do update ",
                "set ",
                "    value = EXCLUDED.value;"
            ],
            []
        ),

    #{
        ?S_STATS_LOAD => S1,
        ?S_STATS_INSERT => S2
    }.

%%

%% be_block_handler
%%

init(_) ->
    {ok, _, Stats} = ?PREPARED_QUERY(?S_STATS_LOAD, []),
    lager:info("Loaded ~p stats", [length(Stats)]),
    {ok, #state{stats = maps:from_list(Stats)}}.

load_block(Conn, _Hash, Block, _Sync, _Ledger, State = #state{}) ->
    {Stats, Queries} =
        lists:foldl(
            fun(Name, {Stats, Acc}) ->
                Current = maps:get(Name, Stats, 0),
                case update(Name, Current, Block) of
                    Current ->
                        {Stats, Acc};
                    New ->
                        {Stats#{Name => New}, q_insert_stat(Name, New, Acc)}
                end
            end,
            {State#state.stats, []},
            [
                <<"blocks">>,
                <<"transactions">>,
                <<"hotspots">>,
                <<"countries">>,
                <<"cities">>,
                <<"consensus_groups">>,
                <<"challenges">>,
                <<"validators">>,
                <<"ouis">>
            ]
        ),

    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State#state{stats = Stats}}.

-spec update(Name :: binary(), Current :: integer(), Block :: blockchain_block:block()) ->
    New :: integer().
update(<<"blocks">>, _Current, Block) ->
    blockchain_block:height(Block);
update(<<"transactions">>, Current, Block) ->
    Current + length(blockchain_block:transactions(Block));
update(<<"hotspots">>, Current, Block) ->
    case
        lists:any(
            fun(Txn) ->
                blockchain_txn:type(Txn) == blockchain_txn_add_gateway_v1
            end,
            blockchain_block:transactions(Block)
        )
    of
        true ->
            %% We still just go ask the source for the actual value
            {ok, _, [{Count}]} = ?EQUERY("select count(*) from gateway_inventory", []),
            Count;
        false ->
            Current
    end;
update(<<"cities">>, Current, Block) ->
    case
        lists:any(
            fun(Txn) ->
                blockchain_txn:type(Txn) == blockchain_txn_assert_location_v1 orelse
                    blockchain_txn:type(Txn) == blockchain_txn_assert_location_v2
            end,
            blockchain_block:transactions(Block)
        )
    of
        true ->
            %% We still just go ask the source for the actual value
            {ok, _, [{Count}]} = ?EQUERY(
                "select count(distinct long_city) "
                "from gateway_inventory g inner join locations l on g.location = l.location "
                "where l.location is not null;",
                []
            ),
            Count;
        false ->
            Current
    end;
update(<<"countries">>, Current, Block) ->
    case
        lists:any(
            fun(Txn) ->
                blockchain_txn:type(Txn) == blockchain_txn_assert_location_v1 orelse
                    blockchain_txn:type(Txn) == blockchain_txn_assert_location_v2
            end,
            blockchain_block:transactions(Block)
        )
    of
        true ->
            %% We still just go ask the source for the actual value
            {ok, _, [{Count}]} = ?EQUERY(
                "select count(distinct long_country) "
                "from gateway_inventory g inner join locations l on g.location = l.location "
                "where l.location is not null;",
                []
            ),
            Count;
        false ->
            Current
    end;
update(<<"consensus_groups">>, Current, Block) ->
    Txns = lists:filter(
        fun(Txn) ->
            blockchain_txn:type(Txn) == blockchain_txn_consensus_group_v1
        end,
        blockchain_block:transactions(Block)
    ),
    Current + length(Txns);
update(<<"challenges">>, Current, Block) ->
    Txns = lists:filter(
        fun(Txn) ->
            blockchain_txn:type(Txn) == blockchain_txn_poc_receipts_v1
        end,
        blockchain_block:transactions(Block)
    ),
    Current + length(Txns);
update(<<"validators">>, _Current, _Block) ->
    {ok, _, [{Count}]} = ?EQUERY(
        "select count(*) "
        "from validator_inventory v inner join validator_status s on v.address = s.address "
        "where s.online = 'online';",
        []
    ),
    Count;
update(<<"ouis">>, _Current, _Block) ->
    {ok, _, [{Count}]} = ?EQUERY(
        "select count(*) "
        "from oui_inventory;",
        []
    ),
    Count.

q_insert_stat(Name, Value, Acc) ->
    [{?S_STATS_INSERT, [Name, Value]} | Acc].
