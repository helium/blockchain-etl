-module(be_sup).

-include("be_db_worker.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(SUP(I, Args), #{
    id => I,
    start => {I, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => supervisor,
    modules => [I]
}).

-define(WORKER(I, Args), ?WORKER(I, I, Args)).
-define(WORKER(I, M, Args), #{
    id => I,
    start => {M, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [M]
}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    erlang:system_flag(fullsweep_after, 0),

    ok = libp2p_crypto:set_network(application:get_env(blockchain_etl, network, mainnet)),

    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    SeedNodes =
        case application:get_env(blockchain, seed_nodes) of
            {ok, ""} -> [];
            {ok, Seeds} -> string:split(Seeds, ",", all);
            _ -> []
        end,

    BaseDir = application:get_env(blockchain, base_dir, "data"),
    SwarmKey = filename:join([BaseDir, "blockchain_etl", "swarm_key"]),
    ok = filelib:ensure_dir(SwarmKey),
    {PublicKey, ECDHFun, SigFun} =
        case libp2p_crypto:load_keys(SwarmKey) of
            {ok, #{secret := PrivKey0, public := PubKey}} ->
                {PubKey, libp2p_crypto:mk_ecdh_fun(PrivKey0),
                    libp2p_crypto:mk_sig_fun(PrivKey0)};
            {error, enoent} ->
                KeyMap =
                    #{secret := PrivKey0, public := PubKey} = libp2p_crypto:generate_keys(
                        ecc_compact
                    ),
                ok = libp2p_crypto:save_keys(KeyMap, SwarmKey),
                {PubKey, libp2p_crypto:mk_ecdh_fun(PrivKey0),
                    libp2p_crypto:mk_sig_fun(PrivKey0)}
        end,
    SeedNodeDNS = application:get_env(blockchain, seed_node_dns, []),
    SeedAddresses = string:tokens(
        lists:flatten([
            string:prefix(X, "blockchain-seed-nodes=")
            || [X] <- inet_res:lookup(SeedNodeDNS, in, txt),
               string:prefix(X, "blockchain-seed-nodes=") /= nomatch
        ]),
        ","
    ),

    Port = application:get_env(blockchain, port, 0),
    MaxInboundConnections = application:get_env(blockchain, max_inbound_connections, 10),
    BlockchainOpts = [
        {key, {PublicKey, SigFun, ECDHFun}},
        {seed_nodes, SeedNodes ++ SeedAddresses},
        {max_inbound_connections, MaxInboundConnections},
        {port, Port},
        {update_dir, "update"},
        {base_dir, BaseDir}
    ],
    {ok, DBOpts} = application:get_env(blockchain_etl, db),

    PoolOpts = proplists:get_value(pool, DBOpts),
    DBEnv = proplists:get_value(env, DBOpts),
    DBHandlers = proplists:get_value(handlers, DBOpts),
    {ok, DBConnectionOpts} = psql_migration:connection_opts([{env, DBEnv}]),

    {ok,
        {SupFlags, [
            ?SUP(blockchain_sup, [BlockchainOpts]),
            poolboy:child_spec(
                ?DB_POOL,
                [
                    {name, {local, ?DB_POOL}},
                    {worker_module, be_db_worker}
                ] ++ PoolOpts,
                [
                    {db_opts, DBConnectionOpts},
                    {db_handlers, DBHandlers}
                ]
            ),
            ?WORKER(db_follower, blockchain_follower, [
                [{follower_module, {be_db_follower, [{base_dir, BaseDir}]}}]
            ]),
            ?WORKER(be_db_pending_txn, []),
            ?WORKER(be_db_geocoder, []),
            ?WORKER(be_db_gateway_status, []),
            ?WORKER(be_db_validator_status, [])
        ]}}.
