-module(be_db_gateway_status).

-include("be_db_worker.hrl").
-include("be_db_follower.hrl").

-behaviour(gen_server).

-beheviour(bh_db_worker).

%% gen_server
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% be_db_worker
-export([prepare_conn/1]).

%% online status utilities
-export([
    request_rate/0,
    peer_stale/1,
    peer_recent_challenger/1,
    peer_online/1,
    peer_height/1
]).

-export([adjust_request_rate/0]).

%% Status for all hotspots in the gateway_inventory table is attempted
%% to be updated every 10 minutes (see query S1 below in
%% prepare_conn).
%%
%%  In order to spread out the refresh we calculate how many status
%%  updates we need to do per second (see calculate_request_rate/0)
%%  with a small speed margin to adjust for variable timing and fire
%%  those of in parallel.
%%
%% The request rate is automatically adapted to be higher as more
%% hotspots come online.
-define(STATUS_REFRESH_MINS, 10).
%% Maximum number of status updates per second to limit the number of
%% spawned updates.
-define(MAX_REQUEST_RATE, 200).
%% A peer is considered stale if it's peerbook entry is more than an hour old
-define(STALE_PEER_TIME, 3600000).
%% A peer is recently added if it's (first) add_gateway transaction is in the
%% last "24 hours" in blocks (60 blocks per hour assumed)
-define(PEER_RECENTLY_ADDED_BLOCKS, 60 * 24).

%%
%% Utility API
%%

peer_recent_challenger(Address) ->
    Ledger = blockchain:ledger(),
    peer_recent_challenger(Address, Ledger).

peer_stale(Address) ->
    PeerBook = libp2p_swarm:peerbook(blockchain_swarm:swarm()),
    peer_stale(Address, PeerBook, true).

peer_online(Address) ->
    PeerBook = libp2p_swarm:peerbook(blockchain_swarm:swarm()),
    Ledger = blockchain:ledger(),
    peer_online(Address, PeerBook, Ledger).

peer_height(Address) ->
    PeerBook = libp2p_swarm:peerbook(blockchain_swarm:swarm()),
    peer_height(Address, PeerBook).

-spec request_rate() -> pos_integer().
request_rate() ->
    gen_server:call(?SERVER, request_rate).

adjust_request_rate() ->
    gen_server:cast(?SERVER, adjust_request_rate).

%%
%% be_db_worker
%%

-define(S_STATUS_UNKNOWN_LIST, "status_unknown_list").
-define(S_STATUS_INSERT, "status_insert").
-define(S_PEER_ADDED, "peer_added").

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_STATUS_UNKNOWN_LIST,
            [
                "select g.address from gateway_inventory g",
                "  left join gateway_status s on s.address = g.address ",
                "where coalesce(updated_at, to_timestamp(0)) ",
                "    < (now() - '",
                integer_to_list(?STATUS_REFRESH_MINS),
                " minute'::interval) ",
                "order by s.updated_at ",
                "limit $1"
            ],
            []
        ),
    {ok, S2} =
        epgsql:parse(
            Conn,
            ?S_STATUS_INSERT,
            [
                "insert into gateway_status as status ",
                "(address, ",
                " online, ",
                " poc_interval, ",
                " last_challenge, ",
                " block, ",
                " peer_timestamp "
                ") values ($1, $2, $3, $4, $5, to_timestamp($6::double precision / 1000)) ",
                "on conflict (address) do update ",
                "set ",
                "    online = EXCLUDED.online,",
                "    last_challenge = EXCLUDED.last_challenge,",
                "    poc_interval = EXCLUDED.poc_interval,",
                "    block = coalesce(EXCLUDED.block, status.block),"
                "    peer_timestamp = coalesce(EXCLUDED.peer_timestamp, status.peer_timestamp);"
            ],
            []
        ),
    {ok, S3} =
        epgsql:parse(
            Conn,
            ?S_PEER_ADDED,
            [
                "select block ",
                "from transaction_actors ",
                "where actor = $1 and actor_role = 'gateway' ",
                "order by block ",
                "limit 1"
            ],
            []
        ),
    #{
        ?S_STATUS_UNKNOWN_LIST => S1,
        ?S_STATUS_INSERT => S2,
        ?S_PEER_ADDED => S3
    }.

%%
%% gen_server
%%

-record(state, {
    request_rate :: pos_integer(),
    requests :: ets:tid()
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
    self() ! check_status,
    RequestRate = calculate_request_rate(),
    lager:info("Gateway status starting with update rate: ~p per second", [RequestRate]),
    {ok, #state{
        request_rate = RequestRate,
        requests = ets:new(?SERVER, [ordered_set, public, {write_concurrency, true}])
    }}.

handle_call(request_rate, _From, State = #state{request_rate = RequestRate}) ->
    {reply, RequestRate, State};
handle_call(Request, _From, State) ->
    lager:notice("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(adjust_request_rate, State = #state{}) ->
    {noreply, State#state{request_rate = calculate_request_rate()}};
handle_cast(Msg, State) ->
    lager:notice("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info(check_status, State = #state{requests = Requests}) ->
    %% If there are outstnading requests in the ets table we
    %% recalcualte the request rate since we're not keeping up.
    RequestRate =
        case ets:info(Requests, size) of
            0 -> State#state.request_rate;
            _ -> calculate_request_rate()
        end,
    case RequestRate == State#state.request_rate of
        true -> ok;
        false -> lager:info("Gateway status adjusting update rate to: ~p per second", [RequestRate])
    end,
    {ok, _, Results} = ?PREPARED_QUERY(?S_STATUS_UNKNOWN_LIST, [RequestRate]),

    %% Ignore already outstanding requests
    FilteredResults = lists:filter(
        fun({A}) ->
            length(ets:lookup(Requests, A)) == 0
        end,
        Results
    ),

    PeerBook = libp2p_swarm:peerbook(blockchain_swarm:swarm()),
    Ledger = blockchain:ledger(),
    lists:foreach(
        fun({A}) ->
            request_status(A, PeerBook, Ledger, Requests)
        end,
        FilteredResults
    ),
    erlang:send_after(timer:seconds(1), self(), check_status),
    {noreply, State#state{request_rate = RequestRate}};
handle_info(Info, State) ->
    lager:notice("Unhandled info ~p", [Info]),
    {noreply, State}.

calculate_request_rate() ->
    %% NOTE:We make time be 10s per minute faster to catch up to the
    %% per second updates.
    {ok, _, [{GWCount}]} = ?EQUERY("select count(*) from gateway_inventory", []),
    min(?MAX_REQUEST_RATE, round(GWCount / (?STATUS_REFRESH_MINS * 50))).

request_status(B58Address, PeerBook, Ledger, Requests) ->
    Request = fun() ->
        try
            true = ets:insert_new(Requests, {B58Address, self()}),
            Address = ?B58_TO_BIN(B58Address),
            Online = peer_online(Address),
            PoCInterval = blockchain_utils:challenge_interval(Ledger),
            LastChallenge = peer_last_challenge(Address, Ledger),
            Block = peer_metadata(<<"height">>, Address, PeerBook),
            PeerTime = peer_time(Address, PeerBook),

            ?PREPARED_QUERY(
                ?S_STATUS_INSERT,
                [B58Address, Online, PoCInterval, LastChallenge, Block, PeerTime]
            )
        catch
            What:Why ->
                lager:info("Failed to update gateway status for ~p: ~p", [B58Address, {What, Why}])
        after
            ets:delete(Requests, B58Address)
        end
    end,
    spawn_link(Request).

%%
%% Peer Status lookups
%%

-spec peer_online(libp2p_crypto:pubkey_bin(), libp2p_peerbook:peerbook(), blockchain:ledger()) ->
    binary().
peer_online(Address, PeerBook, Ledger) ->
    Ledger = blockchain:ledger(),
    case peer_recently_added(Address, Ledger) of
        true ->
            <<"online">>;
        false ->
            case peer_recent_challenger(Address, Ledger) of
                true ->
                    <<"online">>;
                false ->
                    PeerBook = libp2p_swarm:peerbook(blockchain_swarm:swarm()),
                    case peer_stale(Address, PeerBook, true) of
                        true -> <<"offline">>;
                        false -> <<"online">>
                    end
            end
    end.

-spec peer_recently_added(libp2p_crypto:pubkey_bin(), blockchain:ledger()) -> boolean().
peer_recently_added(Address, Ledger) ->
    {ok, Height} = blockchain_ledger_v1:current_height(Ledger),
    {ok, _, [{AddedHeight}]} = ?PREPARED_QUERY(?S_PEER_ADDED, [?BIN_TO_B58(Address)]),
    case Height - AddedHeight of
        V when V =< ?PEER_RECENTLY_ADDED_BLOCKS ->
            true;
        _ ->
            false
    end.

-spec peer_last_challenge(libp2p_crypto:pubkey_bin(), blockchain_ledger_v1:ledger()) ->
    undefined | pos_integer().
peer_last_challenge(Address, Ledger) ->
    case blockchain_ledger_v1:find_gateway_info(Address, Ledger) of
        {error, _} -> undefined;
        {ok, GWInfo} -> blockchain_ledger_gateway_v2:last_poc_challenge(GWInfo)
    end.

peer_recent_challenger(Address, Ledger) ->
    {ok, Height} = blockchain_ledger_v1:current_height(Ledger),
    PoCInterval = blockchain_utils:challenge_interval(Ledger),
    case peer_last_challenge(Address, Ledger) of
        undefined ->
            false;
        LastChallenge when LastChallenge >= (Height - (2 * PoCInterval)) ->
            true;
        _ ->
            false
    end.

peer_time(Address, PeerBook) ->
    case libp2p_peerbook:get(PeerBook, Address) of
        {ok, Peer} ->
            libp2p_peer:timestamp(Peer);
        {error, _} ->
            undefined
    end.

peer_metadata(Key, Address, PeerBook) ->
    case libp2p_peerbook:get(PeerBook, Address) of
        {ok, Peer} ->
            libp2p_peer:signed_metadata_get(Peer, Key, undefined);
        {error, _} ->
            undefined
    end.

peer_height(Address, PeerBook) ->
    case peer_metadata(<<"height">>, Address, PeerBook) of
        undefined ->
            undefined;
        Height when is_integer(Height) ->
            Height;
        Other ->
            lager:warning("Invalid block height for gateway ~s: ~p", [?BIN_TO_B58(Address), Other]),
            undefined
    end.

peer_stale(Address, PeerBook, Refresh) ->
    case libp2p_peerbook:get(PeerBook, Address) of
        {ok, Peer} ->
            case libp2p_peer:is_stale(Peer, ?STALE_PEER_TIME) of
                true when Refresh ->
                    libp2p_peerbook:refresh(PeerBook, Address),
                    %% ARP should be quick so give it a short while
                    timer:sleep(100),
                    peer_stale(Address, PeerBook, false);
                Stale ->
                    Stale
            end;
        {error, _} when Refresh ->
            libp2p_peerbook:refresh(PeerBook, Address),
            %% ARP should be quick so give it a short while
            timer:sleep(100),
            peer_stale(Address, PeerBook, false);
        _ ->
            true
    end.
