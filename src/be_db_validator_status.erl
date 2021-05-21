-module(be_db_validator_status).

-include("be_db_worker.hrl").
-include("be_db_follower.hrl").

-include_lib("blockchain/include/blockchain_vars.hrl").

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
    peer_online/1
]).

-export([adjust_request_rate/0]).

%% Status for all validators in the validator_inventory table is attempted
%% to be updated every 10 minutes (see query S1 below in
%% prepare_conn).
%%
%%  In order to spread out the refresh we calculate how many status
%%  updates we need to do per second (see calculate_request_rate/0)
%%  with a small speed margin to adjust for variable timing and fire
%%  those of in parallel.
%%
%% The request rate is automatically adapted to be higher as more
%% validators come online.
-define(STATUS_REFRESH_MINS, 10).
%% Maximum number of status updates per second to limit the number of
%% spawned updates.
-define(MAX_REQUEST_RATE, 200).

%%
%% Utility API
%%

peer_online(Address) ->
    PeerBook = libp2p_swarm:peerbook(blockchain_swarm:swarm()),
    Ledger = blockchain:ledger(),
    peer_online(Address, PeerBook, Ledger).

-spec request_rate() -> pos_integer().
request_rate() ->
    gen_server:call(?SERVER, request_rate).

adjust_request_rate() ->
    gen_server:cast(?SERVER, adjust_request_rate).

%%
%% be_db_worker
%%

-define(S_STATUS_UNKNOWN_LIST, "validator_status_unknown_list").
-define(S_STATUS_INSERT, "validator_status_insert").

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_STATUS_UNKNOWN_LIST,
            [
                "select v.address from validator_inventory v",
                "  left join validator_status s on s.address = v.address ",
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
                "insert into validator_status as status ",
                "(address, ",
                " online, ",
                " block, ",
                " peer_timestamp, ",
                " listen_addrs ",
                ") values ($1, $2, $3, to_timestamp($4::double precision / 1000), $5) ",
                "on conflict (address) do update ",
                "set ",
                "    online = EXCLUDED.online,",
                "    block = coalesce(EXCLUDED.block, status.block),"
                "    peer_timestamp = coalesce(EXCLUDED.peer_timestamp, status.peer_timestamp),",
                "    listen_addrs = EXCLUDED.listen_addrs;"
            ],
            []
        ),
    #{
        ?S_STATUS_UNKNOWN_LIST => S1,
        ?S_STATUS_INSERT => S2
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
    lager:info("Validator status starting with update rate: ~p per second", [RequestRate]),
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
        true ->
            ok;
        false ->
            lager:info("Validator status adjusting update rate to: ~p per second", [
                RequestRate
            ])
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
    {ok, _, [{Count}]} = ?EQUERY("select count(*) from validator_inventory", []),
    min(?MAX_REQUEST_RATE, max(1, round(Count / (?STATUS_REFRESH_MINS * 50)))).

request_status(B58Address, PeerBook, _Ledger, Requests) ->
    Request = fun() ->
        try
            true = ets:insert_new(Requests, {B58Address, self()}),
            Address = ?B58_TO_BIN(B58Address),
            Online = peer_online(Address),
            Block = be_peer_status:peer_metadata(<<"height">>, Address, PeerBook),
            PeerTime = be_peer_status:peer_time(Address, PeerBook),
            ListenAddrs = be_peer_status:peer_listen_addrs(Address, PeerBook),

            ?PREPARED_QUERY(
                ?S_STATUS_INSERT,
                [
                    B58Address,
                    Online,
                    Block,
                    PeerTime,
                    ListenAddrs
                ]
            )
        catch
            What:Why ->
                lager:info("Failed to update validator status for ~p: ~p", [
                    B58Address,
                    {What, Why}
                ])
        after
            ets:delete(Requests, B58Address)
        end
    end,
    spawn_link(Request).

%%
%% Custom Peer Status lookups
%%

-spec peer_online(
    libp2p_crypto:pubkey_bin(),
    libp2p_peerbook:peerbook(),
    blockchain:ledger()
) -> binary().
peer_online(Address, _PeerBook, Ledger) ->
    {ok, HBInterval} = blockchain:config(?validator_liveness_interval, Ledger),
    {ok, HBGrace} = blockchain:config(?validator_liveness_grace_period, Ledger),
    {ok, Validator} = blockchain_ledger_v1:get_validator(Address, Ledger),
    {ok, CurrentHeight} = blockchain_ledger_v1:current_height(Ledger),
    case
        (blockchain_ledger_validator_v1:last_heartbeat(Validator) + HBInterval + HBGrace) >=
            CurrentHeight
    of
        true -> <<"online">>;
        false -> <<"offline">>
    end.
