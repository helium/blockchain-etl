-module(be_db_geocoder).

-include("be_db_worker.hrl").

-behaviour(gen_server).

-beheviour(bh_db_worker).

%% gen_server
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% be_db_worker
-export([prepare_conn/1]).

%% utilities
-export([request_geocode/2, parse_geocode_results/1]).

-define(DEFAULT_LOCATION_TIME, 10000).

%%
%% be_db_worker
%%

-define(S_UNKNOWN_LOCATION_LIST, "location_unknown_list").
-define(S_LOCATION_INSERT, "location_insert").

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_UNKNOWN_LOCATION_LIST,
            "select g.location from gateway_inventory g where not exists ( select from locations l where l.location = g.location ) and g.location is not null limit 100",
            []
        ),

    {ok, S2} =
        epgsql:parse(
            Conn,
            ?S_LOCATION_INSERT,
            "insert into  locations (location, short_street, long_street, short_city, long_city, short_state, long_state, short_country, long_country) values ($1, $2, $3, $4, $5, $6, $7, $8, $9) on conflict do nothing",
            []
        ),

    #{
        ?S_UNKNOWN_LOCATION_LIST => S1,
        ?S_LOCATION_INSERT => S2
    }.

%%
%% gen_server
%%

-record(state, {
    requests = #{} :: #{reference() => Loc :: h3:h3index()}
}).

-record(request, {
    data :: binary(),
    location :: binary()
}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    case os:getenv("GOOGLE_MAPS_API_KEY") of
        false ->
            %% If no API key don't bother to start checking
            ok;
        _ ->
            self() ! check_locations
    end,
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    lager:notice("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:notice("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info(check_locations, State = #state{requests = Requests}) ->
    {ok, _, Results} = ?PREPARED_QUERY(?S_UNKNOWN_LOCATION_LIST, []),
    %% Ignore already outstanding requests
    FilteredResults = lists:filter(fun({L}) -> not maps:is_key(L, Requests) end, Results),
    NewReqs = lists:foldl(
        fun({L}, Acc) ->
            case request_geocode(L, [async]) of
                {ok, Ref} ->
                    Acc#{Ref => #request{location = L, data = <<>>}};
                {error, Error} ->
                    lager:warning("Failed to request geocode: ~p", [Error]),
                    Acc
            end
        end,
        Requests,
        FilteredResults
    ),
    case length(FilteredResults) > 0 of
        true ->
            lager:info(
                "Added geocoder requests: ~p (total ~p)",
                [length(FilteredResults), maps:size(NewReqs)]
            );
        false ->
            ok
    end,
    {noreply, State#state{requests = maybe_check_locations(NewReqs)}};
handle_info({hackney_response, Ref, {status, Status, Reason}}, State = #state{}) when
    Status /= 200
->
    take_request(
        fun(#request{location = Loc}) ->
            lager:warning(
                "Request for geocode for h3 index: ~s failed: ~p reason: ~p",
                [Loc, Status, Reason]
            )
        end,
        Ref,
        State
    );
handle_info({hackney_response, Ref, {error, Error}}, State = #state{}) ->
    take_request(
        fun(#request{location = Loc}) ->
            lager:warning("Request for geocode for h3 index: ~s error: ~p", [Loc, Error])
        end,
        Ref,
        State
    );
handle_info({hackney_response, Ref, Bin}, State = #state{requests = Requests}) when
    is_binary(Bin)
->
    case maps:get(Ref, Requests, false) of
        false ->
            {noreply, State};
        Req = #request{data = Data} ->
            {noreply, State#state{
                requests = maps:update(
                    Ref,
                    Req#request{data = <<Data/binary, Bin/binary>>},
                    Requests
                )
            }}
    end;
handle_info({hackney_response, Ref, done}, State = #state{}) ->
    take_request(
        fun(#request{location = Loc, data = Bin}) ->
            try
                case parse_geocode_results(jsone:decode(Bin)) of
                    {ok, Results} ->
                        {ok, _} = store_geocode_result(Loc, Results);
                    {error, Error} ->
                        lager:info("Failed to get geocode for ~p: ~p", [Loc, Error])
                end
            catch
                What:Why ->
                    lager:info("Failed to get geocode for ~p: ~p", [Loc, {What, Why}])
            end
        end,
        Ref,
        State
    );
handle_info({hackney_response, _, _}, State = #state{}) ->
    %% Ignore all other resposnes
    {noreply, State};
handle_info(Info, State) ->
    lager:notice("Unhandled info ~p", [Info]),
    {noreply, State}.

take_request(Fun, Ref, State = #state{}) ->
    case maps:take(Ref, State#state.requests) of
        error ->
            {noreply, State};
        {Req, NewReqs} ->
            Fun(Req),
            {noreply, State#state{requests = maybe_check_locations(NewReqs)}}
    end.

maybe_check_locations(Requests) when map_size(Requests) == 0 ->
    erlang:send_after(?DEFAULT_LOCATION_TIME, self(), check_locations),
    Requests;
maybe_check_locations(Requests) ->
    Requests.

store_geocode_result(Loc, #{
    street := {ShortStreet, LongStreet},
    city := {ShortCity, LongCity},
    state := {ShortState, LongState},
    country := {ShortCountry, LongCountry}
}) ->
    ?PREPARED_QUERY(?S_LOCATION_INSERT, [
        Loc,
        ShortStreet,
        LongStreet,
        ShortCity,
        LongCity,
        ShortState,
        LongState,
        ShortCountry,
        LongCountry
    ]).

-spec request_geocode(h3:h3index() | binary(), [any()]) ->
    {ok, Status :: integer(), Headers :: list(), hackney:client_ref()} |
    {ok, Status :: integer(), Headers :: list()} |
    {ok, hackney:client_ref()} |
    {error, term()}.
request_geocode(BinLoc, Opts) when is_binary(BinLoc) ->
    request_geocode(h3:from_string(binary_to_list(BinLoc)), Opts);
request_geocode(Location, Opts) ->
    case os:getenv("GOOGLE_MAPS_API_KEY") of
        false ->
            %% If no API key don't bother to start checking
            {error, no_api_key};
        Key ->
            {Lat, Lon} = h3:to_geo(Location),
            URL =
                <<"https://maps.googleapis.com/maps/api/geocode/json?latlng=",
                    (float_to_binary(Lat, [{decimals, 20}]))/binary, ",",
                    (float_to_binary(Lon, [{decimals, 20}]))/binary, "&key=",
                    (list_to_binary(Key))/binary>>,
            hackney:get(URL, [], <<>>, Opts)
    end.

-spec parse_geocode_results(map()) -> {ok, map()} | {error, term()}.
parse_geocode_results(#{<<"results">> := [], <<"status">> := <<"ZERO_RESULTS">>}) ->
    %% ZERO_RESULT means the request was valid but nothing was
    %% found. Return all unknowns.
    parse_geocode_results(#{
        <<"results">> => [#{<<"address_components">> => []}]
    });
parse_geocode_results(#{<<"results">> := [], <<"status">> := Status}) ->
    {error, {status, Status}};
parse_geocode_results(#{<<"results">> := [#{<<"address_components">> := Components} | _Tail]}) ->
    %% We currently only examine the first set of returned components
    {ok, #{
        street => find_types(
            [<<"route">>, <<"sublocality_level_1">>],
            Components
        ),
        city => find_types(
            [
                <<"locality">>,
                <<"sublocality">>,
                <<"postal_town">>,
                <<"administrative_area_level_3">>
            ],
            Components
        ),
        state => find_types([<<"administrative_area_level_1">>], Components),
        country => find_types([<<"country">>], Components)
    }}.

find_types([], _Components) ->
    {undefined, undefined};
find_types([Type | Tail], Components) ->
    %% Look for an entry in the components list that has the given
    %% Type in it's types list. If found extract and return the short
    %% and long name versions.
    case
        lists:filtermap(
            fun(#{<<"types">> := Types} = Entry) ->
                case lists:member(Type, Types) of
                    false ->
                        false;
                    true ->
                        {true,
                            {maps:get(<<"short_name">>, Entry, undefined),
                                maps:get(<<"long_name">>, Entry, undefined)}}
                end
            end,
            Components
        )
    of
        [] -> find_types(Tail, Components);
        [V | _T] -> V
    end.
