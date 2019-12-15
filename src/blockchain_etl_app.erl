-module(blockchain_etl_app).

-behaviour(application).
-export([start/2, stop/1]).

-spec database_opts() -> [{atom(), any()}].
database_opts() ->
    case os:getenv("DATABASE_URL") of
        false ->
            {error, missing_database_url};
        URL ->
            PoolSize = list_to_integer(os:getenv("DATABASE_POOL_SIZE", "10")),
            ParseOpts = [{scheme_defaults, [{postgres, 5432}]}],
            case http_uri:parse(URL, ParseOpts) of
                {error, Error} ->
                    {error, Error};
                {ok, {postgres, UserPass, Host, Port, Database, _}} ->
                    [User, Pass] = string:split(UserPass, ":"),
                    {ok, [
                          {size, PoolSize},
                          {host, Host},
                          {port, Port},
                          {database, string:slice(Database, 1)},
                          {username, User},
                          {password, Pass}
                         ]};
                {ok, {Schema, _, _, _, _, _}} ->
                    {error, {unsupported_schema, Schema}}
            end
    end.


start(_StartType, _StartArgs) ->
    case database_opts() of
        {ok, Opts} ->
            be_cli_registry:register_cli(),
            _ = pgapp:connect(Opts),
            be_sup:start_link();
        {error, Error} ->
            {error, Error}
    end.

stop(_State) ->
    ok.
