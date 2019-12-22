-module(be_follower).

-behaviour(gen_server).

%% API
-export([start_link/0, counts/0, pause/0, resume/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
         chain :: blockchain:chain(),
         db_conn :: epgsql:connection() | undefined,
         db_state :: be_block:state() | undefined,
         load_pid=undefined :: {pid(), reference()} | undefined,
         state=normal :: any()
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

pause() ->
    gen_server:call(?SERVER, pause).

resume() ->
    gen_server:call(?SERVER, resume).

counts() ->
    {ok, Conn} = psql_migration:open_connection(),
    {ok, ChainHeight} = blockchain:height(blockchain_worker:blockchain()),
    Stats = #{
              db_height => row_count("blocks", Conn),
              chain_height => ChainHeight,
              transaction_count => row_count("transactions", Conn),
              actor_count => row_count("transaction_actors", Conn),
              block_sig_count => row_count("block_signatures", Conn)
             },
    epgsql:close(Conn),
    Stats.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {Conn, ConnState} = connect_database(),
    self() ! chain_check,
    {ok, #state{db_state=ConnState, db_conn=Conn}}.

handle_call(pause, _From, State=#state{db_conn=undefined}) ->
    {reply, ok, State};
handle_call(pause, _From, State=#state{}) ->
    epgsql:close(State#state.db_conn),
    lager:info("paused"),
    {reply, ok, State#state{state=paused, db_state=undefined, db_conn=undefined}};
handle_call(resume, _From, State=#state{db_conn=undefined}) ->
    {Conn, ConnState} = connect_database(),
    self() ! chain_load,
    lager:info("resumed"),
    {reply, ok, State#state{state=normal, db_state=ConnState, db_conn=Conn}};
handle_call(resume, _From, State=#state{}) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({blockchain_event, {add_block, _Height, _Sync, _Ledger}}, State=#state{load_pid=undefined}) ->
    %% Trigger a chain load if we're not already working on it
    self() ! chain_load,
    {noreply, State};
handle_info({blockchain_event, {add_block, _Height, _Sync, _Ledger}}, State=#state{}) ->
    {noreply, State};

%% Check height of chain looking for new inserts
handle_info(chain_load, State=#state{chain=undefined}) ->
    {noreply, State};
handle_info(chain_load, State=#state{state=LoadState}) when LoadState /= normal ->
    {noreply, State};
handle_info(chain_load, State=#state{db_state=Conn}) ->
    {ok, FollowHeight} = be_block:current_height(Conn),
    handle_info({chain_load, FollowHeight}, State);

handle_info({chain_load, _}, State=#state{state=LoadState}) when LoadState /= normal ->
    {noreply, State};
handle_info({chain_load, FollowHeight}, State=#state{chain=Chain, db_state=Conn}) ->
    {ok, ChainHeight} = blockchain:height(Chain),
    case ChainHeight > FollowHeight of
        false ->
            {noreply, State#state{load_pid=undefined}};
        true ->
            BlockHeight = FollowHeight + 1,
            {ok, Block} = blockchain:get_block(BlockHeight, Chain),
            Parent = self(),
            {Pid, Monitor} = spawn_monitor(fun() ->
                                                   lager:info("Loading block: ~p", [BlockHeight]),
                                                   be_block:load(Block, Conn),
                                                   Parent ! {chain_load, BlockHeight}
                                           end),
            {noreply, State#state{load_pid={Pid, Monitor}}}
    end;
handle_info({'DOWN', _, process, _, normal}, State=#state{}) ->
    {noreply, State};
handle_info({'DOWN', Monitor, process, Pid, Info}, State=#state{load_pid={Pid, Monitor}, db_conn=Conn}) ->
    catch epgsql:close(Conn),
    {noreply, State#state{state=Info, load_pid=undefined, db_conn=undefined, db_state=undefined}};




%% Wait fo chain to come up
handle_info(chain_check, State=#state{chain=undefined}) ->
    case blockchain_worker:blockchain() of
        undefined ->
            erlang:send_after(500, self(), chain_check),
            {noreply, State};
        Chain ->
            ok = blockchain_event:add_handler(self()),
            self() ! chain_load,
            {noreply, State#state{chain = Chain}}
    end;
handle_info(chain_check, State=#state{}) ->
    {noreply, State};
handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Utilities
%%

row_count(TableName, Conn) ->
    {ok, _, [{V}]} = epgsql:squery(Conn, lists:flatten(io_lib:format("select count(*) from ~s", [TableName]))),
    {Num, <<>>} = string:to_integer(V),
    Num.

connect_database() ->
    {ok, Conn} = psql_migration:open_connection(),
    ConnState = be_block:init(Conn),
    {Conn, ConnState}.
