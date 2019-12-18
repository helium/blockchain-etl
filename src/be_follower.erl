-module(be_follower).

-behaviour(gen_server).

%% API
-export([start_link/0, height/0, chain_height/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
         chain :: blockchain:chain(),
         db_conn :: be_block:state(),
         load_pid=undefined :: pid() | undefined
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

height() ->
    gen_server:call(?SERVER, height).

chain_height() ->
    gen_server:call(?SERVER, chain_height).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Conn} = psql_migration:open_connection(),
    ConnState = be_block:init(Conn),
    self() ! chain_check,
    {ok, #state{db_conn=ConnState}}.

handle_call(chain_height, _From, State=#state{chain=Chain}) ->
    {reply, blockchain:height(Chain), State};
handle_call(height, _From, State=#state{db_conn=Conn}) ->
    {reply, be_block:current_height(Conn), State};
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
handle_info(chain_load, State=#state{db_conn=Conn}) ->
    {ok, FollowHeight} = be_block:current_height(Conn),
    handle_info({chain_load, FollowHeight}, State);
handle_info({chain_load, FollowHeight}, State=#state{chain=Chain, db_conn=Conn}) ->
    {ok, ChainHeight} = blockchain:height(Chain),
    case ChainHeight > FollowHeight of
        false ->
            {noreply, State#state{load_pid=undefined}};
        true ->
            BlockHeight = FollowHeight + 1,
            {ok, Block} = blockchain:get_block(BlockHeight, Chain),
            Parent = self(),
            Pid = spawn_link(fun() ->
                                     lager:info("Loading block: ~p", [BlockHeight]),
                                     be_block:load(Block, Conn),
                                     Parent ! {chain_load, BlockHeight}
                             end),
            {noreply, State#state{load_pid=Pid}}
    end;


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
