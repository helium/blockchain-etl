-module(be_follower).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
         chain
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case blockchain_worker:blockchain() of
        undefined ->
            erlang:send_after(500, self(), chain_check),
            {ok, #state{}};
        Chain ->
            ok = blockchain_event:add_handler(self()),
            {ok, #state{chain = Chain}}
    end.

handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({blockchain_event, {add_block, Hash, _Sync, Ledger}},
            #state{chain = Chain} = State) ->

    %%% START TODO

    %% Here, you should add the whatever code you would like called at
    %% each block sync.  The code before the END comment is very
    %% basic, to give you an idea of the kind of thing that can be
    %% done here.  For more advanced examples, please see the miner
    %% repository.
    %% e.g. https://github.com/helium/miner/blob/master/src/miner.erl#L497
    %%      https://github.com/helium/miner/blob/master/src/miner_consensus_mgr.erl#L364

    {ok, Block} = blockchain:get_block(Hash, Chain),
    {ok, LedgerHeight} = blockchain_ledger_v1:current_height(Ledger),
    BlockHeight = blockchain_block:height(Block),
    Txns = types(blockchain_block:transactions(Block)),

    lager:info("added block ~p (ledger height ~p) with txns: ~p",
               [BlockHeight, LedgerHeight, Txns]),

    %%% END TODO

    {noreply, State};
handle_info(chain_check, _State) ->
    {ok, State1} = init([]),
    {noreply, State1};
handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

types(L) ->
    lists:map(fun blockchain_txn:type/1, L).
