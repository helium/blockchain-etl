-module(be_db_follower).

-callback init() -> {ok, State::any()} | {error, term()}.
-callback load(Conn::term(),
               Hash::binary(),
               blockchain:block(),
               Sync::boolean(),
               blockchain_ledger_v1:ledger(),
               State::any()) -> {ok, NewState::any()}.

-behaviour(be_follower).
-include("be_db_worker.hrl").

-export([init/0, load/5]).

-define(HANDLER_MODULES,
        [
         be_db_block,
         be_db_txn_actor,
         be_db_account,
         be_db_gateway
         ]).

-record(state,
        {
         handler_state :: [{Module::atom(), State::any()}]
        }).

init() ->
    Handlers = lists:map(fun(Mod) ->
                                 {ok, State} = Mod:init(),
                                 {Mod, State}
                         end, ?HANDLER_MODULES),
    {ok, #state{handler_state=Handlers}}.

load(Hash, Block, Sync, Ledger, State=#state{}) ->
    LoadFun = fun(Conn) ->
                      States =
                          lists:foldl(fun({Handler, HandlerState}, HandlerStates) ->
                                              {ok, NewHandlerState} =
                                                  Handler:load(Conn, Hash, Block, Sync, Ledger, HandlerState),
                                              [{Handler, NewHandlerState} | HandlerStates]
                                      end, [], State#state.handler_state),
                      {ok, lists:reverse(States)}
              end,

    lager:info("Storing block: ~p", [blockchain_block_v1:height(Block)]),
    {ok, HandlerStates} = ?WITH_TRANSACTION(LoadFun),
    {ok, #state{handler_state=HandlerStates}}.
