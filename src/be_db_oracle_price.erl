-module(be_db_oracle_price).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").


-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-record(state,
       {
        last_oracle_price :: pos_integer()
       }).


-define(S_ORACLE_PRICE_INSERT, "oracle_price_insert").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(Conn, ?S_ORACLE_PRICE_INSERT,
                     ["insert into oracle_prices (block, price) values ($1, $2)"
                     ], []),
    #{
      ?S_ORACLE_PRICE_INSERT => S1
     }.


%%
%% be_block_handler
%%

init(_) ->
    {Block, LastPrice} = case ?EQUERY("select block, price from oracle_prices order by block desc limit 1", []) of
                    {ok, _, []} -> 0;
                    {ok, _, [{B,P}]} -> {B,P}
                end,
    lager:info("Oracle price set at: ~p to: ~p", [Block, LastPrice]),
    {ok, #state{last_oracle_price=LastPrice}}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State=#state{last_oracle_price=CurrentPrice}) ->
    case blockchain_ledger_v1:current_oracle_price(Ledger) of
        {error, Error} ->
            lager:info("Could not get current oracle price: ~p", [Error]),
            {ok, State};
        {ok, CurrentPrice} ->
            {ok, State};
        {ok, Price} ->
            BlockHeight = blockchain_block_v1:height(Block),
            lager:info("Adjusting oracle at: ~p to: ~p", [BlockHeight, Price]),
            Queries = [q_insert_oracle_price(BlockHeight, Price)],
            ok = ?BATCH_QUERY(Conn, Queries),
            {ok, State#state{last_oracle_price=Price}}
    end.


q_insert_oracle_price(BlockHeight, Price) ->
    Params = [BlockHeight, Price],
    {?S_ORACLE_PRICE_INSERT, Params}.
