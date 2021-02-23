-module(be_db_oracle_price).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-include_lib("blockchain/include/blockchain_vars.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load_block/6]).

-record(state, {
    last_oracle_price :: pos_integer()
}).

-define(S_ORACLE_PRICE_INSERT, "oracle_price_insert").
-define(S_ORACLE_PRICE_PREDICTION_INSERT, "oracle_price_prediction_insert").
-define(S_ORACLE_PRICE_PREDICTION_DELETE_ALL, "oracle_price_prediction_delete").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_ORACLE_PRICE_INSERT,
            ["insert into oracle_prices (block, price) values ($1, $2)"],
            []
        ),

    {ok, S2} =
        epgsql:parse(
            Conn,
            ?S_ORACLE_PRICE_PREDICTION_INSERT,
            ["insert into oracle_price_predictions (price, time) values ($1, $2)"],
            []
        ),

    {ok, S3} =
        epgsql:parse(
            Conn,
            ?S_ORACLE_PRICE_PREDICTION_DELETE_ALL,
            ["delete from oracle_price_predictions"],
            []
        ),

    #{
        ?S_ORACLE_PRICE_INSERT => S1,
        ?S_ORACLE_PRICE_PREDICTION_INSERT => S2,
        ?S_ORACLE_PRICE_PREDICTION_DELETE_ALL => S3
    }.

%%
%% be_block_handler
%%

init(_) ->
    {Block, LastPrice} =
        case
            ?EQUERY(
                "select block, price from oracle_prices order by block desc limit 1",
                []
            )
        of
            {ok, _, []} -> {1, 0};
            {ok, _, [{B, P}]} -> {B, P}
        end,
    lager:info("Oracle price set at: ~p to: ~p", [Block, LastPrice]),
    {ok, #state{last_oracle_price = LastPrice}}.

load_block(
    Conn,
    _Hash,
    Block,
    _Sync,
    Ledger,
    State = #state{last_oracle_price = CurrentPrice}
) ->
    BlockHeight = blockchain_block_v1:height(Block),
    UpdateOraclePrices =
        fun (Queries, AccState) ->
            case blockchain_ledger_v1:current_oracle_price(Ledger) of
                {error, Error} ->
                    lager:info("Could not get current oracle price: ~p", [Error]),
                    {Queries, AccState};
                {ok, CurrentPrice} ->
                    {Queries, AccState};
                {ok, Price} ->
                    lager:info("Adjusting oracle at: ~p to: ~p", [BlockHeight, Price]),
                    {[q_insert_oracle_price(BlockHeight, Price) | Queries], State#state{
                        last_oracle_price = Price
                    }}
            end
        end,
    UpdateOraclePredictions =
        fun (Queries, AccState) ->
            case blockchain:config(?price_oracle_refresh_interval, Ledger) of
                {ok, Interval} when BlockHeight >= Interval ->
                    NextPrices = blockchain_ledger_v1:next_oracle_prices(
                        blockchain_worker:blockchain(),
                        Ledger
                    ),
                    DeleteQueries = [{?S_ORACLE_PRICE_PREDICTION_DELETE_ALL, []}],
                    InsertQueries = [
                        {?S_ORACLE_PRICE_PREDICTION_INSERT, [Price, Time]}
                        || {Price, Time} <- NextPrices
                    ],
                    {DeleteQueries ++ InsertQueries ++ Queries, AccState};
                _ ->
                    {Queries, AccState}
            end
        end,
    {Queries, NewState} =
        lists:foldl(
            fun (Fun, {Q, S}) ->
                Fun(Q, S)
            end,
            {[], State},
            [
                UpdateOraclePrices,
                UpdateOraclePredictions
            ]
        ),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, NewState}.

q_insert_oracle_price(BlockHeight, Price) ->
    Params = [BlockHeight, Price],
    {?S_ORACLE_PRICE_INSERT, Params}.
