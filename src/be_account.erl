-module(be_account).

-include("be_block_handler.hrl").

-behavior(be_db_worker).
-behavior(be_block_handler).

%% be_db_worker
-export([prepare_conn/1]).
%% be_block_handler
-export([init/1, load/6]).

-define(ACCOUNT_LEDGER_REFRESH_SECS, 30).

-record(state,
       {
        base_secs=calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) :: pos_integer(),
        s_insert_account :: epgsql:statement(),

        last_account_ledger_refresh=0 :: non_neg_integer(),
        account_ledger_refresh_secs=?ACCOUNT_LEDGER_REFRESH_SECS  :: non_neg_integer()
       }).

-record(account,
        {
         address :: libp2p_crypto:pubkey_bin(),
         dc_balance = 0,
         dc_nonce = 0,
         security_balance = 0,
         security_nonce = 0,
         balance = 0,
         nonce = 0
        }).

-define(Q_INSERT_ACCOUNT, "insert_account").
-define(Q_REFRESH_ASYNC_ACCOUNT_LEDGER, "refresh materialized view concurrently account_ledger").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, _} =
        epgsql:parse(Conn, ?Q_INSERT_ACCOUNT,
                     "insert into accounts (block, timestamp, address, dc_balance, dc_nonce, security_balance, security_nonce, balance, nonce) values ($1, $2, $3, $4, $5, $6, $7, $8, $9)", []),
    ok.


%%
%% be_block_handler
%%

init(Conn) ->
    {ok, InsertAccount} = epgsql:describe(Conn, statement, ?Q_INSERT_ACCOUNT),

    lager:info("Updating account_ledger table concurrently"),
    {ok, _, _} = epgsql:squery(Conn, ?Q_REFRESH_ASYNC_ACCOUNT_LEDGER),

    {ok, #state{
            s_insert_account = InsertAccount
           }}.

load(Conn, _Hash, Block, _Sync, Ledger, State=#state{}) ->
    Txns = blockchain_block_v1:transactions(Block),
    %% Fetch actor keys that relate to accounts from each transaction's actors
    AccountsFromActors = fun(Actors) ->
                                 lists:filtermap(fun({"payee", Key}) -> {true, Key};
                                                    ({"payer", Key}) -> {true, Key};
                                                    (_) -> false
                                                 end, Actors)
                         end,
    AccountKeys = lists:usort(lists:foldl(fun(Txn, Acc) ->
                                                  AccountKeys = AccountsFromActors(be_txn_actor:to_actors(Txn)),
                                                  AccountKeys ++ Acc
                                          end, [], Txns)),
    %% Now update an accounts map with account balances for each found
    %% account key by looking at ledger entries
    Accounts = lists:foldl(fun(UpdateFun, Acc) ->
                                   UpdateFun(Ledger, AccountKeys, Acc)
                           end,
                           #{},
                           [fun update_securities/3,
                            fun update_data_credits/3,
                            fun update_balance/3]),
    BlockHeight = blockchain_block_v1:height(Block),
    BlockTime = blockchain_block_v1:time(Block),
    Queries = [q_insert_account(BlockHeight, BlockTime, A, State) || A <- maps:values(Accounts)],
    maybe_refresh_account_ledger(Conn, be_block_handler:run_queries(Conn, Queries, State)).

maybe_refresh_account_ledger(_Conn, {ok, 0, State=#state{}}) ->
    {ok, 0, State};
maybe_refresh_account_ledger(Conn, {ok, Count, State=#state{}}) ->
    case erlang:system_time(seconds) - State#state.last_account_ledger_refresh
        > State#state.account_ledger_refresh_secs of
        true ->
            lager:info("Updating account_ledger table concurrently"),
            {ok, _, _} = epgsql:squery(Conn, ?Q_REFRESH_ASYNC_ACCOUNT_LEDGER),
            {ok, Count+1, State#state{last_account_ledger_refresh=erlang:system_time(seconds)}};
        _ ->
            {ok, Count, State}
    end.


q_insert_account(BlockHeight, BlockTime, Acc=#account{}, #state{s_insert_account=Stmt, base_secs=BaseSecs}) ->
    BlockDate = calendar:gregorian_seconds_to_datetime(BaseSecs + BlockTime),
    Params = [BlockHeight,
              BlockDate,
              ?BIN_TO_B58(Acc#account.address),
              Acc#account.dc_balance,
              Acc#account.dc_nonce,
              Acc#account.security_balance,
              Acc#account.security_nonce,
              Acc#account.balance,
              Acc#account.nonce
             ],
    {Stmt, Params}.


update_securities(Ledger, Keys, Accounts) ->
    update_accounts(Keys, Accounts, blockchain_ledger_v1:securities(Ledger),
                    fun(Account, Entry) ->
                            Account#account {
                              security_balance = blockchain_ledger_security_entry_v1:balance(Entry),
                              security_nonce = blockchain_ledger_security_entry_v1:nonce(Entry)
                             }
                    end).

update_data_credits(Ledger, Keys, Accounts) ->
    update_accounts(Keys, Accounts, blockchain_ledger_v1:dc_entries(Ledger),
                    fun(Account, Entry) ->
                            Account#account {
                              dc_balance = blockchain_ledger_data_credits_entry_v1:balance(Entry),
                              dc_nonce = blockchain_ledger_data_credits_entry_v1:nonce(Entry)
                             }
                    end).

update_balance(Ledger, Keys, Accounts) ->
    update_accounts(Keys, Accounts, blockchain_ledger_v1:entries(Ledger),
                    fun(Account, Entry) ->
                            Account#account {
                              balance = blockchain_ledger_entry_v1:balance(Entry),
                              nonce = blockchain_ledger_entry_v1:nonce(Entry)
                             }
                    end).

%% Helper to map a function over an account map with given ledger entries
update_accounts(Keys, Accounts, Entries, Fun) ->
    lists:foldl(fun(Key, Acc) ->
                        case maps:get(Key, Entries, false) of
                            false ->
                                Acc;
                            Entry ->
                                Account = maps:get(Key, Acc, #account{address=Key}),
                                NewAccount = Fun(Account, Entry),
                                Acc#{Key => NewAccount}
                        end
                end,
                Accounts, Keys).
