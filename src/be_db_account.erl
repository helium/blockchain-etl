-module(be_db_account).

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
        base_secs=calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) :: pos_integer()
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

-define(S_ACCOUNT_INSERT, "account_insert").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(Conn, ?S_ACCOUNT_INSERT,
                     ["insert into accounts (block, address, dc_balance, dc_nonce, security_balance, security_nonce, balance, nonce) select ",
                      "$1 as block, ",
                      "$2 as address, ",
                      "$3 as dc_balance, ",
                      "$4 as dc_nonce, ",
                      "$5 as security_balance, ",
                      "$6 as security_nonce, ",
                      "$7 as balance, ",
                      "$8 as nonce"],
                     []),

    #{
      ?S_ACCOUNT_INSERT => S1
     }.


%%
%% be_block_handler
%%

init(_) ->
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State=#state{}) ->
    Txns = blockchain_block_v1:transactions(Block),
    %% Fetch actor keys that relate to accounts from each transaction's actors
    AccountsFromActors = fun(Actors) ->
                                 lists:filtermap(fun({"payee", Key}) -> {true, Key};
                                                    ({"payer", Key}) -> {true, Key};
                                                    (_) -> false
                                                 end, Actors)
                         end,
    AccountKeys = lists:usort(lists:foldl(fun(Txn, Acc) ->
                                                  AccountKeys = AccountsFromActors(be_db_txn_actor:to_actors(Txn)),
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
    Queries = [q_insert_account(BlockHeight, A, State) || A <- maps:values(Accounts)],
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State}.


q_insert_account(BlockHeight, Acc=#account{}, #state{}) ->
    Params = [BlockHeight,
              ?BIN_TO_B58(Acc#account.address),
              Acc#account.dc_balance,
              Acc#account.dc_nonce,
              Acc#account.security_balance,
              Acc#account.security_nonce,
              Acc#account.balance,
              Acc#account.nonce
             ],
    {?S_ACCOUNT_INSERT, Params}.


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
