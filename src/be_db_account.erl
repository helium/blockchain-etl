-module(be_db_account).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").
-include_lib("blockchain/include/blockchain_vars.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_db_follower
-export([init/1, load_block/6]).
%% hooks
-export([incremental_commit_hook/1, end_commit_hook/2]).

-record(account, {
    address :: libp2p_crypto:pubkey_bin(),
    dc_balance = 0,
    dc_nonce = 0,
    security_balance = 0,
    security_nonce = 0,
    balance = 0,
    staked_balance = 0,
    mobile_balance = 0,
    iot_balance = 0,
    nonce = 0
}).

-record(state, {}).

-define(S_ACCOUNT_INSERT, "account_insert").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_ACCOUNT_INSERT,
            [
                "insert into accounts (block, address, dc_balance, dc_nonce, security_balance, security_nonce, balance, staked_balance, mobile_balance, iot_balance, nonce) select ",
                "$1 as block, ",
                "$2 as address, ",
                "$3 as dc_balance, ",
                "$4 as dc_nonce, ",
                "$5 as security_balance, ",
                "$6 as security_nonce, ",
                "$7 as balance, ",
                "$8 as staked_balance, ",
                "$9 as mobile_balance, ",
                "$10 as iot_balance, ",
                "$11 as nonce"
            ],
            []
        ),

    #{
        ?S_ACCOUNT_INSERT => S1
    }.

%%
%% be_block_handler
%%

init(_) ->
    ets:new(?MODULE, [public, named_table]),
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State = #state{}) ->
    StartStaked = erlang:monotonic_time(millisecond),
    Staked = blockchain_ledger_v1:fold_validators(
        fun(Val, Acc) ->
            Address = blockchain_ledger_validator_v1:owner_address(Val),
            Amount = blockchain_ledger_validator_v1:stake(Val),
            maps:update_with(
                Address,
                fun(Balance) -> Balance + Amount end,
                Amount,
                Acc
            )
        end,
        #{},
        Ledger
    ),
    be_db_follower:maybe_log_duration(db_account_staked_make, StartStaked),

    {ok, TokenVersion} =
        case blockchain_ledger_v1:config(?token_version, Ledger) of
            {ok, N} -> {ok, N};
            _ -> {ok, 1}
        end,
    UpdateAccount = fun(Account) ->
        lists:foldl(
            fun
                ({UpdateFun, Args}, Acc) ->
                    UpdateFun(Acc, Args, Ledger);
                (UpdateFun, Acc) ->
                    UpdateFun(Acc, Ledger)
            end,
            Account,
            [
                {fun update_securities/3, TokenVersion},
                fun update_data_credits/2,
                {fun update_balance/3, TokenVersion},
                {fun update_staked_balance/3, Staked}
            ]
        )
    end,
    %% Construct the list of accounts that have changed in this block based on
    %% transaction actors
    StartActor = erlang:monotonic_time(millisecond),
    BlockAccounts = be_db_follower:fold_actors(
        ["payer", "payee", "owner"],
        fun({_Role, Key}, Acc) ->
            Account = maps:get(Key, Acc, #account{address = Key}),
            maps:put(Key, UpdateAccount(Account), Acc)
        end,
        #{},
        Block
    ),
    be_db_follower:maybe_log_duration(db_account_actor_fold, StartActor),

    %% Merge in any accounts that are indirectly updated by the ledger and stashed
    %% in the module ets table
    StartUnhandled = erlang:monotonic_time(millisecond),
    Accounts = ets:foldl(
        fun
            ({Key}, Acc) ->
                case maps:is_key(Key, Acc) of
                    true ->
                        Acc;
                    false ->
                        lager:info("processing unhandled account ~p", [?BIN_TO_B58(Key)]),
                        maps:put(Key, UpdateAccount(#account{address = Key}), Acc)
                end;
            (_, Acc) ->
                Acc
        end,
        BlockAccounts,
        ?MODULE
    ),
    be_db_follower:maybe_log_duration(db_account_unhandled_fold, StartUnhandled),

    StartMkQuery = erlang:monotonic_time(millisecond),
    BlockHeight = blockchain_block_v1:height(Block),
    Queries = maps:fold(
        fun(_Key, Account, Acc) ->
            [q_insert_account(BlockHeight, Account) | Acc]
        end,
        [],
        Accounts
    ),
    be_db_follower:maybe_log_duration(db_account_query_make, StartMkQuery),

    StartQuery = erlang:monotonic_time(millisecond),
    ok = ?BATCH_QUERY(Conn, Queries),
    be_db_follower:maybe_log_duration(db_account_query_exec, StartQuery),

    ets:delete_all_objects(?MODULE),
    {ok, State}.

q_insert_account(BlockHeight, Acc = #account{}) ->
    Params = [
        BlockHeight,
        ?BIN_TO_B58(Acc#account.address),
        Acc#account.dc_balance,
        Acc#account.dc_nonce,
        Acc#account.security_balance,
        Acc#account.security_nonce,
        Acc#account.balance,
        Acc#account.staked_balance,
        Acc#account.mobile_balance,
        Acc#account.iot_balance,
        Acc#account.nonce
    ],
    {?S_ACCOUNT_INSERT, Params}.

update_securities(Account, _TokenVersion = 1, Ledger) ->
    case
        blockchain_ledger_v1:find_security_entry(
            Account#account.address,
            Ledger
        )
    of
        {ok, Entry} ->
            Account#account{
                security_balance = blockchain_ledger_security_entry_v1:balance(Entry),
                security_nonce = blockchain_ledger_security_entry_v1:nonce(Entry)
            };
        _ ->
            Account
    end;
update_securities(Account, _TokenVersion, _Ledger) ->
    Account.

update_data_credits(Account, Ledger) ->
    case blockchain_ledger_v1:find_dc_entry(Account#account.address, Ledger) of
        {ok, Entry} ->
            Account#account{
                dc_balance = blockchain_ledger_data_credits_entry_v1:balance(Entry),
                dc_nonce = blockchain_ledger_data_credits_entry_v1:nonce(Entry)
            };
        _ ->
            Account
    end.

update_balance(Account, TokenVersion, Ledger) ->
    case blockchain_ledger_v1:find_entry(Account#account.address, Ledger) of
        {ok, Entry} when TokenVersion == 1 ->
            Account#account{
                balance = blockchain_ledger_entry_v1:balance(Entry),
                nonce = blockchain_ledger_entry_v1:nonce(Entry)
            };
        {ok, Entry} when TokenVersion == 2 ->
            Account#account{
                mobile_balance = blockchain_ledger_entry_v2:balance(Entry, mobile),
                iot_balance = blockchain_ledger_entry_v2:balance(Entry, iot),
                security_balance = blockchain_ledger_entry_v2:balance(Entry, hst),
                balance = blockchain_ledger_entry_v2:balance(Entry),
                nonce = blockchain_ledger_entry_v2:nonce(Entry)
            };
        _ ->
            Account
    end.

update_staked_balance(Account, StakedAccounts, _Ledger) ->
    Address = Account#account.address,
    Staked = maps:get(Address, StakedAccounts, 0),
    Account#account{
        staked_balance = Staked
    }.

incremental_commit_hook(_Changes) ->
    ok.

end_commit_hook(_CF, Changes) ->
    Keys = lists:filtermap(
        fun
            ({put, Key}) -> {true, {Key}};
            (_) -> false
        end,
        Changes
    ),
    ets:insert(?MODULE, Keys).
