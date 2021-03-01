-module(be_db_account).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_db_follower
-export([init/1, load_block/6]).
%% api
-export([incremental_commit_hook/1, end_commit_hook/1]).

-record(account, {
    address :: libp2p_crypto:pubkey_bin(),
    dc_balance = 0,
    dc_nonce = 0,
    security_balance = 0,
    security_nonce = 0,
    balance = 0,
    nonce = 0
}).

-record(state, {
    commit_hooks = [] :: [reference()],
    updates = #{} :: #{libp2p_crypto:pubkey_bin() => #account{}}
}).

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
                "insert into accounts (block, address, dc_balance, dc_nonce, security_balance, security_nonce, balance, nonce) select ",
                "$1 as block, ",
                "$2 as address, ",
                "$3 as dc_balance, ",
                "$4 as dc_nonce, ",
                "$5 as security_balance, ",
                "$6 as security_nonce, ",
                "$7 as balance, ",
                "$8 as nonce"
            ],
            []
        ),

    #{
        ?S_ACCOUNT_INSERT => S1
    }.

%%
%% be_block_handler
%%

incremental_commit_hook(Changes) ->
    lists:foreach(
        fun
            ({_CF, put, Key, _Value}) ->
                ets:insert(?MODULE, {Key});
            (_) ->
                ok
        end,
        Changes
    ).

end_commit_hook(_) -> ok.

init(_) ->
    ets:new(?MODULE, [public, named_table]),
    {ok, #state{}}.

load_block(Conn, _Hash, Block, _Sync, Ledger, State = #state{}) ->
    MkAccount = fun (Key) ->
        lists:foldl(
            fun (UpdateFun, Account) ->
                UpdateFun(Account, Ledger)
            end,
            #account{address = Key},
            [
                fun update_securities/2,
                fun update_data_credits/2,
                fun update_balance/2
            ]
        )
    end,
    BlockHeight = blockchain_block_v1:height(Block),
    Queries = ets:foldl(
        fun
            ({Key}, Acc) ->
                [q_insert_account(BlockHeight, MkAccount(Key)) | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        ?MODULE
    ),
    ok = ?BATCH_QUERY(Conn, Queries),
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
        Acc#account.nonce
    ],
    {?S_ACCOUNT_INSERT, Params}.

update_securities(Account, Ledger) ->
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
    end.

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

update_balance(Account, Ledger) ->
    case blockchain_ledger_v1:find_entry(Account#account.address, Ledger) of
        {ok, Entry} ->
            Account#account{
                balance = blockchain_ledger_entry_v1:balance(Entry),
                nonce = blockchain_ledger_entry_v1:nonce(Entry)
            };
        _ ->
            Account
    end.
