-module(be_db_oui).

-include("be_db_follower.hrl").
-include("be_db_worker.hrl").

-behavior(be_db_worker).
-behavior(be_db_follower).

%% be_db_worker
-export([prepare_conn/1]).
%% be_db_follower
-export([init/1, load_block/6]).
%% API
-export([subnet_to_list/1]).

-record(state, {}).

-define(S_OUI_INSERT, "oui_insert").

%%
%% be_db_worker
%%

prepare_conn(Conn) ->
    {ok, S1} =
        epgsql:parse(
            Conn,
            ?S_OUI_INSERT,
            [
                "insert into ouis (block, oui, owner, nonce, addresses, subnets) select ",
                "$1 as block, ",
                "$2 as oui, ",
                "$3 as owner, ",
                "$4 as nonce, ",
                "$5 as addresses, ",
                "$6 as subnets "
            ],
            []
        ),

    #{
        ?S_OUI_INSERT => S1
    }.

%%
%% be_block_handler
%%

init(_) ->
    {ok, #state{}}.

fold_txns(TxnTypes, Fun, InitAcc, Block) ->
    Txns = blockchain_block_v1:transactions(Block),
    FilteredTxns = lists:filter(
        fun(Txn) ->
            lists:member(blockchain_txn:type(Txn), TxnTypes)
        end,
        Txns
    ),
    lists:foldl(
        fun(Txn, Acc) -> Fun(blockchain_txn:type(Txn), Txn, Acc) end,
        InitAcc,
        FilteredTxns
    ).

load_block(Conn, _Hash, Block, _Sync, Ledger, State = #state{}) ->
    %% Find all ouis affected by transactions in this block
    BlockOuis = lists:usort(
        fold_txns(
            [
                blockchain_txn_oui_v1,
                blockchain_txn_routing_v1
            ],
            fun
                (blockchain_txn_oui_v1, Txn, Acc) ->
                    Oui =
                        case blockchain_txn_oui_v1:oui(Txn) of
                            V when V =< 1 -> 1;
                            Other -> Other + 1
                        end,
                    [Oui | Acc];
                (blockchain_txn_routing_v1, Txn, Acc) ->
                    [blockchain_txn_routing_v1:oui(Txn) | Acc]
            end,
            [],
            Block
        )
    ),
    BlockHeight = blockchain_block_v1:height(Block),
    Queries = lists:map(
        fun(Oui) ->
            {ok, Entry} = blockchain_ledger_v1:find_routing(Oui, Ledger),
            q_insert_oui(BlockHeight, Entry)
        end,
        BlockOuis
    ),
    ok = ?BATCH_QUERY(Conn, Queries),
    {ok, State}.

-spec subnet_to_list(<<_:48>>) -> [pos_integer()].
subnet_to_list(<<ABase:25/integer-unsigned-big, AMask:23/integer-unsigned-big>>) ->
    [ABase, AMask].

-spec q_insert_oui(pos_integer(), blockchain_ledger_routing_v1:routing()) ->
    {Query :: string(), Params :: list()}.
q_insert_oui(BlockHeight, Routing) ->
    Params = [
        BlockHeight,
        blockchain_ledger_routing_v1:oui(Routing),
        ?BIN_TO_B58(blockchain_ledger_routing_v1:owner(Routing)),
        blockchain_ledger_routing_v1:nonce(Routing),
        [
            ?BIN_TO_B58(A)
         || A <- blockchain_ledger_routing_v1:addresses(Routing)
        ],
        [
            subnet_to_list(S)
         || S <- blockchain_ledger_routing_v1:subnets(Routing)
        ]
    ],
    {?S_OUI_INSERT, Params}.
