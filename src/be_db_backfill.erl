-module(be_db_backfill).

-include("be_db_worker.hrl").

-export([receipts_challenger/4]).

-define(INSERT_RECEIPTS_CHALLENGERS, [
    "insert into transaction_actors ",
    "(select fields->>'challenger' as actor, 'challenger' as actor_role, hash as transaction_hash, block ",
    " from transactions ",
    " where type = 'poc_receipts_v1' and block between $1 and $2) ",
    "on conflict do nothing"
]).

receipts_challenger(MinBlock, MaxBlock, BatchSize, _Fun) when
    MinBlock >= MaxBlock orelse BatchSize == 0
->
    0;
receipts_challenger(MinBlock, MaxBlock, BatchSize, Fun) ->
    BatchMinBlock = max(MinBlock, MaxBlock - BatchSize),
    {ok, Inserted} = receipts_challenger(BatchMinBlock, MaxBlock),
    Fun(BatchMinBlock, MaxBlock, Inserted),
    timer:sleep(100),
    Inserted + receipts_challenger(MinBlock, BatchMinBlock, BatchSize, Fun).

receipts_challenger(MinBlock, MaxBlock) ->
    ?EQUERY(?INSERT_RECEIPTS_CHALLENGERS, [MinBlock, MaxBlock]).
