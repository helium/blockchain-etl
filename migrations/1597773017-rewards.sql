-- migrations/1597773017-rewards.sql
-- :up

create table rewards (
       block bigint not null references blocks(height) on delete cascade,
       transaction_hash text not null references transactions(hash),
       time bigint not null,
       account text not null,
       gateway text,
       amount bigint not null,

       primary key(block, account, gateway)
);


create type reward_entry as (account TEXT, gateway TEXT, type TEXT, amount bigint);
create or replace function insert_rewards() returns void as $$
declare
    txn RECORD;
begin
    for txn in
        select *
        from transactions where type = 'rewards_v1'
        order by block asc
    loop
        insert into rewards (block, transaction_hash, time, account, gateway, amount)
        select txn.block, txn.hash, txn.time, account, coalesce(gateway, '1Wh4bh') as gateway, sum(amount)::bigint as amount
        from jsonb_populate_recordset(null::reward_entry, txn.fields->'rewards')
        group by (account, gateway);
    end loop;
end; $$
language plpgsql;

set synchronous_commit = off;
alter table rewards set (autovacuum_enabled = false);
alter table rewards set unlogged;


select insert_rewards();

set synchronous_commit = on;
alter table rewards set (autovacuum_enabled = true);
alter table rewards set logged;

create index rewards_block_idx on rewards(block);
create index rewards_account_idx on rewards(account);
create index rewards_gateway_idx on rewards(gateway);

-- :down

drop function insert_rewards;
drop table rewards;
drop type reward_entry;
