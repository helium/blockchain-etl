-- migrations/1584580834-acccount_first_block.sql
-- :up

alter table accounts add column first_block bigint references blocks(height);

update accounts set first_block=subquery.block
       from (select min(block) as block, address
            from accounts group by address) as subquery
where accounts.address=subquery.address;

alter table accounts alter column first_block set not null;

drop materialized view account_ledger;
create materialized view account_ledger as
       select * from accounts
       where (block, address) in
             (select max(block) as block, address from accounts group by address);

create unique index account_ledger_address_idx on account_ledger(address);
create index account_ledger_first_block_idx on account_ledger(first_block);

-- :down

drop materialized view account_ledger;

alter table accounts drop column first_block;

create materialized view account_ledger as
       select * from accounts
       where (block, address) in
             (select max(block) as block, address from accounts group by address);

create unique index account_ledger_address_idx on account_ledger(address);
