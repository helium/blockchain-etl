-- migrations/1584580834-acccount_first_block.sql
-- :up

alter table accounts add column first_block bigint references blocks(height);

-- update all account rows with the first block that account was seen
-- in
update accounts set first_block=subquery.block
       from (select min(block) as block, address
            from accounts group by address) as subquery
where accounts.address=subquery.address;

alter table accounts alter column first_block set not null;

-- re-create the materialized view to include the first_block column
drop materialized view account_ledger;
create materialized view account_ledger as
       select * from accounts
       where (block, address) in
             (select max(block) as block, address from accounts group by address);

-- re-construct the primary index
create unique index account_ledger_address_idx on account_ledger(address);
-- Add an index that allows ordering the ledger for paging purposes
create index account_ledger_first_block_idx on account_ledger(first_block);

-- :down

-- Drop the materialized view so it doesn't depend on first_block
drop materialized view account_ledger;

-- remove the column from accounts
alter table accounts drop column first_block;

-- re-create the materialized view
create materialized view account_ledger as
       select * from accounts
       where (block, address) in
             (select max(block) as block, address from accounts group by address);

-- and just create the one address index
create unique index account_ledger_address_idx on account_ledger(address);
