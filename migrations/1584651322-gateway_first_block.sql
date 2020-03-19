-- migrations/1584651322-gateway_first_block.sql
-- :up

alter table gateways add column first_block bigint references blocks(height);

-- update all gateway rows with the first block that account was seen
-- in
update gateways set first_block=subquery.block
       from (select min(block) as block, address
            from gateways group by address) as subquery
where gateways.address=subquery.address;

alter table gateways alter column first_block set not null;

-- recreate materialized view
drop materialized view gateway_ledger;
create materialized view gateway_ledger as
       select * from gateways
       where (block, address) in
             (select max(block) as block, address from gateways group by address);

-- recreate unique index
create unique index gateway_ledger_gateway_idx on gateway_ledger(address);
-- Add an index that allows ordering the ledger for paging purposes
create index gateway_ledger_first_block_idx on gateway_ledger(first_block);

-- :down

-- Drop the materialized view so it doesn't depend on first_block
drop materialized view gateway_ledger;

-- remove the column from gateways
alter table gateways drop column first_block;

-- recreate materialized view
create materialized view gateway_ledger as
       select * from gateways
       where (block, address) in
             (select max(block) as block, address from gateways group by address);

-- recreate unique index
create unique index gateway_ledger_gateway_idx on gateway_ledger(address);
