-- migrations/1584651322-gateway_first_block.sql
-- :up

alter table gateways add column first_block bigint references blocks(height);

update gateways set first_block=subquery.block
       from (select min(block) as block, address
            from gateways group by address) as subquery
where gateways.address=subquery.address;

alter table gateways alter column first_block set not null;

drop materialized view gateway_ledger;
create materialized view gateway_ledger as
       select * from gateways
       where (block, address) in
             (select max(block) as block, address from gateways group by address);

create unique index gateway_ledger_gateway_idx on gateway_ledger(address);
create index gateway_ledger_first_block_idx on gateway_ledger(first_block);

-- :down

drop materialized view gateway_ledger;

alter table gateways drop column first_block;

create materialized view gateway_ledger as
       select * from gateways
       where (block, address) in
             (select max(block) as block, address from gateways group by address);

create unique index gateway_ledger_gateway_idx on gateway_ledger(address);
