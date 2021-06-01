-- migrations/1586706609-ledger_first_block.sql
-- :up

drop materialized view gateway_ledger;
alter table gateways drop column first_block;

create materialized view gateway_ledger as
    select ga.first_block, g.*, l.long_country, l.short_country, l.long_state, l.short_state, l.long_city, l.short_city, l.long_street, l.short_street from
        (select max(block) as last_block, min(block) as first_block, address from gateways group by address) ga
        inner join gateways g on (g.block, g.address) = (ga.last_block, ga.address)
        left join locations l on g.location = l.location;

create unique index gateway_ledger_gateway_idx on gateway_ledger(address);
create index gateway_ledger_first_block_idx on gateway_ledger(first_block);

drop materialized view account_ledger;
alter table accounts drop column first_block;
create materialized view account_ledger as
    select aa.first_block, a.*  from
        (select max(block) as last_block, min(block) as first_block, address from accounts group by address) aa
        inner join accounts a on (a.block, a.address) = (aa.last_block, aa.address);

create unique index account_ledger_address_idx on account_ledger(address);
create index account_ledger_first_block_idx on account_ledger(first_block);


-- :down

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
