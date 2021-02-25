-- migrations/1586300385-rebuild_first_blocks.sql
-- :up

update gateways set first_block=subquery.block
       from (select min(block) as block, address
            from gateways group by address) as subquery
where gateways.address=subquery.address;

update accounts set first_block=subquery.block
       from (select min(block) as block, address
            from accounts group by address) as subquery
where accounts.address=subquery.address;


