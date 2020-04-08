-- migrations/1586300385-rebuild_first_blocks.sql
-- :up

-- update all gateway rows with the first block that gateway was seen
-- in. This will take a long time
update gateways set first_block=subquery.block
       from (select min(block) as block, address
            from gateways group by address) as subquery
where gateways.address=subquery.address;

-- update all account rows with the first block that account was seen
-- in. This will takea long time too!
update accounts set first_block=subquery.block
       from (select min(block) as block, address
            from accounts group by address) as subquery
where accounts.address=subquery.address;

-- :down

-- No down migration possible or required
