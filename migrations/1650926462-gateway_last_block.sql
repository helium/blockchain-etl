-- migrations/1650926462-gateway_last_block.sql
-- :up

update gateway_inventory set last_block=subquery.last_block
       from (
            select 
                address, 
                (select max(block) from transaction_actors
                    where actor = i.address
                ) as last_block 
            from gateway_inventory i
        ) as subquery
where gateway_inventory.address=subquery.address;