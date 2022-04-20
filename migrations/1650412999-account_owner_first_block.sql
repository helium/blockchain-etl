-- migrations/1650412999-account_owner_first_block.sql
-- :up

update account_inventory set first_block=subquery.first_block
       from (
            select address, first_block from (
                select 
                    address, 
                    first_block as current, 
                    (select max(block) from transaction_actors
                        where actor_role = 'owner' and actor = i.address
                    ) as first_block from account_inventory i
            ) s where s.first_block is not null and s.first_block < s.current 
        ) as subquery
where account_inventory.address=subquery.address;

