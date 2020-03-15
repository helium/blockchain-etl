-- migrations/1584239323-actor_block.sql
-- :up

-- Create index for transaction_actor transaction_hash
create index transaction_actor_transaction_hash_idx on transaction_actors(transaction_hash);

-- add block to transaction_actor table
alter table transaction_actors add column block bigint references blocks(height);
-- For each block update block in transaction_actor. This can take a
-- _long_ time
do $$
begin
    for b in 1..(select max(height) from blocks)
    loop
        update transaction_actors set block = b
               where transaction_hash in (select hash from transactions where block = b);
        -- raise notice 'handled block %', b;
    end loop;
end$$;

-- finally update actor block constraint to be non null.
ALTER TABLE transaction_actors ALTER COLUMN block SET NOT NULL;

-- :down
-- Down migration

-- drop index on transaction_hhash
drop index transaction_actor_transaction_hash_idx;
-- then drop the column
alter table transaction_actors drop column block;
