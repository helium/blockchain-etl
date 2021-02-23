-- migrations/1584239323-actor_block.sql
-- :up

create index transaction_actor_transaction_hash_idx on transaction_actors(transaction_hash);

alter table transaction_actors add column block bigint references blocks(height);
do
$$
declare
    max_height bigint;
begin
    max_height := (select max(height) from blocks);
    for b in 1..coalesce(max_height, 1)
    loop
        update transaction_actors set block = b
               where transaction_hash in (select hash from transactions where block = b);
    end loop;
end
$$;

alter table transaction_actors alter column block set not null;
create index transaction_actor_block_idx on transaction_actors(block);

-- :down

drop index transaction_actor_transaction_hash_idx;
alter table transaction_actors drop column block;
