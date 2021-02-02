-- migrations/1612220889-reward_brin_idx.sql
-- :up

drop index if exists rewards_block_idx;
create index rewards_time_idx on rewards using brin(time);

-- :down

drop index rewards_time_idx;
create index rewards_block_idx on rewards(block);