-- migrations/1628698151-reward_index_improve.sql
-- :up

create index if not exists rewards_block_idx on rewards(block);
create index if not exists blocks_timestamp_idx on blocks(timestamp);


-- :down

drop index if exists rewards_block_idx;
drop index if exists blocks_timestamp_idx;
