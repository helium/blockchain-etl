-- migrations/1591708556-block_snapshot_hash.sql
-- :up

alter table blocks add column snapshot_hash TEXT;

create index blocks_snapshot_idx on blocks(snapshot_hash);


-- :down

alter table blocks drop column snapshot_hash;
