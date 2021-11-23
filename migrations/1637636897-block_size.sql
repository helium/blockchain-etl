-- migrations/1637636897-block_size.sql
-- :up

alter table blocks add column size bigint;

-- :down

alter table blocks drop column size;
