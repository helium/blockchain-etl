-- migrations/1592918856-block_created_at.sql
-- :up

alter table blocks add column created_at TIMESTAMPTZ;

-- :down

alter table blocks drop column created_at;
