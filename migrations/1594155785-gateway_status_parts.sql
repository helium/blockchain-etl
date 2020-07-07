-- migrations/1594155785-gateway_status_parts.sql
-- :up

alter table gateway_status
      add column poc_interval BIGINT,
      add column last_challenge BIGINT;

-- :down

alter table gateway_status
      drop column poc_interval,
      drop column last_challenge;
