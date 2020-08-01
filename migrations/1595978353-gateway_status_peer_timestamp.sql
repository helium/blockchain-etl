-- migrations/1595978353-gateway_status_peer_timestamp.sql
-- :up

alter table gateway_status add column peer_timestamp timestamptz;

-- :down

alter table gateway_status drop column peer_timestamp;
