-- migrations/1609078980-gateway_listen_addrs.sql
-- :up

alter table gateway_status 
add column listen_addrs jsonb;

-- :down

alter table gateway_status 
drop column listen_addrs;
