-- migrations/1623858428-gateway_mode_rename.sql
-- :up

alter table gateways
    drop column mode;

alter table gateway_inventory
    drop column mode;

drop type if exists gateway_mode;

create type gateway_mode as enum (
    'full',
    'light',
    'dataonly'
);

alter table gateways
    add column mode gateway_mode;

alter table gateway_inventory
    add column mode gateway_mode;

update gateway_inventory SET
    mode = 'full';


