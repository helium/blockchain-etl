-- migrations/1593015009-stats_inventory.sql
-- :up

create table stats_inventory (
       name TEXT NOT NULL,
       value BIGINT NOT NULL DEFAULT 0,

       PRIMARY KEY (name)
);

insert into stats_inventory (name, value) values
       ('blocks', (select COALESCE(max(height), 0) from blocks)),
       ('hotspots', (select COALESCE(count(*), 0) from gateway_inventory)),
       ('consensus_groups', (select COALESCE(count(*), 0) from transactions where type = 'consensus_group_v1')),
       ('challenges', (select COALESCE(count(*), 0) from transactions where type in ('poc_receipts_v1', 'poc_receipts_v2')));

-- :down

drop table stats_inventory;
