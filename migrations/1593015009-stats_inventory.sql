-- migrations/1593015009-stats_inventory.sql
-- :up

create table stats_inventory (
       name TEXT NOT NULL,
       value BIGINT NOT NULL,

       PRIMARY KEY (name)
);

insert into stats_inventory (name, value) values
       ('blocks', (select max(height) from blocks)),
       ('hotspots', (select count(*) from gateway_inventory)),
       ('consensus_groups', (select count(*) from transactions where type = 'consensus_group_v1')),
       ('challenges', (select count(*) from transactions where type = 'poc_receipts_v1'));

-- :down

drop table stats_inventory;
