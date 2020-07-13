-- migrations/1594674327-stats_inventory_txns.sql
-- :up

insert into stats_inventory (name, value) values
       ('transactions', (select COALESCE(count(*), 0) from transactions));


-- :down

delete from stats_inventory where name = 'transactions';
