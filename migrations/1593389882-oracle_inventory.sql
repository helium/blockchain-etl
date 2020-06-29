-- migrations/1593389882-oracle_inventory.sql
-- :up

create table oracle_inventory (
       address TEXT NOT NULL,

       PRIMARY KEY (address)
);

-- :down

drop table oracle_inventory;
