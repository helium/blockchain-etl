-- migrations/1592306954-vars_inventory.sql
-- :up

create type var_type as enum (
       'integer',
       'float',
       'atom',
       'binary'
);

create table vars_inventory (
       name TEXT NOT NULL,
       type var_type NOT NULL,
       value TEXT NOT NULL,

       PRIMARY KEY (name)
);

-- :down

drop table vars_inventory;
drop type var_type;
