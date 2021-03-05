-- migrations/1614947351-ouis.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'router';

create table ouis (
    block BIGINT NOT NULL references blocks(height) on delete cascade,

    oui BIGINT NOT NULL,
    owner TEXT NOT NULL,
    nonce BIGINT NOT NULL,
    addresses TEXT ARRAY NOT NULL,
    subnets TEXT ARRAY NOT NULL,

    primary key (block, oui)
);

create table oui_inventory (
    oui BIGINT NOT NULL,
    owner TEXT NOT NULL,
    nonce BIGINT NOT NULL,
    addresses TEXT ARRAY NOT NULL,
    subnets TEXT ARRAY NOT NULL,

    first_block BIGINT references blocks(height) on delete set NULL,
    last_block BIGINT references blocks(height) on delete set NULL,

    primary key(oui)
);

create index oui_inventory_first_block on oui_inventory(first_block);
create index oui_owner_idx on oui_inventory(owner);

CREATE OR REPLACE FUNCTION oui_inventory_update()
 RETURNS TRIGGER AS $$
 BEGIN
   insert into oui_inventory
        (oui, owner,
        nonce, addresses, subnets,
        first_block, last_block)
   VALUES
        (NEW.oui, NEW.owner,
        NEW.nonce, NEW.addresses, NEW.subnets,
        NEW.block, NEW.block)
   ON CONFLICT (oui) DO UPDATE SET
        owner = EXCLUDED.owner,
        nonce = EXCLUDED.nonce,
        addresses = EXCLUDED.addresses,
        subnet = EXCLUDED.subnets,
        last_block = EXCLUDED.last_block;
  RETURN NEW;
 END;
 $$ LANGUAGE plpgsql;

create trigger oui_insert
       after insert on ouis
       for each row
       execute procedure oui_inventory_update();

with data as (
    select
        block,
        hash,
        r.address
    from transactions t1
    left join jsonb_array_elements_text(t1.fields->'addresses') as r(address) on true
    where type = 'oui_v1' and address is not null
)
insert into transaction_actors (actor, actor_role, transaction_hash, block)
    select address, 'router', hash, block from data
on conflict do nothing;

with data as (
    select
        block,
        hash,
        r.address
    from transactions t1
    left join jsonb_array_elements_text(t1.fields#>'{action, addresses}') as r(address) on true
    where type = 'routing_v1' and fields#>>'{action, action}' = 'update_routers' and address is not null
)
insert into transaction_actors (actor, actor_role, transaction_hash, block)
    select address, 'router', hash, block from data
on conflict do nothing;


with data as (
    select
        block,
        fields->>'owner'::text as owner,
        (fields->>'oui')::numeric + 1 as oui,
        (select coalesce(array_agg(a)::text[], array[]::text[]) from jsonb_array_elements_text(fields->'addresses') as a) as addresses,
        (select array[]::text[]) as subnets
    from transactions
    where type = 'oui_v1'
)
insert into ouis (block, oui, owner, nonce, addresses, subnets)
    select block, oui, owner, 0, addresses, subnets from data
on conflict do nothing;


with data as (
    select
        block,
        fields->>'owner'::text as owner,
        (fields->>'oui')::numeric as oui,
        (select coalesce(array_agg(a)::text[], array[]::text[]) from jsonb_array_elements_text(fields#>'{actions, addresses}') as a) as addresses,
        (select array[]::text[]) as subnets
    from transactions
    where type = 'routing_v1'
)
insert into ouis (block, oui, owner, nonce, addresses, subnets)
    select block, oui, owner, 0, addresses, subnets from data
on conflict do nothing;


-- :down

drop trigger oui_insert on ouis;
drop function oui_inventory_update;
drop table oui_inventory;
drop table ouis;