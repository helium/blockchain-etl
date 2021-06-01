-- migrations/1590689602-gateway_inventory.sql
-- :up

create table gateway_inventory (
        address TEXT NOT NULL,

        owner TEXT NOT NULL,
        location TEXT,
        alpha FLOAT NOT NULL,
        beta FLOAT NOT NULL,
        delta INT NOT NULL,
        score FLOAT NOT NULL,
        last_poc_challenge BIGINT references blocks(height) on delete set NULL,
        last_poc_onion_key_hash TEXT,
        witnesses jsonb NOT NULL,

        first_block BIGINT references blocks(height) on delete set NULL,
        last_block BIGINT references blocks(height) on delete set NULL,

        PRIMARY KEY (address)
);

create index gateway_inventory_location_idx on gateway_inventory(location);
create index gateway_inventory_first_block on gateway_inventory(first_block);
create index gateway_inventory_owner on gateway_inventory(owner);

insert into gateway_inventory
    select g.address, g.owner, g.location, g.alpha, g.beta, g.delta, g.score, g.last_poc_challenge, g.last_poc_onion_key_hash, g.witnesses, ga.first_block, ga.last_block from
        (select max(block) as last_block, min(block) as first_block, address from gateways group by address) ga
        inner join gateways g on (g.block, g.address) = (ga.last_block, ga.address);

CREATE OR REPLACE FUNCTION gateway_inventory_update()
RETURNS TRIGGER AS $$
BEGIN
  insert into gateway_inventory
         (address, owner, location, alpha, beta, delta, score,
          last_poc_challenge, last_poc_onion_key_hash, witnesses,
          first_block, last_block)
  VALUES
        (NEW.address, NEW.owner, NEW.location, NEW.alpha, NEW.beta, NEW.delta, NEW.score,
        NEW.last_poc_challenge, NEW.last_poc_onion_key_hash, NEW.witnesses,
        NEW.block, NEW.block
        )
  ON CONFLICT (address) DO UPDATE SET
       owner = EXCLUDED.owner,
       location = EXCLUDED.location,
       alpha = EXCLUDED.alpha,
       beta = EXCLUDED.beta,
       delta = EXCLUDED.delta,
       score = EXCLUDED.score,
       last_poc_challenge = EXCLUDED.last_poc_challenge,
       last_poc_onion_key_hash = EXCLUDED.last_poc_onion_key_hash,
       witnesses = EXCLUDED.witnesses,
       last_block = EXCLUDED.last_block;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

create trigger gateway_insert
after insert on gateways
for each row
execute procedure gateway_inventory_update();

drop materialized view gateway_ledger;

-- :down

create materialized view gateway_ledger as
    select ga.first_block, g.*, l.long_country, l.short_country, l.long_state, l.short_state, l.long_city, l.short_city, l.long_street, l.short_street from
        (select max(block) as last_block, min(block) as first_block, address from gateways group by address) ga
        inner join gateways g on (g.block, g.address) = (ga.last_block, ga.address)
        left join locations l on g.location = l.location;

create unique index gateway_ledger_gateway_idx on gateway_ledger(address);
create index gateway_ledger_first_block_idx on gateway_ledger(first_block);

drop trigger gateway_insert on gateways;
drop function gateway_inventory_update;
drop table gateway_inventory;
