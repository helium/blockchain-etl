-- migrations/1614086419-validators.sql
-- :up

CREATE TYPE validator_status_type as ENUM (
    'staked',
    'cooldown',
    'unstaked'
);

create table validators (
    block BIGINT NOT NULL references blocks(height) on delete cascade,
    address TEXT NOT NULL,
    owner TEXT NOT NULL,
    status validator_status_type NOT NULL,
    stake BIGINT NOT NULL DEFAULT 0,
    nonce BIGINT NOT NULL,
    last_heartbeat BIGINT references blocks(height) on delete cascade,
    version_heartbeat BIGINT NOT NULL,

    primary key (block, address)
);

create table validator_inventory (
    address TEXT NOT NULL,
    owner TEXT NOT NULL,
    status validator_status_type NOT NULL,
    stake BIGINT NOT NULL DEFAULT 0,
    nonce BIGINT NOT NULL,
    last_heartbeat BIGINT references blocks(height) on delete set NULL,
    version_heartbeat BIGINT NOT NULL,

    first_block BIGINT references blocks(height) on delete set NULL,
    last_block BIGINT references blocks(height) on delete set NULL,

    primary key(address)
);

create index validator_inventory_first_block on validator_inventory(first_block);
create index validator_owner_idx on validator_inventory(owner);

CREATE OR REPLACE FUNCTION validator_inventory_update()
 RETURNS TRIGGER AS $$
 BEGIN
   insert into validator_inventory
          (address, owner,
          stake, status, nonce,
          last_heartbeat, version_heartbeat,
          first_block, last_block)
   VALUES
         (NEW.address, NEW.owner,
         NEW.stake, NEW.status, NEW.nonce,
         NEW.last_heartbeat, NEW.version_heartbeat,
         NEW.block, NEW.block
         )
   ON CONFLICT (address) DO UPDATE SET
        stake = EXCLUDED.stake,
        status = EXCLUDED.status,
        owner = EXCLUDED.owner,
        nonce = EXCLUDED.nonce,
        last_heartbeat = EXCLUDED.last_heartbeat,
        version_heartbeat = EXCLUDED.version_heartbeat,
        last_block = EXCLUDED.last_block;
  RETURN NEW;
 END;
 $$ LANGUAGE plpgsql;

create trigger validator_insert
       after insert on validators
       for each row
       execute procedure validator_inventory_update();

-- :down

drop trigger validator_insert on validators;
drop function validator_inventory_update;
drop table validator_inventory;
drop table validators;
