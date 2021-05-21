-- migrations/1577890272-create-gateway.sql
-- :up

CREATE TABLE gateways (
       block BIGINT NOT NULL references blocks on delete cascade,
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

       PRIMARY KEY (block, address)
);


CREATE INDEX gateway_owner_idx on gateways(owner);

create materialized view gateway_ledger as
       select * from gateways
       where (block, address) in
             (select max(block) as block, address from gateways group by address);

create unique index gateway_ledger_gateway_idx on gateway_ledger(address);


-- :down

DROP TABLE gateways, gateway_ledger
