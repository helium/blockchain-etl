-- migrations/1577890272-create-gateway.sql
-- :up

CREATE TABLE gateways (
       block BIGINT NOT NULL references blocks on delete cascade,
       address TEXT NOT NULL,

       owner TEXT NOT NULL,

       location TEXT,

       alpha FLOAT NOT NULL DEFAULT 1.0,
       beta FLOAT NOT NULL DEFAULT 1.0,
       delta INT NOT NULL DEFAULT 1,

       last_poc_challenge BIGINT references blocks(height) on delete set NULL,
       last_poc_onion_key_hash TEXT,

       witnesses jsonb NOT NULL,

       PRIMARY KEY (block, address)
);


CREATE INDEX gateway_owner_idx on gateways(owner);


-- :down

DROP TABLE gateways
