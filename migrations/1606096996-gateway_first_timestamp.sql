-- migrations/1606096996-gateway_first_timestamp.sql
-- :up

alter table gateways add column time BIGINT;
alter table gateway_inventory add column first_timestamp timestamptz;

update gateway_inventory i
set first_timestamp = (select timestamp from blocks where height = i.first_block)
where first_timestamp is null;

create index gateway_inventory_first_timestamp on gateway_inventory(first_timestamp);

CREATE OR REPLACE FUNCTION gateway_inventory_update()
RETURNS TRIGGER AS $$
BEGIN
  insert into gateway_inventory
         (address, name, owner, location, alpha, beta, delta, score,
          last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce,
          first_block, last_block, first_timestamp)
  VALUES
        (NEW.address, NEW.name, NEW.owner, NEW.location, NEW.alpha, NEW.beta, NEW.delta, NEW.score,
        NEW.last_poc_challenge, NEW.last_poc_onion_key_hash, NEW.witnesses, NEW.nonce,
        NEW.block, NEW.block, to_timestamp(NEW.time)
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
       nonce = EXCLUDED.nonce,
       last_block = EXCLUDED.last_block;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


-- :down


CREATE OR REPLACE FUNCTION gateway_inventory_update()
RETURNS TRIGGER AS $$
BEGIN
  insert into gateway_inventory
         (address, name, owner, location, alpha, beta, delta, score,
          last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce,
          first_block, last_block)
  VALUES
        (NEW.address, NEW.name, NEW.owner, NEW.location, NEW.alpha, NEW.beta, NEW.delta, NEW.score,
        NEW.last_poc_challenge, NEW.last_poc_onion_key_hash, NEW.witnesses, NEW.nonce,
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
       nonce = EXCLUDED.nonce,
       last_block = EXCLUDED.last_block;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


drop index gateway_inventory_first_timestamp;
alter table gateways drop column time;
alter table gateway_inventory drop column first_timestamp;
