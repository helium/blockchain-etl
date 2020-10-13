-- migrations/1591562890-gateway_nonce.sql
-- :up

alter table gateways add column nonce bigint;
alter table gateway_inventory add column nonce bigint;

CREATE OR REPLACE FUNCTION gateway_inventory_update()
RETURNS TRIGGER AS $$
BEGIN
  insert into gateway_inventory
         (address, owner, location, alpha, beta, delta, score,
          last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce,
          first_block, last_block)
  VALUES
        (NEW.address, NEW.owner, NEW.location, NEW.alpha, NEW.beta, NEW.delta, NEW.score,
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

-- :down

alter table gateways drop column nonce;
alter table gateway_inventory drop column nonce;

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
