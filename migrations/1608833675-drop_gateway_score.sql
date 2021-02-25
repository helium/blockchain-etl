-- migrations/1608833675-drop_gateway_score.sql
-- :up

alter table gateways
drop column if exists alpha,
drop column if exists beta,
drop column if exists delta,
drop column if exists score;

alter table gateway_inventory
drop column if exists alpha,
drop column if exists beta,
drop column if exists delta,
drop column if exists score;

CREATE OR REPLACE FUNCTION gateway_inventory_update()
RETURNS TRIGGER AS $$
BEGIN
  insert into gateway_inventory
         (address, name, owner, location,
          last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce,
          first_block, last_block, first_timestamp)
  VALUES
        (NEW.address, NEW.name, NEW.owner, NEW.location,
        NEW.last_poc_challenge, NEW.last_poc_onion_key_hash, NEW.witnesses, NEW.nonce,
        NEW.block, NEW.block, to_timestamp(NEW.time)
        )
  ON CONFLICT (address) DO UPDATE SET
       owner = EXCLUDED.owner,
       location = EXCLUDED.location,
       last_poc_challenge = EXCLUDED.last_poc_challenge,
       last_poc_onion_key_hash = EXCLUDED.last_poc_onion_key_hash,
       witnesses = EXCLUDED.witnesses,
       nonce = EXCLUDED.nonce,
       last_block = EXCLUDED.last_block;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


