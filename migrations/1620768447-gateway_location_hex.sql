-- migrations/1620768447-gateway_location_hex.sql
-- :up

alter table gateways
    add column location_hex TEXT;

alter table gateway_inventory
    add column location_hex TEXT;

CREATE OR REPLACE FUNCTION gateway_inventory_update()
RETURNS TRIGGER AS $$
BEGIN
  insert into gateway_inventory as g
         (address, name, owner, location,
          last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce,
          first_block, last_block, first_timestamp, reward_scale,
          elevation, gain, location_hex)
  VALUES
        (NEW.address, NEW.name, NEW.owner, NEW.location,
        NEW.last_poc_challenge, NEW.last_poc_onion_key_hash, NEW.witnesses, NEW.nonce,
        NEW.block, NEW.block, to_timestamp(NEW.time), NEW.reward_scale,
        NEW.elevation, NEW.gain,
        NEW.location_hex
        )
  ON CONFLICT (address) DO UPDATE SET
       owner = EXCLUDED.owner,
       location = EXCLUDED.location,
       last_poc_challenge = EXCLUDED.last_poc_challenge,
       last_poc_onion_key_hash = EXCLUDED.last_poc_onion_key_hash,
       witnesses = EXCLUDED.witnesses,
       nonce = EXCLUDED.nonce,
       last_block = EXCLUDED.last_block,
       reward_scale = COALESCE(EXCLUDED.reward_scale, g.reward_scale),
       elevation = EXCLUDED.elevation,
       gain = EXCLUDED.gain,
       location_hex = EXCLUDED.location_hex;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- :down

CREATE OR REPLACE FUNCTION gateway_inventory_update()
RETURNS TRIGGER AS $$
BEGIN
  insert into gateway_inventory as g
         (address, name, owner, location,
          last_poc_challenge, last_poc_onion_key_hash, witnesses, nonce,
          first_block, last_block, first_timestamp, reward_scale,
          elevation, gain)
  VALUES
        (NEW.address, NEW.name, NEW.owner, NEW.location,
        NEW.last_poc_challenge, NEW.last_poc_onion_key_hash, NEW.witnesses, NEW.nonce,
        NEW.block, NEW.block, to_timestamp(NEW.time), NEW.reward_scale,
        NEW.elevation, NEW.gain
        )
  ON CONFLICT (address) DO UPDATE SET
       owner = EXCLUDED.owner,
       location = EXCLUDED.location,
       last_poc_challenge = EXCLUDED.last_poc_challenge,
       last_poc_onion_key_hash = EXCLUDED.last_poc_onion_key_hash,
       witnesses = EXCLUDED.witnesses,
       nonce = EXCLUDED.nonce,
       last_block = EXCLUDED.last_block,
       reward_scale = COALESCE(EXCLUDED.reward_scale, g.reward_scale),
       elevation = EXCLUDED.elevation,
       gain = EXCLUDED.gain;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

alter table gateways
    drop column location_hex;

alter table gateway_inventory
    drop column location_hex;
