-- migrations/1656430115-account_subnetwork.sql
-- :up

alter table accounts 
    add column mobile_balance BIGINT,
    add column iot_balance BIGINT;

alter table account_inventory
    add column mobile_balance BIGINT,
    add column iot_balance BIGINT;

CREATE OR REPLACE FUNCTION account_inventory_update()
 RETURNS TRIGGER AS $$
 BEGIN
   insert into account_inventory
          (address,
          balance, nonce,
          dc_balance, dc_nonce,
          security_balance, security_nonce,
          staked_balance,
          mobile_balance, iot_balance,
          first_block, last_block)
   VALUES
         (NEW.address,
         NEW.balance, NEW.nonce,
         NEW.dc_balance, NEW.dc_nonce,
         NEW.security_balance, NEW.security_nonce,
         NEW.staked_balance,
         NEW.mobile_balance, NEW.iot_balance,
         NEW.block, NEW.block
         )
   ON CONFLICT (address) DO UPDATE SET
        balance = EXCLUDED.balance,
        nonce = EXCLUDED.nonce,
        dc_balance = EXCLUDED.dc_balance,
        dc_nonce = EXCLUDED.dc_nonce,
        security_balance = EXCLUDED.security_balance,
        security_nonce = EXCLUDED.security_nonce,
        staked_balance = EXCLUDED.staked_balance,
        mobile_balance = EXCLUDED.mobile_balance,
        iot_balance = EXCLUDED.iot_balance,
        last_block = EXCLUDED.last_block;
  RETURN NEW;
 END;
 $$ LANGUAGE plpgsql;



-- :down

CREATE OR REPLACE FUNCTION account_inventory_update()
 RETURNS TRIGGER AS $$
 BEGIN
   insert into account_inventory
          (address,
          balance, nonce,
          dc_balance, dc_nonce,
          security_balance, security_nonce,
          staked_balance,
          first_block, last_block)
   VALUES
         (NEW.address,
         NEW.balance, NEW.nonce,
         NEW.dc_balance, NEW.dc_nonce,
         NEW.security_balance, NEW.security_nonce,
         NEW.staked_balance,
         NEW.block, NEW.block
         )
   ON CONFLICT (address) DO UPDATE SET
        balance = EXCLUDED.balance,
        nonce = EXCLUDED.nonce,
        dc_balance = EXCLUDED.dc_balance,
        dc_nonce = EXCLUDED.dc_nonce,
        security_balance = EXCLUDED.security_balance,
        security_nonce = EXCLUDED.security_nonce,
        staked_balance = EXCLUDED.staked_balance,
        last_block = EXCLUDED.last_block;
  RETURN NEW;
 END;
 $$ LANGUAGE plpgsql;


alter table accounts 
    drop column mobile_balance,
    drop column iot_balance;

alter table account_inventory
    drop column mobile_balance,
    drop column iot_balance;

