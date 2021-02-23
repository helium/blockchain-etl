-- migrations/1591133143-account_inventory.sql
-- :up

create table account_inventory (
       address TEXT NOT NULL,

       balance BIGINT NOT NULL,
       nonce BIGINT NOT NULL,

       dc_balance BIGINT NOT NULL,
       dc_nonce BIGINT NOT NULL,

       security_balance BIGINT NOT NULL,
       security_nonce BIGINT NOT NULL,

       first_block BIGINT references blocks(height) on delete set NULL,
       last_block BIGINT references blocks(height) on delete set NULL,

       PRIMARY KEY (address)
);

create index account_inventory_first_block on account_inventory(first_block);

insert into account_inventory
       select a.address,
              a.balance,
              a.nonce,
              a.dc_balance,
              a.dc_nonce,
              a.security_balance,
              a.security_nonce,
              aa.first_block,
              aa.last_block
        from
             (select max(block) as last_block, min(block) as first_block, address from accounts group by address) aa
              inner join accounts a on (a.block, a.address) = (aa.last_block, aa.address);

CREATE OR REPLACE FUNCTION account_inventory_update()
 RETURNS TRIGGER AS $$
 BEGIN
   insert into account_inventory
          (address,
          balance, nonce,
          dc_balance, dc_nonce,
          security_balance, security_nonce,
          first_block, last_block)
   VALUES
         (NEW.address,
         NEW.balance, NEW.nonce,
         NEW.dc_balance, NEW.dc_nonce,
         NEW.security_balance, NEW.security_nonce,
         NEW.block, NEW.block
         )
   ON CONFLICT (address) DO UPDATE SET
        balance = EXCLUDED.balance,
        nonce = EXCLUDED.nonce,
        dc_balance = EXCLUDED.dc_balance,
        dc_nonce = EXCLUDED.dc_nonce,
        security_balance = EXCLUDED.security_balance,
        security_nonce = EXCLUDED.security_nonce,
        last_block = EXCLUDED.last_block;
  RETURN NEW;
 END;
 $$ LANGUAGE plpgsql;

create trigger account_insert
       after insert on accounts
       for each row
       execute procedure account_inventory_update();

drop materialized view account_ledger;
alter table accounts drop column timestamp;


-- :down

create materialized view account_ledger as
    select aa.first_block, a.*  from
        (select max(block) as last_block, min(block) as first_block, address from accounts group by address) aa
        inner join accounts a on (a.block, a.address) = (aa.last_block, aa.address);

create unique index account_ledger_address_idx on account_ledger(address);
create index account_ledger_first_block_idx on account_ledger(first_block);

drop trigger account_insert on accounts;
drop function account_inventory_update;
drop table account_inventory;
