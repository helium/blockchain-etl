-- migrations/1577040141-create-account.sql
-- :up

CREATE TABLE accounts (
       block BIGINT NOT NULL references blocks on delete cascade,
       address TEXT NOT NULL,

       dc_balance BIGINT NOT NULL DEFAULT 0,
       dc_nonce BIGINT NOT NULL DEFAULT 0,

       security_balance BIGINT NOT NULL DEFAULT 0,
       security_nonce BIGINT NOT NULL DEFAULT 0,

       balance BIGINT NOT NULL DEFAULT 0,
       nonce BIGINT NOT NULL DEFAULT 0,

       PRIMARY KEY (block, address)
);


-- A collapsed view of accounts that includes all known accounts at the
-- highest block
create materialized view account_ledger as
       select * from accounts
       where (block, address) in
             (select max(block) as block, address from accounts group by address);

-- This allows a quick lookup of an account by it's address. Any index on
-- the materialized view also allows a concurrent view refresh
create unique index account_ledger_address_idx on account_ledger(address);

-- :down
DROP TABLE accounts, account_ledger
