-- migrations/1576304775-create-account.sql
-- :up

CREATE TABLE accounts (
       block_height BIGINT NOT NULL,
       address TEXT NOT NULL,

       dc_balance BIGINT NOT NULL DEFAULT 0,
       security_balance BIGINT NOT NULL DEFAULT 0,
       balance BIGINT NOT NULL DEFAULT 0,

       PRIMARY KEY (block_height, address)
);


-- :down
DROP TABLE accounts
