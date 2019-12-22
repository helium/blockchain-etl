-- migrations/1577040141-create-account.sql
-- :up

CREATE TABLE accounts (
       block BIGINT NOT NULL references blocks,
       address TEXT NOT NULL,

       dc_balance BIGINT NOT NULL DEFAULT 0,
       security_balance BIGINT NOT NULL DEFAULT 0,
       balance BIGINT NOT NULL DEFAULT 0,

       PRIMARY KEY (block, address)
);


-- :down
DROP TABLE accounts
