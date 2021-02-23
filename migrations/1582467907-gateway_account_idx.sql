-- migrations/1582467907-gateway_account_idx.sql
-- :up

CREATE INDEX IF NOT EXISTS gateway_block_idx on gateways(block);
CREATE INDEX IF NOT EXISTS gateway_address_idx on gateways(address);

CREATE INDEX IF NOT EXISTS account_block_idx on accounts(block);
CREATE INDEX IF NOT EXISTS account_address_idx on accounts(address);

-- :down

DROP INDEX IF EXISTS gateway_block_idx, gateway_address_idx;
DROP INDEX IF EXISTS account_block_idx, account_address_idx;
