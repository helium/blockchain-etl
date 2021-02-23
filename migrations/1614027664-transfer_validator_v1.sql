-- migrations/1614027664-transfer_validator_v1.sql
-- :up
-- Up migration

ALTER TYPE transaction_type ADD VALUE 'transfer_validator_stake_v1';

-- :down
-- Down migration
