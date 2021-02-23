-- migrations/1614027603-stake_validator_v1.sql
-- :up
-- Up migration

ALTER TYPE transaction_type ADD VALUE 'stake_validator_v1';

-- :down
-- Down migration
