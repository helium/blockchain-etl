-- migrations/1614027664-transfer_validator_v1.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'transfer_validator_stake_v1';

