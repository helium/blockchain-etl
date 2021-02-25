-- migrations/1614027623-unstake_validator_v1.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'unstake_validator_v1';

