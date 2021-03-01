-- migrations/1614027603-stake_validator_v1.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'stake_validator_v1';

