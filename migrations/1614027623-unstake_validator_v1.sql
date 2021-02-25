-- migrations/1614027623-unstake_validator_v1.sql
-- :up

ALTER TYPE transaction_type ADD VALUE 'unstake_validator_v1';

-- :down
