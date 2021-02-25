-- migrations/1614027603-stake_validator_v1.sql
-- :up

alter type pending_transaction_nonce_type add value 'validator';
ALTER TYPE transaction_type ADD VALUE 'stake_validator_v1';

-- :down
