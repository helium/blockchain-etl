-- migrations/1614027603-stake_validator_v1.sql
-- :up

alter type pending_transaction_nonce_type add value if not exists 'validator';
ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'stake_validator_v1';

