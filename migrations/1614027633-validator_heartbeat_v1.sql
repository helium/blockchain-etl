-- migrations/1614027633-validator_heartbeat_v1.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'validator_heartbeat_v1';

