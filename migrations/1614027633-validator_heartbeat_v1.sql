-- migrations/1614027633-validator_heartbeat_v1.sql
-- :up

ALTER TYPE transaction_type ADD VALUE 'validator_heartbeat_v1';

-- :down
