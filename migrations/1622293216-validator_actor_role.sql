-- migrations/1622293216-validator_actor_role.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'validator';

