-- migrations/1614027314-gen_validator_v1.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE 'validator';
ALTER TYPE transaction_type ADD VALUE 'gen_validator_v1';

-- Down migration
-- :down
