-- migrations/1628080839-consensus_failure_member.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'consensus_failure_member';

