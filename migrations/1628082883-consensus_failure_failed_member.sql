-- migrations/1628082883-consensus_failure_failed_member.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'consensus_failure_failed_member';
