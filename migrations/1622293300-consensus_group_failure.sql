-- migrations/1616099811-consensus_group_failure.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'consensus_group_failure_v1';

