-- migrations/1656356441-add_subnetwork_rewards.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'subnetwork_rewards_v1';
