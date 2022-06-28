-- migrations/1656356440-network_token.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'add_subnetwork_v1';
ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'subnetwork_rewards_v1';

ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'reward_server';
ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'subnetwork_key';
