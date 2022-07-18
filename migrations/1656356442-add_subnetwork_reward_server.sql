-- migrations/1656356442-add_subnetwork_reward_server.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'reward_server';
