-- migrations/1656356443-add_subnetwork_server.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'subnetwork_key';
