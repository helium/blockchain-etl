-- migrations/1656356440-add_subnetwork.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'add_subnetwork_v1';
