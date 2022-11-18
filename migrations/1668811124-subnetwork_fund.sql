-- migrations/1668811124-subnetwork_fund.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'subnetwork_fund_v1';

