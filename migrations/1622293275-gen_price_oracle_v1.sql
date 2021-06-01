-- migrations/1614288925-gen_price_oracle_v1.sql
-- :up

ALTER TYPE transaction_type ADD VALUE IF NOT EXISTS 'gen_price_oracle_v1';
