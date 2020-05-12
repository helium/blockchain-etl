-- migrations/1588975207-state_channel.sql
-- :up
-- Up migration
ALTER TYPE transaction_type ADD VALUE 'state_channel_open_v1';

-- :down
-- Down migration
-- There's nothing really to do here sadly
