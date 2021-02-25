-- migrations/1588975207-state_channel.sql
-- :up
ALTER TYPE transaction_type ADD VALUE 'state_channel_open_v1';

-- There's nothing really to do here sadly
-- :down
