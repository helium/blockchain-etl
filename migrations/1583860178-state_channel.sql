-- migrations/1583860178-state_channel.sql
-- :up
-- Up migration
ALTER TYPE transaction_type ADD VALUE 'blockchain_txn_state_channel_open_v1';
ALTER TYPE transaction_type ADD VALUE 'blockchain_txn_state_channel_close_v1';

-- :down
-- Down migration
-- There's nothing really to do here sadly
