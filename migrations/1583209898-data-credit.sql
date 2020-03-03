-- migrations/1583209898-data-credit.sql
-- :up
-- Up migration
ALTER TYPE transaction_type ADD VALUE 'blockchain_txn_state_channel_open_v1';
ALTER TYPE transaction_type ADD VALUE 'blockchain_txn_state_channel_close_v1';

-- :down
-- Down migration
-- XXX: Unsure what this actually needs to be? I don't think we should be dropping the whole transaction_type enum
-- here
DROP TYPE transaction_type;
