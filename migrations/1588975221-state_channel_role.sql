-- migrations/1588975221-state_channel_role.sql
-- :up
-- Up migration
ALTER TYPE transaction_actor_role ADD VALUE 'sc_opener';
ALTER TYPE transaction_actor_role ADD VALUE 'sc_closer';

-- :down
-- Down migration
-- Nothing to do here.
