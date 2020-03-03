-- migrations/1583261388-data-credit-role.sql
-- :up
-- Up migration
ALTER TYPE transaction_actor_role ADD VALUE 'sc_opener';
ALTER TYPE transaction_actor_role ADD VALUE 'sc_closer';

-- :down
-- Down migration
DROP TYPE transaction_actor_role;
