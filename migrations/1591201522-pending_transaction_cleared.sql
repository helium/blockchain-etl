-- migrations/1591201522-pending_transaction_cleared.sql
-- :up

ALTER TYPE pending_transaction_status ADD VALUE 'cleared';

-- There's nothing really to do here sadly
-- :down
