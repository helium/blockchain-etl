-- migrations/1607123758-transfer_hotspot.sql
-- :up

ALTER TYPE transaction_type ADD VALUE 'transfer_hotspot_v1';

-- really nothing we can do here
-- :down
