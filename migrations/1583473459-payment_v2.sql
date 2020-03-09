-- migrations/1583473459-payment_v2.sql
-- :up

ALTER TYPE transaction_type ADD VALUE 'payment_v2';

-- :down

-- Since there is no good way to rmeove an enum entry without knowing
-- what to do with associated transactions transactions, there's
-- really nothing we can do here
