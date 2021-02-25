-- migrations/1591654952-price_oracle_v1.sql
-- :up

ALTER TYPE transaction_type ADD VALUE 'price_oracle_v1';


-- Since there is no good way to rmeove an enum entry without knowing
-- what to do with associated transactions transactions, there's
-- really nothing we can do here
-- :down
