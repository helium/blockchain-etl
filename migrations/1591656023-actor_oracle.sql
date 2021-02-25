-- migrations/1591656023-actor_oracle.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE 'oracle';


-- Since there is no good way to rmeove an enum entry without knowing
-- what to do with associated transactions transactions, there's
-- really nothing we can do here
-- :down
