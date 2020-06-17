-- migrations/1592407175-pending_pk_created_at.sql
-- :up

-- first we drop the foreing key dependency on the primary key in pending_transactions
alter table pending_transaction_actors drop constraint pending_transaction_actors_transaction_hash_fkey;
-- drop the primary key constraint since we need to allow multiple txns with the same has.
alter table pending_transactions drop constraint pending_transactions_pkey;
-- and make created_at the primary key
alter table pending_transactions add primary key (created_at);
-- drop the old created_at index
drop index pending_transaction_created_idx;
-- add a hash index
create index pending_transaction_hash_idx on pending_transactions(hash);
-- Fix up pending_txn_actors with a new reference
alter table pending_transaction_actors add column created_at TIMESTAMPTZ references pending_transactions(created_at);
-- initialize created_ats for the pending actors
update pending_transaction_actors set created_at=subquery.created_at
       from (select hash, created_at from pending_transactions p) as subquery
where pending_transaction_actors.transaction_hash=subquery.hash;
-- Drop the pending actor primary key
alter table pending_transaction_actors drop constraint pending_transaction_actors_pkey;
-- And recreate with actor, role and created_at
alter table pending_transaction_actors add primary key(actor, actor_role, created_at);

-- :down

-- The down migration may fail since the uniqueness constraing on txn
-- hash was removed. So we can't really roll this back.
