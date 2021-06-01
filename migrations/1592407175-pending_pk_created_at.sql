-- migrations/1592407175-pending_pk_created_at.sql
-- :up

alter table pending_transaction_actors drop constraint pending_transaction_actors_transaction_hash_fkey;
alter table pending_transactions drop constraint pending_transactions_pkey;
alter table pending_transactions add primary key (created_at);
drop index pending_transaction_created_idx;
create index pending_transaction_hash_idx on pending_transactions(hash);
alter table pending_transaction_actors add column created_at TIMESTAMPTZ references pending_transactions(created_at);
update pending_transaction_actors set created_at=subquery.created_at
       from (select hash, created_at from pending_transactions p) as subquery
where pending_transaction_actors.transaction_hash=subquery.hash;
alter table pending_transaction_actors drop constraint pending_transaction_actors_pkey;
alter table pending_transaction_actors add primary key(actor, actor_role, created_at);

