-- migrations/1587049334-pending_txn_fields.sql
-- :up

alter table pending_transactions add column fields jsonb;
alter table pending_transactions alter column address drop not null;

create table pending_transaction_actors (
       actor TEXT NOT NULL,
       actor_role transaction_actor_role NOT NULL,
       transaction_hash TEXT references pending_transactions on delete cascade,

       PRIMARY KEY (actor, actor_role, transaction_hash)
);


-- :down

alter table pending_transactions drop column fields;
drop table pending_transaction_actors;
