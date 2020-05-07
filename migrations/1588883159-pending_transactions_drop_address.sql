-- migrations/1588883159-pending_transactions_drop_address.sql
-- :up

alter table pending_transactions drop column address;

-- :down

-- We don't use the address field so we do not re-add the NOT NULL constraint
alter table pending_transactions add column address TEXT;
