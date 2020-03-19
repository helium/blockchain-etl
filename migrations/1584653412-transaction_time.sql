-- migrations/1584653412-transaction_time.sql
-- :up

alter table transactions add column time bigint;

update transactions set time = subquery.time
       from (select height as block, time from blocks) as subquery
where transactions.block=subquery.block;

alter table transactions alter column time set not null;

-- :down

alter table transactions drop column time;
