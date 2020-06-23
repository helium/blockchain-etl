-- migrations/1592947898-transaction_time_idx.sql
-- :up

create index transaction_time_idx on transactions(time);

-- :down

drop index transaction_time_idx;
