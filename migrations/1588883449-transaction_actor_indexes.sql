-- migrations/1588883449-transaction_actor_indexes.sql
-- :up

create index transaction_actor_role_idx on transaction_actors(actor_role);
create index transaction_actor_actor_idx on transaction_actors(actor);

-- :down

drop index transaction_actor_role_idx;
drop index transaction_actor_actor_idx;
