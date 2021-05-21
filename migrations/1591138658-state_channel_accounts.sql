-- migrations/1591138658-state_channel_accounts.sql
-- :up

insert into transaction_actors
    (select actor, 'payer' as actor_role, transaction_hash, block from transaction_actors where actor_role = 'sc_opener');

insert into transaction_actors
    (select actor, 'payee' as actor_role, transaction_hash, block from transaction_actors where actor_role = 'sc_closer');

