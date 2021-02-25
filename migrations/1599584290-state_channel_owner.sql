-- migrations/1599584290-state_channel_owner.sql
-- :up

insert into transaction_actors
    (select actor, 'owner' as actor_role, transaction_hash, block from transaction_actors where actor_role = 'sc_opener')
on conflict do nothing;

insert into transaction_actors
    (select t.fields#>>'{state_channel,owner}' as actor, 'owner' as actor_role, a.transaction_hash, a.block
     from transaction_actors a inner join transactions t on a.transaction_hash = t.hash
     where actor_role = 'sc_closer')
on conflict do nothing;


