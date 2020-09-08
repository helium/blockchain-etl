-- migrations/1599584290-state_channel_owner.sql
-- :up

-- for opens the otner is the same as the opener
insert into transaction_actors
    (select actor, 'owner' as actor_role, transaction_hash, block from transaction_actors where actor_role = 'sc_opener');

-- for closes pick up the owner from the transactions
insert into transaction_actors
    (select t.fields#>>'{state_channel,owner}' as actor, 'owner' as actor_role, a.transaction_hash, a.block
     from transaction_actors a inner join transactions t on a.transaction_hash = t.hash
     where actor_role = 'sc_closer');


-- :down
-- Down migration
