-- migrations/1589565332-transaction_role_receiver.sql
-- :up

alter type transaction_actor_role add value 'packet_receiver';

