-- migrations/1614947350-router_actor_role.sql
-- :up

ALTER TYPE transaction_actor_role ADD VALUE IF NOT EXISTS 'router';

