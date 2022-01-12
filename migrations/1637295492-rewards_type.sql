-- migrations/1637295492-rewards_type.sql
-- :up

DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'reward_type') THEN
        create type reward_type as ENUM (
            'poc_challenger',
            'poc_challengee',
            'poc_witness',
            'dc_rewards',
            'consensus_rewards',
            'securities_reward'
        );
    END IF;
END
$$;

alter table rewards add column type reward_type;

alter table rewards drop constraint if exists rewards_pkey;

-- :down

alter table rewards drop column type;


