-- migrations/1583473459-payment_v2.sql
-- :up

ALTER TYPE transaction_type ADD VALUE 'payment_v2';

-- :down

-- rename the existing type
ALTER TYPE transaction_type RENAME TO transaction_type_old;

-- create the new type
CREATE TYPE transaction_type as ENUM (
        'coinbase_v1',
        'security_coinbase_v1',
        'oui_v1',
        'gen_gateway_v1',
        'routing_v1',
        'payment_v1',
        'security_exchange_v1',
        'consensus_group_v1',
        'add_gateway_v1',
        'assert_location_v1',
        'create_htlc_v1',
        'redeem_htlc_v1',
        'poc_request_v1',
        'poc_receipts_v1',
        'vars_v1',
        'rewards_v1',
        'token_burn_v1',
        'dc_coinbase_v1',
        'token_burn_exchange_rate_v1'
);

-- update the column to use the new type
ALTER TABLE transactions ALTER COLUMN type TYPE transaction_type USING type::text::transaction_type;
ALTER TABLE pending_transactions ALTER COLUMN type TYPE transaction_type USING type::text::transaction_type;

-- remove the old type
DROP TYPE transaction_type_old;
