-- :up

CREATE TABLE blocks (
       block_height BIGINT NOT NULL,

       prev_hash BYTEA,
       time BIGINT NOT NULL,
       hbbft_round BIGINT NOT NULL,
       election_epoch BIGINT NOT NULL,
       epoch_start BIGINT NOT NULL,
       rescue_signature BYTEA NOT NULL,

       PRIMARY KEY(block_height)
);

CREATE TABLE block_signatures (
       block_height BIGINT NOT NULL,
       signer TEXT NOT NULL,
       signature BYTEA NOT NULL,

       PRIMARY KEY(block_height)
);

-- Types are created outside of a transaction. Drop it in case it
-- already exists :-/
DROP TYPE transaction_type;
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

CREATE TABLE transactions (
       block_height BIGINT NOT NULL,
       hash BYTEA NOT NULL,
       type transaction_type NOT NULL,
       fields jsonb NOT NULL,

       PRIMARY KEY (block_height, hash)
);

CREATE TABLE transaction_actors (
       actor TEXT NOT NULL,
       transaction_hash BYTEA NOT NULL,

       PRIMARY KEY (actor)
);


-- :down

DROP TABLE blocks, block_signatures, transactions, transaction_actors;
DROP TYPE transaction_type;
