-- :up

CREATE TABLE blocks (
       height BIGINT NOT NULL,

       time BIGINT NOT NULL,
       timestamp TIMESTAMPTZ NOT NULL,
       prev_hash TEXT,
       block_hash TEXT NOT NULL,
       transaction_count INT NOT NULL,
       hbbft_round BIGINT NOT NULL,
       election_epoch BIGINT NOT NULL,
       epoch_start BIGINT NOT NULL,
       rescue_signature TEXT NOT NULL,

       PRIMARY KEY(height)
);

CREATE TABLE block_signatures (
       block BIGINT NOT NULL references blocks on delete cascade,
       signer TEXT NOT NULL,
       signature TEXT NOT NULL,

       PRIMARY KEY(block, signer)
);

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
       block BIGINT NOT NULL references blocks on delete cascade,
       hash TEXT NOT NULL,
       type transaction_type NOT NULL,
       fields jsonb NOT NULL,

       PRIMARY KEY (hash)
);

CREATE INDEX transaction_type_idx on transactions(type);
CREATE INDEX transaction_block_idx on transactions(block);

CREATE TYPE transaction_actor_role as ENUM (
       'payee',
       'payer',
       'owner',
       'gateway',
       'reward_gateway',
       'challenger',
       'challengee',
       'witness',
       'consensus_member',
       'escrow'
);


CREATE TABLE transaction_actors (
       actor TEXT NOT NULL,
       actor_role transaction_actor_role NOT NULL,
       transaction_hash TEXT references transactions on delete cascade,

       PRIMARY KEY (actor, actor_role, transaction_hash)
);


-- :down

DROP TABLE blocks, block_signatures, transactions, transaction_actors;
DROP TYPE transaction_type, transaction_actor_role;
