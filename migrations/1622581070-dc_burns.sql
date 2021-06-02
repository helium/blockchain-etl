-- migrations/1622581070-dc_burns.sql
-- :up

CREATE TYPE burn_type as ENUM (
    'fee',
    'state_channel',
    'assert_location',
    'add_gateway',
    'oui',
    'routing'
);

create table dc_burns (
    block BIGINT references blocks,
    transaction_hash TEXT references transactions on delete cascade,
    actor TEXT,
    type burn_type NOT NULL,
    amount BIGINT NOT NULL,
    oracle_price BIGINT,
    
    PRIMARY KEY(actor, transaction_hash, type)
);

create index dc_burns_block_idx on dc_burns(block);

-- :down

drop table dc_burns;
drop type burn_type;