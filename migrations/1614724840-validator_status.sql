-- migrations/1614724840-validator_status.sql
-- :up

create type validator_status_online as ENUM (
       'online',
       'offline'
);

create table validator_status (
    address TEXT NOT NULL references validator_inventory(address) on delete cascade,
    online validator_status_online,
    block BIGINT,
    peer_timestamp timestamptz,
    listen_addrs jsonb,

    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY(address)
);

create index validator_status_updated_at_idx on validator_status(updated_at);

CREATE TRIGGER validator_status_set_updated_at
BEFORE UPDATE ON validator_status
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_updated_at();


-- :down

drop trigger validator_status_set_updated_at;
drop table validator_status;
drop type validator_status_online;
