-- migrations/1591302054-gateway_status.sql
-- :up

create type gateway_status_gps as ENUM (
       'good_fix',
       'bad_assert',
       'no_fix',
       'not_asserted'
);

create type gateway_status_online as ENUM (
       'online',
       'offline'
);

create table gateway_status (
       address TEXT NOT NULL references gateway_inventory(address) on delete cascade,

       online gateway_status_online,
       gps gateway_status_gps,
       block BIGINT,

       updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
       PRIMARY KEY(address)
);

create index gateway_status_updated_at_idx on gateway_status(updated_at);

CREATE TRIGGER gateway_status_set_updated_at
BEFORE UPDATE ON gateway_status
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_updated_at();


-- :down

drop trigger gateway_status_set_updated_at;
drop table gateway_status;
drop type gateway_status_online;
drop type gateway_status_gps;
