-- migrations/1599596004-remove_gps_status.sql
-- :up

alter table gateway_status drop column gps;
drop type gateway_status_gps;


-- :down

create type gateway_status_gps as ENUM (
       'good_fix',
       'bad_assert',
       'no_fix',
       'not_asserted'
);

alter table gateway_status add column gps gateway_status_gps;
