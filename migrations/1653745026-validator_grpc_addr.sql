-- migrations/1653745026-validator_grpc_addr.sql
-- :up

alter table validator_status
    add column grpc_addr TEXT;

-- :down

alter table validator_status
    drop column grpc_addr;
