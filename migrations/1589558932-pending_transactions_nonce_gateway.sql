-- migrations/1589558932-pending_transactions_nonce_gateway.sql
-- :up

alter type pending_transaction_nonce_type add value 'gateway'

