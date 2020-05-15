-- migrations/1589558747-pending_transactions_dc_none.sql
-- :up

alter type pending_transaction_nonce_type rename value 'dc' to 'none'

-- :down

alter type pending_transaction_nonce_type rename value 'none' to 'dc'
