-- migrations/1580305069-pending-transactions.sql
-- :up

CREATE OR REPLACE FUNCTION trigger_set_updated_at()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TYPE pending_transaction_status as ENUM (
       'received',
       'pending',
       'failed'
);

CREATE TYPE pending_transaction_nonce_type as ENUM (
       'balance',
       'security',
       'dc'
);

CREATE TABLE pending_transactions (
       created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
       updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
       hash TEXT NOT NULL,
       type transaction_type NOT NULL,
       address TEXT NOT NULL,
       nonce BIGINT NOT NULL,
       nonce_type pending_transaction_nonce_type NOT NULL,
       status pending_transaction_status NOT NULL,
       failed_reason TEXT,
       data BYTEA NOT NULL,

       PRIMARY KEY (hash)
);

CREATE INDEX pending_transaction_created_idx ON pending_transactions(created_at);
CREATE INDEX pending_transaction_nonce_type_idx ON pending_transactions(nonce_type);

CREATE TRIGGER pending_transaction_set_updated_at
BEFORE UPDATE ON pending_transactions
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_updated_at();

-- :down

DROP TABLE pending_transactions;
DROP TYPE pending_transaction_status, pending_transaction_nonce_type;
