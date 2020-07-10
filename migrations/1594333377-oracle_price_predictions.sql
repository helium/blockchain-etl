-- migrations/1594333377-oracle_price_predictions.sql
-- :up

create table oracle_price_predictions (
       time BIGINT NOT NULL,
       price BIGINT NOT NULL,

       PRIMARY KEY(time)
);

-- :down

drop table oracle_price_predictions;
