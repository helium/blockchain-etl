-- migrations/1591660917-oracle_prices.sql
-- :up

create table oracle_prices (
       block BIGINT NOT NULL references blocks(height) on delete cascade,
       price BIGINT NOT NULL
);

create index oracle_prices_block_idx on oracle_prices(block);


-- :down

drop table oracle_prices;
