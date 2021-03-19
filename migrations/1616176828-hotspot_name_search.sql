-- migrations/1616176828-hotspot_name_search.sql
-- :up
create index if not exists gateway_search_name_idx on gateway_inventory using GIN(name gin_trgm_ops);


-- :down
drop index gateway_search_name_idx;

