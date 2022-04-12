-- migrations/1649804744-location_geo_index.sql
-- :up

create index if not exists location_geometry_idx on locations using GIST((geometry::geography));

-- :down

drop index location_geometry_idx;