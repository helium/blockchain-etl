-- migrations/1616328011-hotspot_location_search.sql
-- :up
CREATE EXTENSION IF NOT EXISTS postgis;
alter table locations add column geometry geometry(POINT, 4326);


-- :down
alter table locations drop column geometry;
drop extension postgis;
