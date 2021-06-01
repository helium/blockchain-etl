-- migrations/1593644594-location_city_search.sql
-- :up

alter table locations add column search_city text;

create or replace function location_city_words(l locations) returns text as $$
begin
    return (select string_agg(distinct word, ' ')
            from regexp_split_to_table(
                    lower(
                        coalesce(l.long_city, '') || ' ' || coalesce(l.short_city, '') || ' '
                    ) , '\s'
                 ) as word where length(word) >= 3);
end;
$$ language plpgsql;

create or replace function location_search_city_update()
returns trigger as $$
begin
    NEW.search_city := location_city_words(NEW);
    return NEW;
end;
$$ language plpgsql;

update locations set search_city = location_city_words(locations::locations);
create index location_search_city_idx on locations using GIN(search_city gin_trgm_ops);

create trigger location_update_search_city
before insert on locations
for each row
execute procedure location_search_city_update();

CREATE OR REPLACE FUNCTION public.last_agg ( anyelement, anyelement )
RETURNS anyelement LANGUAGE SQL IMMUTABLE STRICT AS $$
        SELECT $2;
$$;

CREATE AGGREGATE public.LAST (
       sfunc    = public.last_agg,
        basetype = anyelement,
        stype    = anyelement
);


-- :down
alter table locations drop column search_city;
drop trigger location_update_search_city on locations;
drop function location_search_city_update;
drop function location_city_words;
