-- migrations/1593782722-consolidate_city_search.sql
-- :up

alter table locations drop column search;
drop trigger location_update_search on locations;
drop function location_search_update;
drop function location_words;

create or replace function location_city_words(l locations) returns text as $$
begin
    return (select string_agg(distinct word, ' ')
            from regexp_split_to_table(
                    lower(
                        coalesce(l.long_city, '') || ' ' || coalesce(l.short_city, '') || ' ' ||
                        coalesce(l.long_state, '') || ' ' || coalesce(l.short_state, '') || ' ' ||
                        coalesce(l.long_country, '') || ' ' || coalesce(l.short_country, '') || ' '
                    ) , '\s'
                 ) as word where length(word) >= 3);
end;
$$ language plpgsql;
update locations set search_city = location_city_words(locations::locations);

alter table locations add column city_id text;
create or replace function location_city_id(l locations) returns text as $$
begin
    return lower(coalesce(l.long_city, '') || coalesce(l.long_state, '') || coalesce(l.long_country, ''));
end;
$$ language plpgsql;
update locations set city_id = location_city_id(locations::locations);

create or replace function location_city_id_update()
returns trigger as $$
begin
    NEW.city_id := location_city_id(NEW);
    return NEW;
end;
$$ language plpgsql;

create index location_city_id_idx on locations(city_id);

create trigger location_update_city_id
before insert on locations
for each row
execute procedure location_city_id_update();


-- :down

drop trigger location_update_city_id on locations;
drop function location_city_id_update;
drop function location_city_id;

alter table locations add column search text;
alter table locations drop column city_id;

create or replace function location_words(l locations) returns text as $$
begin
    return (select string_agg(distinct word, ' ')
            from regexp_split_to_table(
                    lower(
                        coalesce(l.long_city, '') || ' ' || coalesce(l.short_city, '') || ' ' ||
                        coalesce(l.long_state, '') || ' ' || coalesce(l.short_state, '') || ' ' ||
                        coalesce(l.long_country, '') || ' ' || coalesce(l.short_country, '') || ' ' ||
                        coalesce(l.long_street, '') || ' ' || coalesce(l.short_street, '')
                    ) , '\s'
                 ) as word where length(word) >= 3);
end;
$$ language plpgsql;

create or replace function location_search_update()
returns trigger as $$
begin
    NEW.search := location_words(NEW);
    return NEW;
end;
$$ language plpgsql;

update locations set search = location_words(locations::locations);
create index location_search_idx on locations using GIN(search gin_trgm_ops);

create trigger location_update_search
before insert on locations
for each row
execute procedure location_search_update();
