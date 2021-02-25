-- migrations/1593610727-location_search.sql
-- :up

CREATE EXTENSION pg_trgm;

update locations set long_street = null where long_street = 'unknown';
update locations set short_street = null where short_street = 'unknown';
update locations set long_city = null where long_city = 'unknown';
update locations set short_city = null where short_city = 'unknown';
update locations set long_state = null where long_state = 'unknown';
update locations set short_state = null where short_state = 'unknown';
update locations set long_country = null where long_country = 'unknown';
update locations set short_country = null where short_country = 'unknown';
alter table locations add column search text;
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


-- :down

alter table locations drop column search;
drop trigger location_update_search on locations;
drop function location_search_update;
drop function location_words;
drop extension pg_trgm;
