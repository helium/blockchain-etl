-- migrations/1628163172-location_city_search_word_order.sql
-- :up

create or replace function location_city_words(l locations) returns text as $$
begin
    return (select string_agg(word, ' ' order by rn)
        from (select word, min(rn) as rn
            from regexp_split_to_table(
                        lower(
                            coalesce(l.long_city, '') || ' ' || coalesce(l.short_city, '') || ' ' || 
                            coalesce(l.long_state, '') || ' ' || coalesce(l.short_state, '') || ' ' ||
                            coalesce(l.long_country, '') || ' ' || coalesce(l.short_country, '') || ' '                            
                        ) , '\s'
                     ) with ordinality x(word, rn) where length(word) >= 3
            group by word) x);
end;
$$ language plpgsql;

update locations set search_city = location_city_words(locations::locations);

-- :down

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

update locations set search_city = location_city_words(locations::locations);
