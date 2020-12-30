-- migrations/1609356128-jsonb_merge_agg.sql
-- :up

create aggregate jsonb_merge_agg(jsonb)
(
    sfunc = jsonb_concat,
    stype = jsonb,
    initcond = '{}'
);


-- :down

drop aggregate if exists jsonb_merge_agg(jsonb);
