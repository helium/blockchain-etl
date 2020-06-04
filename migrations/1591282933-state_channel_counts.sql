-- migrations/1591282933-state_channel_counts.sql
-- :up

create or replace function state_channel_counts(type transaction_type, fields jsonb, out num_packets numeric, out num_dcs numeric) as $$
begin
    case
        when type = 'state_channel_close_v1' then
            select into num_packets, num_dcs sum(x.num_packets), sum(x.num_dcs)
            from jsonb_to_recordset(fields#>'{state_channel, summaries}') as x(owner TEXT, client TEXT, num_dcs BIGINT, location TEXT, num_packets BIGINT);
        else
            num_packets := 0;
            num_dcs := 0;
    end case;
end;
 $$ language plpgsql;

-- :down

drop function state_channel_counts;
