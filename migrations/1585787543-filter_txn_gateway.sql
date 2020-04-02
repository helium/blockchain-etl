-- migrations/1585787543-filter_txn_gateway.sql
-- :up

create function txn_filter_gateway_activity(gw text, type transaction_type, fields jsonb) returns jsonb as $$
begin
    case
        when type = 'rewards_v1' then
            return jsonb_set(fields, '{rewards}', (select jsonb_agg(x) from jsonb_to_recordset(fields#>'{rewards}') as x(account text, amount bigint, type text, gateway text) where gateway = gw));
        else
            return fields;
    end case;
end; $$
language plpgsql;

-- :down

drop function txn_filter_gateway_activity;
