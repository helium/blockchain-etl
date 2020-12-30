-- migrations/1609338698-packets.sql
-- :up

create table packets (
       block bigint not null references blocks(height) on delete cascade,
       transaction_hash text not null references transactions(hash),
       time bigint not null,
       gateway text not null,
       num_packets bigint not null,
       num_dcs bigint not null,

       primary key(block, transaction_hash, gateway)
);


create type packet_entry as (client TEXT, type TEXT, num_packets bigint, num_dcs bigint);
create or replace function insert_packets() returns void as $$
declare
    txn RECORD;
begin
    for txn in
        select *
        from transactions where type = 'state_channel_close_v1'
        order by block asc
    loop
        insert into packets (block, transaction_hash, time, gateway, num_packets, num_dcs)
        select 
            txn.block, txn.hash, txn.time, client as gateway, 
            sum(num_packets)::bigint as num_packets, 
            sum(num_dcs)::bigint as num_dcs
        from jsonb_populate_recordset(null::packet_entry, txn.fields#>'{state_channel, summaries}')
        group by client;
    end loop;
end; $$
language plpgsql;

select insert_packets();

create index packets_block_idx on packets(block);
create index packets_gateway_idx on packets(gateway);

-- :down

drop function insert_packets;
drop table packets;
drop type packet_entry;