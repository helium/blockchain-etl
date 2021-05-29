-- migrations/1622207946-gateway_last_assert.sql
-- :up

alter table gateway_inventory
    add column last_assert BIGINT;

CREATE OR REPLACE FUNCTION gateway_inventory_last_assert_update()
RETURNS TRIGGER AS $$
BEGIN
    with last_assert as (
        select block 
        from transaction_actors a inner join transactions t on a.transaction_hash = t.hash
        where a.actor = NEW.address and a.actor_role = 'gateway'
        and t.type = ANY('{assert_location_v1, assert_location_v2}')
        order by a.block desc limit 1
    )
    update gateway_inventory set 
        last_assert = (select block from last_assert)
    where address = NEW.address;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER gateway_inventory_location_trigger
AFTER UPDATE ON gateway_inventory
FOR EACH ROW
WHEN (OLD.location IS DISTINCT FROM NEW.location)
EXECUTE PROCEDURE gateway_inventory_last_assert_update();

-- :down

drop trigger gateway_inventory_location_trigger;
drop function gateway_inventory_last_assert_update;

alter table gateway_inventory
    drop column last_assert;