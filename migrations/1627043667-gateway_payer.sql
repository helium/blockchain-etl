-- migrations/1627043667-gateway_owner.sql
-- :up

alter table gateway_inventory
    add column payer TEXT;

CREATE OR REPLACE FUNCTION gateway_inventory_on_insert()
RETURNS TRIGGER AS $$
BEGIN
    UPDATE gateway_inventory SET payer = (
        select fields->>'payer' 
        from transaction_actors a inner join transactions t 
        on a.transaction_hash = t.hash
            and a.actor = NEW.address
            and a.actor_role = 'gateway'
            and a.block = NEW.first_block
    )
    where address = NEW.address;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER gateway_inventory_insert
AFTER INSERT ON gateway_inventory
FOR EACH ROW
EXECUTE PROCEDURE gateway_inventory_on_insert();

-- :down

drop trigger gateway_inventory_insert on gateway_inventory;
drop function gateway_inventory_on_insert;

alter table gateway_inventory
    drop column payer;
