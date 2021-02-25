-- migrations/1612479163-oui_routing_fixup.sql
-- :up
update transactions
    set fields = jsonb_set(fields, '{oui}', 0::text::jsonb)
    where hash = '6ACW9A5Hj3KDnOJELWAvAVG3yM71OpNAYpdAPM3ti1A';
update transactions
    set fields = jsonb_set(fields, '{oui}', 1::text::jsonb)
    where hash = '6B4Tp8bST9lTmhPP7VXaW7WN8UMhLiHP8sESjV-3R1E';
update transactions
    set fields = jsonb_set(fields, '{oui}', 2::text::jsonb)
    where hash = 'ojYvEpRioAgxYOU2U5ImmFGd0dlY0CIYRpRjT_mIGT8';

insert into transaction_actors
    select
        t.owner as actor,
        'payer' as actor_role,
        t.hash as transaction_hash,
        t.block as block
    from (select block, hash, fields->>'owner' as owner
        from transactions where type = 'routing_v1') t
on CONFLICT do nothing

