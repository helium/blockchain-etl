-- migrations/1599674789-initial_ouis.sql
-- :up

update transactions
    set fields = jsonb_set(fields, '{oui}', 1::text::jsonb)
    where hash = '6ACW9A5Hj3KDnOJELWAvAVG3yM71OpNAYpdAPM3ti1A';
update transactions
    set fields = jsonb_set(fields, '{oui}', 2::text::jsonb)
    where hash = '6B4Tp8bST9lTmhPP7VXaW7WN8UMhLiHP8sESjV-3R1E';
update transactions
    set fields = jsonb_set(fields, '{oui}', 3::text::jsonb)
    where hash = 'ojYvEpRioAgxYOU2U5ImmFGd0dlY0CIYRpRjT_mIGT8';

