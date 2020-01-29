-define(DB_POOL, db_pool).

-define(SQUERY(S), be_db_worker:squery((S))).
-define(EQUERY(S, P), be_db_worker:equery((S), (P))).
-define(PREPARED_QUERY(S, P), be_db_worker:prepared_query((S), (P))).
-define(WITH_TRANSACTION(F), be_db_worker:with_transaction((F))).
-define(WITH_CONNECTION(F), be_db_worker:with_connection((F))).
