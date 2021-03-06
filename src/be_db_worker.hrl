-define(DB_POOL, db_pool).

-define(SQUERY(S), be_db_worker:squery((S))).
-define(EQUERY(S, P), be_db_worker:equery((S), (P))).
-define(BATCH_QUERY(B), be_db_worker:batch_query((B))).
-define(BATCH_QUERY(C, B), be_db_worker:batch_query((C), (B))).
-define(PREPARED_QUERY(S, P), be_db_worker:prepared_query((S), (P))).
-define(PREPARED_QUERY(C, S, P), be_db_worker:prepared_query((C), (S), (P))).
-define(WITH_TRANSACTION(F), be_db_worker:with_transaction((F))).
-define(WITH_CONNECTION(F), be_db_worker:with_connection((F))).
