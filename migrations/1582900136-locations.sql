-- migrations/1582900136-locations.sql
-- :up

CREATE TABLE locations (
       location TEXT NOT NULL,

       long_street TEXT,
       short_street TEXT,

       long_city TEXT,
       short_city TEXT,

       long_state TEXT,
       short_state TEXT,

       long_country TEXT,
       short_country TEXT,

       PRIMARY KEY (location)
);

-- :down

DROP TABLE locations;
