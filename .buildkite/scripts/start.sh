#!/bin/bash

set -euo pipefail

/var/helium/blockchain_etl/bin/blockchain_etl escript \
    bin/psql_migration setup
sleep 1
/var/helium/blockchain_etl/bin/blockchain_etl escript \
    bin/psql_migration -d /var/helium/blockchain_etl/migrations run
sleep 1
/var/helium/blockchain_etl/bin/blockchain_etl foreground
