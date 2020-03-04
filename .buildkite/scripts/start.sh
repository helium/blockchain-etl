#!/bin/bash

set -euo pipefail

/var/helium/blockchain_etl/bin/blockchain_etl migrations setup
sleep 1
/var/helium/blockchain_etl/bin/blockchain_etl migrations -d /var/helium/blockchain_etl/migrations run
sleep 1
/var/helium/blockchain_etl/bin/blockchain_etl foreground
