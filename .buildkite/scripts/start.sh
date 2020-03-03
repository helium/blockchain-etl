#!/bin/bash

set -euo pipefail

# Start TinySSH listener on port 22222
/usr/bin/tcpserver -HRDl0 0.0.0.0 22222 /usr/sbin/tinysshd -v /etc/tinyssh/sshkeydir &

/var/helium/blockchain_etl/bin/blockchain_etl escript \
    bin/psql_migration setup
sleep 1
/var/helium/blockchain_etl/bin/blockchain_etl escript \
    bin/psql_migration -d /var/helium/blockchain_etl/migrations run
sleep 1
/var/helium/blockchain_etl/bin/blockchain_etl foreground
