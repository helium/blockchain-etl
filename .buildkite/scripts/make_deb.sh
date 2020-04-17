#!/usr/bin/env bash

set -euo pipefail

fpm -n $(basename $(pwd)) \
    -v $(git describe --long --always) \
    -s dir \
    -t deb \
    --depends libssl1.1 \
    --depends libsodium23 \
    --deb-systemd deb/blockchain-etl.service \
    --deb-no-default-config-files \
    _build/prod/rel/=/var/helium
