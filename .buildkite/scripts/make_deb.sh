#!/usr/bin/env bash

set -euo pipefail

fpm -n $(basename $(pwd)) \
    -v $(git describe --long --always) \
    -s dir \
    -t deb \
    _build/prod/rel/=/var/helium
