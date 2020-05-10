#!/bin/bash

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source "${scriptDir}/load-db-env.sh"

: ${DATABASE_URL:?}

exec "${scriptDir}/blockchain_etl" remote_console
