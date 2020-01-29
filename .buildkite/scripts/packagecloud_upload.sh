#!/usr/bin/env bash

set -euo pipefail

curl -u "${PACKAGECLOUD_API_KEY}:" \
     -F "package[distro_version_id]=190" \ # ubuntu 18.04 LTS id
     -F "package[package_file]=@"$(ls *.deb) \
     https://packagecloud.io/api/v1/repos/helium/internal/packages.json
