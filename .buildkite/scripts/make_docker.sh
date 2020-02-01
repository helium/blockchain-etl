#!/usr/bin/env bash

set -euo pipefail

ECS_REGISTRY_NAME="217417705465.dkr.ecr.us-west-2.amazonaws.com/dev-deploy"
DEB_PKG="$(basename $(pwd))_$(git describe --long --always)_amd64.deb"
DOCKER_NAME="$(basename $(pwd))_${BUILDKITE_TAG}"

buildkite-agent artifact download $DEB_PKG .

$(aws ecr get-login --no-include-email --region us-west-2)

docker build -t dev-deploy:$DOCKER_NAME .
docker tag dev-deploy:$DOCKER_NAME "$ECS_REGISTRY_NAME:$DOCKER_NAME"
docker push "$ECS_REGISTRY_NAME:$DOCKER_NAME"
