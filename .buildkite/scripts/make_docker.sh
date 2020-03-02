#!/usr/bin/env bash

set -euo pipefail

DEV_ECS_REGISTRY_NAME="217417705465.dkr.ecr.us-west-2.amazonaws.com/dev-deploy"
PROD_ECS_REGISTRY_NAME="350169207474.dkr.ecr.us-west-2.amazonaws.com/prod-deploy"
DEB_PKG="$(basename $(pwd))_$(git describe --long --always)_amd64.deb"
DOCKER_NAME="$(basename $(pwd))_${BUILDKITE_TAG}"

buildkite-agent artifact download $DEB_PKG .

docker build -t helium:$DOCKER_NAME -f .buildkite/Dockerfile .
docker tag helium:$DOCKER_NAME "$DEV_ECS_REGISTRY_NAME:$DOCKER_NAME"
docker tag helium:$DOCKER_NAME "$PROD_ECS_REGISTRY_NAME:$DOCKER_NAME"

aws ecr get-login-password | docker login --username AWS --password-stdin 217417705465.dkr.ecr.us-west-2.amazonaws.com
docker push "$DEV_ECS_REGISTRY_NAME:$DOCKER_NAME"

aws ecr get-login-password | docker login --username AWS --password-stdin 350169207474.dkr.ecr.us-west-2.amazonaws.com
docker push "$PROD_ECS_REGISTRY_NAME:$DOCKER_NAME"
