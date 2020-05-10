# Build stage: build-base
FROM erlang:22-slim as build-base

# Install build dependencies
RUN set -xe \
  && apt-get update \
  && apt-get install -y \
    wget \
    git \
    build-essential \
    cmake \
    libssl-dev \
    libsodium-dev \
    libsnappy-dev \
    liblz4-dev \
  && echo "Done"

# Build stage: build-dummy
FROM build-base as build-dummy

# Copy our dependency config only
COPY ./rebar* /etl/

# Set workdir
WORKDIR /etl

# Compile dependencies to make things more repeatable
RUN ./rebar3 as prod do compile

# Build stage: build-main
FROM build-dummy as build-main

# Copy project files
COPY . /etl

# Set workdir
WORKDIR /etl

# Build prod release
RUN ./rebar3 as prod do release

# Build stage: runtime
FROM debian:buster-slim as runtime

# Install the runtime dependencies
RUN set -xe \
  && apt-get update \
  && apt-get install -y \
    openssl \
    libsodium23 \
    libsnappy1v5 \
    lz4 \
    iproute2 \
    postgresql-client-11 \
  && echo "Done"

# Install the released application
COPY --from=build-main /etl/_build/prod/rel/blockchain_etl /etl/

# Set workdir
WORKDIR /etl

# Command
CMD /etl/bin/start-foreground.sh
