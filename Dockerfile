FROM erlang:22.3.2-alpine as builder

RUN apk add --no-cache --update \
    git tar build-base linux-headers autoconf automake libtool pkgconfig \
    dbus-dev bzip2 bison flex gmp-dev cmake lz4 libsodium-dev openssl-dev \
    sed wget cargo

# Install Rust toolchain
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

WORKDIR /usr/src/etl

ENV CC=gcc CXX=g++ CFLAGS="-U__sun__" \
    ERLANG_ROCKSDB_OPTS="-DWITH_BUNDLE_SNAPPY=ON -DWITH_BUNDLE_LZ4=ON" \
    ERL_COMPILER_OPTIONS="[deterministic]" \
    PATH="/root/.cargo/bin:$PATH" \
    RUSTFLAGS="-C target-feature=-crt-static"

# Add our code
ADD . /usr/src/etl/

RUN ./rebar3 as docker_etl tar
RUN mkdir -p /opt/docker
RUN tar -zxvf _build/docker_etl/rel/*/*.tar.gz -C /opt/docker
RUN mkdir -p /opt/docker/update

FROM erlang:22.3.2-alpine as runner

RUN apk add --no-cache --update ncurses dbus gmp libsodium gcc
RUN ulimit -n 64000

WORKDIR /opt/etl

ENV COOKIE=etl \
    # Write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # add miner to path, for easy interactions
    PATH=$PATH:/opt/etl/bin

COPY --from=builder /opt/docker /opt/etl

ENTRYPOINT ["/opt/etl/bin/blockchain_etl"]
CMD ["foreground"]
