FROM ubuntu:18.04

EXPOSE 44158

COPY blockchain-etl*.deb /
RUN apt update
RUN apt install -y iproute2
RUN apt install -y ./blockchain-etl*.deb
RUN rm -rf /var/apt/cache
RUN rm -f blockchain-etl*.deb

# Delete the default .env in case there is one present
# We expect the appropriate environment will be set at
# container start time.
RUN rm -f /var/helium/blockchain_etl/.env

COPY .buildkite/config/blockchain_limits.conf /etc/security/limits.d

COPY .buildkite/scripts/start.sh /
