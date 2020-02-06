FROM ubuntu:18.04

COPY blockchain-etl*.deb /tmp
RUN dpkg -i /tmp/blockchain-etl*.deb
RUN rm -f /tmp/blockchain-etl*.deb
RUN apt update
RUN apt install -y libsodium23 libssl1.1 openssl
RUN apt clean
