FROM ubuntu:18.04

EXPOSE 8080
EXPOSE 22222

RUN apt update
RUN apt install -y libsodium23 libssl1.1 openssl iproute2 tinysshd
RUN apt clean

RUN mkdir -p /root/.ssh
COPY .buildkite/config/ubuntu.pubkey /root/.ssh/authorized_keys
RUN chmod 0600 /root/.ssh/authorized_keys
RUN chmod 0700 /root/.ssh
COPY .buildkite/config/tinysshd.socket /etc/systemd/system
COPY .buildkite/config/tinysshd@.service /etc/systemd/system
RUN systemctl enable tinysshd.socket
COPY .buildkite/config/blockchain_limits.conf /etc/security/limits.d

COPY blockchain-etl*.deb /
RUN dpkg -i blockchain-etl*.deb
RUN rm -f blockchain-etl*.deb
RUN touch /var/helium/blockchain_etl/.env

COPY .buildkite/scripts/lager_console_setup.pl /
RUN perl -i lager_console_setup.pl /var/helium/blockchain_etl/releases/0.1.0/sys.config
RUN rm -f lager_console_setup.pl
