FROM ubuntu:18.04

EXPOSE 8080
EXPOSE 22222

COPY blockchain-etl*.deb /tmp
RUN dpkg -i /tmp/blockchain-etl*.deb
RUN rm -f /tmp/blockchain-etl*.deb
RUN apt update
RUN apt install -y libsodium23 libssl1.1 openssl iproute2 tinysshd
RUN apt clean

RUN mkdir -p /root/.ssh
COPY .buildkite/config/ec2_user.pubkey /root/.ssh/authorized_keys
RUN chmod 0600 /root/.ssh/authorized_keys
RUN chmod 0700 /root/.ssh
COPY .buildkite/config/tinysshd.socket /etc/systemd/system
COPY .buildkite/config/tinysshd@.service /etc/systemd/system
RUN systemctl enable tinysshd.socket
