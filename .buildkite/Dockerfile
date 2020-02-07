FROM ubuntu:18.04

COPY blockchain-etl*.deb /tmp
RUN dpkg -i /tmp/blockchain-etl*.deb
RUN rm -f /tmp/blockchain-etl*.deb
RUN apt update
RUN apt install -y libsodium23 libssl1.1 openssl iproute2 tinysshd
RUN apt clean

RUN adduser --gecos EC2 --disabled-password ec2_user
RUN mkdir -p /home/ec2_user/.ssh
COPY .buildkite/config/ec2_user.pubkey /home/ec2_user/.ssh/authorized_keys
RUN chown -R ec2_user:ec2_user /home/ec2_user
RUN chmod 0600 /home/ec2_user/.ssh/authorized_keys
RUN chmod 0700 /home/ec2_user/.ssh
COPY .buildkite/config/tinysshd.socket /etc/systemd/system
COPY .buildkite/config/tinysshd@.service /etc/systemd/system
RUN systemctl enable tinysshd.socket
