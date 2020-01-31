FROM ubuntu:18.04

EXPOSE 8080
EXPOSE 44158

COPY blockchain-etl*.deb /tmp
RUN dpkg -i /tmp/blockchain-etl*.deb
RUN rm -f /tmp/blockchain-etl*.deb

CMD /var/helium/blockchain_etl/bin/blockchain_etl foreground
