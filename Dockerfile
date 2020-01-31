FROM ubuntu:18.04

EXPOSE 8080
EXPOSE 44158

COPY blockchain-etl*.deb
RUN dpkg -i blockchain-etl*.deb
RUN rm -f blockchain-etl*.deb

CMD /var/helium/bin/blockchain-etl foreground

