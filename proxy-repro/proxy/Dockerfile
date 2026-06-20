FROM ubuntu:24.04

RUN apt-get update \
 && apt-get install -y --no-install-recommends tinyproxy ca-certificates \
 && rm -rf /var/lib/apt/lists/*

COPY tinyproxy.conf /etc/tinyproxy/tinyproxy.conf
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

EXPOSE 8888
CMD ["/usr/local/bin/entrypoint.sh"]
