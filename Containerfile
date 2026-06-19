FROM debian:13
MAINTAINER Kristian Larsson <kristian@spritelink.net>

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

COPY dist/ /usr/lib/acton/

RUN cd /usr/bin \
 && ln -sf ../lib/acton/bin/acton \
 && ln -sf ../lib/acton/bin/actonc \
 && ln -sf ../lib/acton/bin/actondb \
 && ln -sf ../lib/acton/bin/runacton

ENTRYPOINT ["/usr/bin/acton"]
