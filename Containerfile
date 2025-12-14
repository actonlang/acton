FROM debian:13
MAINTAINER Kristian Larsson <kristian@spritelink.net>

COPY dist/ /usr/lib/acton/

RUN cd /usr/bin \
 && ln -sf ../lib/acton/bin/acton \
 && ln -sf ../lib/acton/bin/actonc \
 && ln -sf ../lib/acton/bin/actondb \
 && ln -sf ../lib/acton/bin/runacton

ENTRYPOINT ["/usr/bin/acton"]
