FROM debian:12
MAINTAINER Kristian Larsson <kristian@spritelink.net>

RUN cp -a /src/dist /usr/lib/acton \
 && cd /usr/bin \
 && ln -s ../lib/acton/bin/actonc \
 && ln -s ../lib/acton/bin/actondb \
 && ln -s ../lib/acton/bin/runacton

ENTRYPOINT ["/usr/bin/actonc"]
