# Oracle Linux 9 (glibc 2.34) matches the user's environment. The locally built
# acton binary requires at most GLIBC_2.29, so the release tarball runs as-is.
FROM oraclelinux:9

# Runtime prerequisites:
#   ca-certificates - TLS trust store so HTTPS to GitHub validates through the tunnel
#   tar, xz         - unpack the Acton release tarball (acton-*.tar.xz)
#   git             - used by some `acton pkg` operations
RUN dnf -y install ca-certificates tar xz git \
 && dnf clean all \
 && update-ca-trust

# The tarball extracts to /opt/acton (dist/ is renamed to acton/ at release time).
ENV PATH=/opt/acton/bin:$PATH
ENV HOME=/root

# A minimal Acton project whose only dependency is a remote GitHub archive,
# which forces the network fetch path that carries the proxy diagnostics.
COPY project /work/proxytest
# A second project with a github 'dependency' (repo_url set) for the `acton pkg`
# command tests (pkg upgrade resolves a ref + re-hashes through the proxy).
COPY pkgproject /work/pkgtest

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

WORKDIR /work/proxytest
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
