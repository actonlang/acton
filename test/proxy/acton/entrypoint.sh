#!/bin/sh
# Install Acton "from a tarball" (the same artifact `make release` produces) and
# then idle so the demo can exec scenarios into this container.
set -e

TARBALL="${ACTON_TARBALL:-/acton-dist.tar.xz}"
# Written only after extraction fully completes. The harness waits for THIS, not
# for bin/acton, so it never exec's a still-being-written binary (e.g. zig/zig)
# mid-untar -> avoids ETXTBSY "Text file busy" on the first scenario.
MARKER=/opt/acton/.install-complete

if [ ! -f "$MARKER" ]; then
    if [ ! -f "$TARBALL" ]; then
        echo "[entrypoint] ERROR: tarball $TARBALL not mounted." >&2
        echo "[entrypoint] Build it first: run 'make test-proxy' (see test/proxy/run.sh)" >&2
        exit 1
    fi
    echo "[entrypoint] Installing Acton from tarball $TARBALL -> /opt/acton ..."
    mkdir -p /opt
    tar -C /opt -xf "$TARBALL"
    touch "$MARKER"
    echo "[entrypoint] Installed. acton binary: $(ls -l /opt/acton/bin/acton)"
fi

exec "$@"
