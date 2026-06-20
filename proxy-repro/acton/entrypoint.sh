#!/bin/sh
# Install Acton "from a tarball" (the same artifact `make release` produces) and
# then idle so the demo can exec scenarios into this container.
set -e

TARBALL="${ACTON_TARBALL:-/acton-dist.tar.xz}"

if [ ! -x /opt/acton/bin/acton ]; then
    if [ ! -f "$TARBALL" ]; then
        echo "[entrypoint] ERROR: tarball $TARBALL not mounted." >&2
        echo "[entrypoint] Build it first: see proxy-repro/run-demo.sh" >&2
        exit 1
    fi
    echo "[entrypoint] Installing Acton from tarball $TARBALL -> /opt/acton ..."
    mkdir -p /opt
    tar -C /opt -xf "$TARBALL"
    echo "[entrypoint] Installed. acton binary: $(ls -l /opt/acton/bin/acton)"
fi

exec "$@"
