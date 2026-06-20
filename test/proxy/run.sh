#!/usr/bin/env bash
#
# Deterministic proxy test.
#
# Brings up two containers via docker compose:
#   acton  - Oracle Linux 9, Acton installed from the release tarball, attached
#            ONLY to an internal docker network => NO direct internet egress.
#   proxy  - tinyproxy, bridging the internal network to real egress.
# The ONLY way out of the acton box is the proxy (a locked-down corporate host).
#
# It then runs `acton fetch` (one remote https zig dependency) with the proxy
# env vars set. Because the acton box has no direct egress, a SUCCESSFUL fetch is
# possible only if acton's HTTP client actually routed through the proxy. So:
#
#   exit 0  => acton honoured http(s)_proxy  (PASS)
#   exit !0 => acton ignored the proxy and went direct  (FAIL / reproduces the
#              x86_64 -no-pie `environ` bug: getEnvironment() returns [] so
#              http-client's proxyEnvironment never sees the proxy vars)
#
# Usage:
#   make test-proxy
#   ACTON_DIST=/path/to/dist bash test/proxy/run.sh         # test a specific dist
#   DOCKER_DEFAULT_PLATFORM=linux/amd64 bash test/proxy/run.sh   # cross-arch (emulated)
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
ACTON_DIST="${ACTON_DIST:-$ROOT/dist}"
PROJECT="acton-proxy-test"
PROXY_URL="http://proxy:8888"
INTERNAL_NET="${PROJECT}_internal"
ZLIB_URL="https://github.com/madler/zlib/archive/refs/tags/v1.3.1.tar.gz"
COMPOSE=(docker compose -p "$PROJECT" -f "$HERE/docker-compose.yml")

note() { printf '\n>>> %s\n' "$*"; }

cleanup() { "${COMPOSE[@]}" down -v --remove-orphans >/dev/null 2>&1 || true; rm -f "$HERE/acton-dist.tar.xz"; }
trap cleanup EXIT INT TERM

command -v docker >/dev/null 2>&1 || { echo "ERROR: docker not found" >&2; exit 2; }
docker compose version >/dev/null 2>&1 || { echo "ERROR: docker compose plugin not found" >&2; exit 2; }

# 1. Pack the dist into the release tarball the acton container installs from.
#    Use a symlink + `tar -h` so the tree is archived under acton/ regardless of
#    the source dir's name, portably across GNU tar (Linux) and bsdtar (macOS).
[ -x "$ACTON_DIST/bin/acton" ] || { echo "ERROR: no acton at $ACTON_DIST/bin/acton (run 'make dist/bin/acton', or set ACTON_DIST)" >&2; exit 2; }
note "Packing $ACTON_DIST -> acton-dist.tar.xz"
stage="$(mktemp -d)"
ln -s "$(cd "$ACTON_DIST" && pwd)" "$stage/acton"
tar -C "$stage" -ch acton | xz -z -0 --threads=0 > "$HERE/acton-dist.tar.xz"
rm -rf "$stage"
ls -lh "$HERE/acton-dist.tar.xz" | awk '{print "    tarball: " $5}'

# 2. Build & start the two containers.
note "docker compose up --build"
"${COMPOSE[@]}" up -d --build

note "Waiting for acton to install from the tarball"
installed=
for _ in $(seq 1 150); do
    if "${COMPOSE[@]}" exec -T acton test -x /opt/acton/bin/acton >/dev/null 2>&1; then installed=1; break; fi
    sleep 2
done
[ -n "$installed" ] || { echo "ERROR: acton did not install" >&2; "${COMPOSE[@]}" logs acton >&2 || true; exit 2; }

# 3. Control: prove the proxy path itself works (so a later failure is acton's,
#    not the test infrastructure). curl on the SAME internal-only network, using
#    the SAME proxy, must reach GitHub.
note "Control: curl reaches GitHub through the proxy (same isolated network)"
if ! docker run --rm --network "$INTERNAL_NET" \
        -e http_proxy="$PROXY_URL" -e https_proxy="$PROXY_URL" \
        curlimages/curl:latest -fsS -o /dev/null --max-time 30 "$ZLIB_URL"; then
    echo "ERROR: proxy control failed -- the proxy/egress is broken, not acton" >&2
    "${COMPOSE[@]}" logs proxy >&2 || true
    exit 2
fi
echo "    [ok] proxy reaches GitHub"

# 4. The actual test: acton fetch on the proxy-only network.
note "acton fetch through the proxy (http_proxy + https_proxy set)"
set +e
out="$("${COMPOSE[@]}" exec -T \
        -e http_proxy="$PROXY_URL"  -e https_proxy="$PROXY_URL" \
        -e HTTP_PROXY="$PROXY_URL"  -e HTTPS_PROXY="$PROXY_URL" \
        -e HOME=/root \
        acton sh -c 'rm -rf /root/.cache/acton; cd /work/proxytest && exec /opt/acton/bin/acton fetch' 2>&1)"
code=$?
set -e
echo "$out" | sed 's/^/    acton| /'

connects="$("${COMPOSE[@]}" logs --no-color proxy 2>/dev/null | grep -c -i 'CONNECT' || true)"

# 5. Verdict.
echo
if [ "$code" -eq 0 ]; then
    note "PASS: acton fetched the dependency through the proxy (proxy CONNECTs: $connects)"
    exit 0
fi

note "FAIL: acton fetch failed (exit $code) on a proxy-only network"
echo "    proxy CONNECT log:" >&2
"${COMPOSE[@]}" logs --no-color proxy 2>/dev/null | grep -i 'CONNECT' | sed 's/^/      /' >&2 || true
cat >&2 <<'EOF'
    With no direct egress, this means acton's HTTP client did not route through
    the proxy. The usual cause is GHC's getEnvironment() returning an empty
    environment on x86_64 -no-pie binaries (the `environ` copy-relocation bug),
    so http-client's proxyEnvironment never saw http(s)_proxy.
EOF
exit 1
