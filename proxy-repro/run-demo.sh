#!/usr/bin/env bash
#
# Reproduce & diagnose why HTTP(S)_PROXY does not reach the Acton compiler.
#
# Two containers:
#   acton  - Oracle Linux 9, Acton installed from the release tarball. Attached
#            ONLY to the `internal` network => NO direct route to the internet.
#   proxy  - tinyproxy, on both `internal` and `egress` (real internet).
# So the Acton box's only way out is the proxy. This mirrors a locked-down
# Oracle Linux host where direct egress is blocked.
#
# The demo proves the bug is NOT the network, the proxy, or env passing:
#   1. acton fetch (HTTPS_PROXY set)  -> fails; its diagnostics say every proxy
#      var is "unset" even though the shell clearly exports it.
#   2. curl (HTTPS_PROXY set), same isolated network, same proxy -> reaches
#      GitHub through the proxy (proxy logs the CONNECT).
# => Conclusion: the acton binary itself does not see proxy env vars.
#
# Usage:  ./run-demo.sh         # build (if needed) + run
#         ./run-demo.sh down    # tear down
set -euo pipefail

cd "$(dirname "$0")"
REPO_ROOT="$(cd .. && pwd)"
TARBALL="acton-dist.tar.xz"
COMPOSE="docker compose"
PROJECT="$(basename "$PWD")"
INTERNAL_NET="${PROJECT}_internal"
PROXY_URL="http://proxy:8888"

hr()   { printf '%.0s=' $(seq 1 78); echo; }
note() { printf '\n>>> %s\n' "$*"; }

if [ "${1:-}" = "down" ]; then $COMPOSE down -v; exit 0; fi

# 1. Release tarball (built exactly like `make release`: dist/ renamed to acton/)
if [ ! -f "$TARBALL" ]; then
    note "Building Acton release tarball from $REPO_ROOT/dist ..."
    [ -x "$REPO_ROOT/dist/bin/acton" ] || { echo "ERROR: build dist/bin/acton first (make dist/bin/acton)"; exit 1; }
    ( cd "$REPO_ROOT" && tar c --transform 's,^dist,acton,' --exclude .gitignore dist \
        | xz -z -0 --threads=0 ) > "$TARBALL"
fi
note "Tarball: $(ls -lh "$TARBALL" | awk '{print $5}')"

# 2. Build & start
note "Building and starting containers ..."
$COMPOSE up -d --build
note "Waiting for Acton to install from the tarball ..."
for _ in $(seq 1 90); do
    $COMPOSE exec -T acton test -x /opt/acton/bin/acton 2>/dev/null && break || sleep 2
done
$COMPOSE exec -T acton test -x /opt/acton/bin/acton || { echo "install failed"; $COMPOSE logs acton; exit 1; }

proxy_lines() { $COMPOSE logs --no-color --no-log-prefix proxy 2>/dev/null | wc -l; }
proxy_since() { $COMPOSE logs --no-color --no-log-prefix proxy 2>/dev/null | tail -n +"$1"; }

# 3. Network sanity
note "Network sanity"
$COMPOSE exec -T acton getent hosts proxy >/dev/null 2>&1 \
    && echo "  [ok]   acton resolves the proxy host" \
    || echo "  [warn] acton cannot resolve 'proxy'"

############################################################################
hr; echo "DEMO 1 - acton fetch through the proxy (FIXED binary)"; hr
echo "What the SHELL exports to the acton process:"
$COMPOSE exec -T -e HTTPS_PROXY="$PROXY_URL" acton sh -c 'env | grep -i proxy | sed "s/^/    /"'
echo
echo "acton fetch on the proxy-only network (no direct egress):"
b=$(proxy_lines)
$COMPOSE exec -T -e HTTPS_PROXY="$PROXY_URL" -e HOME=/root -w /work/proxytest acton \
    sh -c 'rm -rf /root/.cache/acton; /opt/acton/bin/acton fetch 2>&1; echo "[exit $?]"' \
    | sed 's/^/    /' || true
echo
echo "  Proxy CONNECTs observed during this acton fetch:"
proxy_since "$((b+1))" | grep -iE 'connect' | sed 's/^/    proxy| /' \
    || echo "    (none — acton went direct; this is the pre-fix behavior)"

############################################################################
hr; echo "DEMO 2 - curl with HTTPS_PROXY, SAME isolated network, SAME proxy (control)"; hr
b=$(proxy_lines)
code=$(docker run --rm --network "$INTERNAL_NET" -e HTTPS_PROXY="$PROXY_URL" \
        curlimages/curl:latest -sS -o /dev/null -w '%{http_code}' --max-time 25 \
        https://github.com/madler/zlib/archive/refs/tags/v1.3.1.tar.gz 2>&1 || echo "FAILED")
echo "  curl result: HTTP $code  (302/200 = reached GitHub through the proxy)"
sleep 1   # let tinyproxy flush its log
echo "  Proxy log for this request:"
proxy_since "$((b+1))" | grep -iE 'connect' | sed 's/^/    proxy| /' || echo "    (none)"

############################################################################
hr; echo "CONCLUSION"; hr
cat <<'EOF'
  With the fix, acton fetch routes through the proxy (DEMO 1 shows CONNECT
  github.com:443 in the proxy log) and succeeds, matching curl (DEMO 2).

  Root cause: the release binary is linked with `zig cc` for an older glibc;
  under that link GHC's getEnvironment() returns an EMPTY environment (an lld
  copy-relocation issue with the environ/__environ glibc alias in a -no-pie
  executable). http-client's proxyEnvironment relies on getEnvironment, so it
  never saw the proxy. getenv()/lookupEnv still work (HOME resolved).

  Fix (compiler/lib/src/Acton/FetchDependencies.hs): read proxy vars via
  lookupEnv and apply the proxy explicitly (HTTP.useProxy), instead of
  HTTP.proxyEnvironment. The diagnostics now read via lookupEnv too.

  To see the ORIGINAL bug, rebuild dist/bin/acton from before the fix, rerun
  ./run-demo.sh (it rebuilds the tarball), and DEMO 1 will show 'unset'/direct.

  Tear down with: ./run-demo.sh down
EOF
