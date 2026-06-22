# Proxy dependency-fetch test

Reproduces (and guards against) the report: *Acton can't fetch dependencies from
behind an HTTP proxy* — even with `http_proxy`/`https_proxy` exported.

## Run

```bash
make test-proxy
```

or directly:

```bash
ACTON_DIST=/path/to/dist bash test/proxy/run.sh        # test a specific dist
DOCKER_DEFAULT_PLATFORM=linux/amd64 bash test/proxy/run.sh   # cross-arch (emulated)
```

Requires Docker with the `compose` plugin, plus internet (to pull base images and
let the proxy reach GitHub). `make test-proxy` defaults to the repo's `dist/`.

## What it does

Two containers (`docker-compose.yml`):

- **acton** — Oracle Linux 9, Acton installed from the release tarball, attached
  **only** to an `internal: true` docker network, so it has **no direct route to
  the internet**. Its only way out is the proxy.
- **proxy** — [tinyproxy](https://tinyproxy.github.io/), on both the internal
  network and an `egress` network with real internet. It logs every `CONNECT`.

`run.sh` then:

1. packs `dist/` into the release tarball the acton container installs from;
2. starts the containers;
3. **control** — runs `curl` on the same internal-only network through the same
   proxy; it must reach GitHub (so a later failure is acton's fault, not the
   network/proxy);
4. runs three scenarios with the proxy variables set, covering every HTTP path:
   `acton fetch` (dependency archive download), `acton pkg update` (the package
   index over http-client), and `acton pkg upgrade` (GitHub API ref resolution
   plus archive re-hash).

Because the acton box has no direct egress, a scenario can only succeed if acton
routed that request through the proxy:

| result | meaning |
|--------|---------|
| **exit 0** | acton honoured `http(s)_proxy` — **PASS** |
| **exit ≠ 0** | acton ignored the proxy and went direct — **FAIL** |

## The bug this guards against

On Linux **x86_64** binaries linked `-no-pie` by `zig cc` (to target an older
glibc), GHC's `getEnvironment` returns an empty list: an lld copy-relocation
issue with the glibc `environ`/`__environ` alias. `http-client`'s
`proxyEnvironment` reads `getEnvironment`, so it never sees the proxy variables
and connects directly — which fails on a proxy-only network. (`getenv`/`lookupEnv`
still work, so `curl` and `$HOME` are fine — which is exactly why this is so
confusing in the field.) **aarch64 is unaffected**, so this test passes there
regardless; on x86_64 it fails before the fix and passes after.

The fixture uses a `zig_dependency` (zlib v1.3.1) because `acton fetch` downloads
and hashes it through the same HTTP path as a package dependency, without needing
it to be a full Acton package.
