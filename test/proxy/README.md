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
4. runs four scenarios with the proxy variables set, covering every HTTP path:
   `acton fetch` (dependency archive download), `acton pkg update` (the package
   index over http-client), `acton pkg upgrade` (GitHub API ref resolution plus
   archive re-hash), and `acton build` (a dependency that itself carries a
   *transitive* zig package dependency, which Acton pre-fetches before Zig uses
   it during final compilation).

Each scenario starts with an empty Acton cache. Because the acton box has no
direct egress, a scenario can only succeed if its downloads went through the
proxy. A failure can also come from dependency hashes or compilation, so inspect
the command output before treating it as a routing failure:

| result | meaning |
|--------|---------|
| **exit 0 + expected success marker** | scenario completed through the proxy — **PASS** |
| **exit ≠ 0 or missing success marker** | scenario failed; inspect download, hash and compiler diagnostics — **FAIL** |

## The bugs this guards against

**1. The `environ` bug (scenarios 1–3).** On Linux **x86_64** binaries linked
`-no-pie` by `zig cc` (to target an older glibc), GHC's `getEnvironment` returns
an empty list: an lld copy-relocation issue with the glibc `environ`/`__environ`
alias. `http-client`'s `proxyEnvironment` reads `getEnvironment`, so it never
sees the proxy variables and connects directly — which fails on a proxy-only
network. (`getenv`/`lookupEnv` still work, so `curl` and `$HOME` are fine — which
is exactly why this is so confusing in the field.) **aarch64 is unaffected**, so
these scenarios pass there regardless; on x86_64 they fail before the fix and
pass after.

**2. The transitive zig-dependency bug (scenario 4).** A real Acton package can
carry zig package dependencies, and those can have their own *transitive* `.url`
dependencies (e.g. `actonlang/acton-zlib`'s `deps/zlib/build.zig.zon` pulls
`zlib_upstream` from github). Previously, Acton fetched the Acton package through
the proxy-aware http-client but left that nested `.url` to **zig** during final
compilation. Zig wrote an absolute-form request URI into the HTTPS tunnel, so
the origin rejected it with `invalid HTTP response: HttpConnectionClosing`.
This was the lmdb/libssh failure seen in the field on **all architectures**.

Acton now pre-fetches non-lazy transitive `.url` dependencies through its
proxy-aware HTTP client and seeds Zig's package cache. Scenario 4 requires the
final build to succeed too, proving Zig can consume those cached archives and
compile the dependency.

The fixtures: `project/` (a `zig_dependency`) and `pkgproject/` (a github
`dependency`) use zlib v1.3.1, downloaded + hashed through Acton's HTTP path
without needing to be a full Acton package. `zigdepproject/` depends on the real
`actonlang/acton-zlib` package and **builds** it, forcing zig to resolve the
transitive `zlib_upstream` dependency from the cache Acton seeds through the proxy.
