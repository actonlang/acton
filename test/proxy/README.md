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
   *transitive* zig package dependency, which zig resolves during final
   compilation).

Because the acton box has no direct egress, a scenario can only succeed if acton
routed that request through the proxy:

| result | meaning |
|--------|---------|
| **exit 0** | acton honoured `http(s)_proxy` — **PASS** |
| **exit ≠ 0** | acton ignored the proxy and went direct — **FAIL** |

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
`zlib_upstream` from github). Acton fetches the Acton package itself through the
proxy-aware http-client, but that nested `.url` is resolved by **zig** during
final compilation — and zig's HTTPS-over-CONNECT-proxy support is broken (it
writes an absolute-form request URI into the tunnel), so the fetch fails with
`invalid HTTP response: HttpConnectionClosing`. This is the lmdb/libssh failure
seen in the field, and it affects **all architectures**.

> **Status:** scenario 4 currently **fails on purpose** — it reproduces the
> still-unfixed transitive-dependency bug. The fix (Acton pre-seeding zig's cache
> for transitive `.url` deps through the proxy-aware http-client) is a follow-up;
> once it lands, this scenario passes too.

The fixtures: `project/` (a `zig_dependency`) and `pkgproject/` (a github
`dependency`) use zlib v1.3.1, downloaded + hashed through Acton's HTTP path
without needing to be a full Acton package. `zigdepproject/` depends on the real
`actonlang/acton-zlib` package and **builds** it, forcing zig to resolve the
transitive `zlib_upstream` `.url` through the proxy.
