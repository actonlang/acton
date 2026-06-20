# Acton proxy reproduction & diagnosis

Reproduces the report: *"`HTTP_PROXY` doesn't reach the Acton compiler, so it
can't fetch packages from GitHub behind a proxy"* (Oracle Linux environment,
Acton installed from a tarball).

## Status: FIXED (and verified end-to-end here)

A fix is implemented in `compiler/lib/src/Acton/FetchDependencies.hs`. With it,
`acton fetch` on this proxy-only Oracle Linux 9 box routes through the proxy and
succeeds (the proxy logs `CONNECT github.com:443`). `run-demo.sh` demonstrates
the fixed behavior; the curl control behaves identically.

## TL;DR — root cause

The Acton release binary's **`getEnvironment` returns an empty environment**, so
`http-client`'s `proxyEnvironment` (used by `acton`'s dependency fetcher) never
saw `HTTP_PROXY`/`HTTPS_PROXY` and always connected **directly**. Behind a
proxy-only network that fails, and nothing was fetched.

This is **not** a proxy-selection bug (scheme/case), not the user's config, not
the network. It is a **link issue**: the release binary is linked with
**`zig cc`** (to target an older glibc for portability). Under that link GHC's
`getEnvironment` (the `environ`-array read) returns an empty list — an lld
copy-relocation handling difference for the `environ`/`__environ` glibc alias in
a `-no-pie` executable. `getenv()`/`lookupEnv` (libc `getenv`) still work, which
is why `$HOME` resolves; only the *full-environment* array read is empty.

Proven by bisection (minimal `getEnvironment` probe, same GHC snapshot):

| Build of the same 1-line program            | sees env vars? |
|----------------------------------------------|----------------|
| plain `stack ghc` (system gcc/ld linker)     | ✅ 43          |
| **`zig cc` as linker** (no ld-wrapper)       | ❌ **0**       |
| `zig cc` + `compiler/tools/ld-wrapper.sh` (= acton) | ❌ **0** |

The `ld-wrapper.sh` static toggling is *not* the cause — `zig cc` as the linker
breaks it on its own. Changing the glibc target version (2.29/2.34/2.39) does not
help; `-z nocopyreloc` and PIE do not link cleanly (which is why `-no-pie` is
currently forced). A fresh C program reading `environ` via `zig cc` is fine — it
is specifically GHC's precompiled-RTS `environ` reference retargeted by `zig cc`.

## What the demo shows

`./run-demo.sh` builds two containers and runs two cases on the **same isolated
network through the same proxy**:

1. **`acton fetch`** with `HTTPS_PROXY=http://proxy:8888` exported → the shell
   clearly has the var, but acton's own diagnostics print every proxy var as
   `unset` and `selected proxy: direct`; the fetch fails and the proxy logs
   nothing.
2. **`curl`** with the identical `HTTPS_PROXY` on the identical network → reaches
   GitHub (`HTTP 302`) and the proxy logs `CONNECT github.com:443`.

Same network, same proxy, same env var: curl works, acton doesn't → the var
never reaches acton's HTTP layer.

## Layout

```
proxy-repro/
├── docker-compose.yml      two containers + an isolated internal network
├── run-demo.sh             build + run the demonstration (./run-demo.sh down to stop)
├── acton/
│   ├── Dockerfile          oraclelinux:9 + ca-certificates/tar/xz/git
│   ├── entrypoint.sh       installs Acton from the mounted tarball into /opt/acton
│   └── project/            minimal Acton project with one remote GitHub dependency
├── proxy/
│   ├── Dockerfile          ubuntu:24.04 + tinyproxy
│   ├── tinyproxy.conf
│   └── entrypoint.sh       runs tinyproxy and tails its log to stdout
└── acton-dist.tar.xz       the release tarball (git-ignored; auto-built, see below)
```

### Network topology (why it forces the proxy)

- `acton` is attached **only** to the `internal` network (`internal: true`),
  so it has **no direct route to the internet**.
- `proxy` (tinyproxy) is attached to both `internal` and `egress`; `egress` has
  real internet via NAT.
- Therefore the *only* way out of the `acton` box is the proxy — exactly like a
  locked-down corporate/Oracle Linux host.

## How the Acton tarball is created (the thing that ships)

`make release` (target in the top-level `Makefile`) just renames `dist/` to
`acton/` and xz-compresses it:

```make
release: distribution
	tar cv --transform 's,^dist,acton,' --exclude .gitignore dist \
	    | xz -z -0 --threads=0 > acton-$(OS)-$(ARCH)-$(VERSION).tar.xz
```

So a tarball is just the built `dist/` tree (renamed `acton/…`): `acton/bin/acton`,
`acton/zig/…`, `acton/base`, `acton/std`, etc. Installing = extract it and put
`acton/bin` on `PATH`. `run-demo.sh` builds this same artifact from the local
`dist/` (the diagnostics build) and the `acton` container extracts it to
`/opt/acton` — i.e. installs Acton from a tarball, like the user's environment.

> The local binary needs at most `GLIBC_2.29`, so it runs on Oracle Linux 9
> (glibc 2.34). It would **not** run on Oracle Linux 8 (glibc 2.28).

## Requirements

- Docker with the `compose` plugin, and internet access to pull base images and
  (for the proxy) reach GitHub.
- A built compiler at `../dist/bin/acton` (`make dist/bin/acton`).

## Run

```bash
./run-demo.sh          # build (first time) + run both cases
./run-demo.sh down     # stop and remove everything
```

## The fix (implemented) and the deeper one (open)

1. **Implemented & verified — application level.** In
   `compiler/lib/src/Acton/FetchDependencies.hs` the fetcher no longer relies on
   `HTTP.proxyEnvironment Nothing` (which reads the empty `getEnvironment`). It
   now reads the proxy vars with `lookupEnv` (`readProxyEnv`) and applies the
   proxy explicitly via `HTTP.useProxy`/`HTTP.noProxy` (`proxyOverrideFor`),
   honoring scheme and `no_proxy`. The diagnostics read via `lookupEnv` too, so
   they report the truth. *Verified:* in an acton-linked probe, `getEnvironment`
   returns 0 vars but `lookupEnv "HTTPS_PROXY"` returns the value; and `acton
   fetch` here now traverses the proxy and succeeds.

2. **Open — the underlying `getEnvironment` breakage.** Any code path using
   `getEnvironment` is still affected (e.g. `runZig` in `compiler/acton/Main.hs`
   passes a near-empty env to the child `zig`; the build tolerates it today). The
   root fix is to make `getEnvironment` work under the `zig cc` link — most
   likely a PIE build (no copy relocations) or a newer `lld` that copies the
   `environ`/`__environ` alias together. `-no-pie` is currently forced and
   `-z nocopyreloc`/PIE did not link cleanly, so this needs build-system work.
   Use the probe below as a regression test.

3. **Pre-fix user workaround (no longer needed with the fix):** intercept egress
   at the network layer (transparent/redirecting proxy or NAT) rather than an
   env-configured `CONNECT` proxy, **or** pre-populate
   `~/.cache/acton/zig-global-cache` from a machine with internet.

### Regression probe

```haskell
import System.Environment (getEnvironment, lookupEnv)
main = do
  e <- getEnvironment
  putStrLn ("getEnvironment count: " ++ show (length e))
  lookupEnv "HTTPS_PROXY" >>= print
```

Built with the acton link recipe this prints `0` then `Just "…"`; built normally
it prints a non-zero count.
