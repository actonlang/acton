# Build and run

This repo builds the compiler (Haskell), runtime, and bundled deps into `dist/`.

## Prereqs

- `stack` for Haskell builds (compiler + LSP)
- `make`, `curl`, `tar`, `xz` (used by the Makefile)
- Zig is downloaded automatically into `dist/zig` (version pinned in `Makefile`)

## Common targets

```sh
make                    # build full distribution (compiler, runtime, deps)
make dist/bin/acton     # compiler + lsp-server-acton only (fast for compiler work)
make dist/bin/actonc    # compatibility symlink to acton
```

## Running local binaries

```sh
dist/bin/acton path/to/file.act
dist/bin/acton build
dist/bin/acton test
dist/bin/actonc path/to/file.act   # compatibility alias
```

- Prefer `dist/bin/*` to avoid accidentally using a globally installed Acton.
- `make clean` removes build outputs; `make clean-all` also removes the Zig cache.

## Caching and build knobs

- `ZIG_LOCAL_CACHE_DIR` controls Zig's cache location (defaults to `~/.cache/acton/zig-local-cache` when `HOME` is set).
- Stack uses `compiler/tools/zig-cc.sh` / `compiler/tools/zig-cxx.sh` so GHC/Cabal go through `dist/zig/zig`.
- On Linux, set `ACTON_ZIG_GLIBC_VERSION=<major.minor>` to force Zig CC/C++ wrappers to pass `-target <host-arch>-linux-gnu.<major.minor>` (for example `2.35`).
- `BUILD_RELEASE=1 make` stamps release versions; default builds get a timestamped version suffix.
- On Linux, the Makefile pins the default target to glibc 2.27 for compatibility.
