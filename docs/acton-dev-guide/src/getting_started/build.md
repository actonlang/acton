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
- `BUILD_RELEASE=1 make` stamps release versions; default builds get a timestamped version suffix.
- On Linux, the Makefile pins the default target to glibc 2.27 for compatibility.
