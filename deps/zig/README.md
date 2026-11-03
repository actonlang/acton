# Zig toolchain

Acton uses the self-contained releases built by
[actonlang/zig](https://github.com/actonlang/zig). Each archive contains a
compiler and its matching library sources. The fork includes the compiler,
cache, linker and build-runner fixes; Acton's build does not rebuild Zig or
apply patches to it.

To upgrade, update `ZIG_VERSION` in the top-level Makefile and replace
`SHA256SUMS` with the checksum manifest from that same release. Keep all four
host archives in the manifest. Published versions are immutable.

The Makefile checks the archive against the checked-in hash, replaces
`dist/zig` with its contents, and adds platform headers from `deps/zig-extras`.

`ZIG_DOWNLOAD_BASE_URL` or `ZIG_DOWNLOAD_URL` may point to a mirror containing
the same archive bytes. Mirrors do not override checksum verification.
