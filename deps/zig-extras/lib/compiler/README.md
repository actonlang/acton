This directory overlays the build runner shipped with Zig 0.16.0, the
version pinned in the top-level Makefile. Keep it in sync when updating Zig.

The original build_runner.zig comes from lib/compiler/build_runner.zig in
the Zig 0.16.0 release archive. Its SHA-256 is
2791bc495d2d9f819a3cc4602578535a9ac1fd8246b77c1918dc5734c9afdf8b.
The adjacent LICENSE covers the upstream source.

Acton's additions implement `--watch --watch-stdin`: the caller requests each
build on standard input after publishing its sources. `Watch.zig` validates
cached steps and sends readiness and completion frames using the nonce in
`ACTON_ZIG_WATCH_TOKEN`. Ordinary builds and file-driven `--watch` retain the
upstream behavior; controlled mode must be requested explicitly.
