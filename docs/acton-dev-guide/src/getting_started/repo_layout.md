# Repository layout

Top-level directories are grouped by subsystem and build outputs.

| Path | Purpose |
| --- | --- |
| `compiler/` | Acton compiler & LSP (`acton`, `lsp-server-acton`). Written in Haskell. |
| `base/` | Builtins, RTS, and standard library sources. |
| `backend/` | Distributed RTS database backend. |
| `builder/` | Zig build system helper definitions. |
| `bin/` | Small helper binaries/scripts (e.g. `runacton`). |
| `completion/` | Shell completion assets. |
| `deps/` | Vendored third-party sources used by the build. |
| `deps-download/` | Cached dependency tarballs (generated). |
| `dist/` | Build output and bundled distribution (generated). |
| `docs/` | Documentation, like this dev guide. |
| `examples/` | Example programs and snippets. |
| `test/` | Integration tests, runtime/db harness, stdlib tests. |
| `utils/` | Misc tooling, scripts, and dev helpers. |
| `ecolift/` | Lifting the Acton ecosystem for new language features. |
| `debian/` | Debian packaging. |
| `homebrew/` | Homebrew formulae. |
| `slides/` | Presentation material. |
| `workspace/` | Scratch notes and experiments (not part of build). |
