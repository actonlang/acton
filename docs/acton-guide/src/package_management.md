# Package Management

`acton` offers integrated package management for adding library dependencies to
a project. At build time, dependencies are automatically fetched and validated.

Acton pins dependencies by content, via a content hash, rather than by mutable
package version labels. Therefore, builds are entirely deterministic so that the
same compilation result can be reproduced on another machine.

The guiding principle behind Acton's package management is to strive for
determinism, robustness, and safety. Dependencies are resolved at design time by
the package developer and written into `Build.act` as archive URLs plus content
hashes. Later builds fetch those recorded archives and verify the hashes; they
do not pick newer compatible versions or rely on a name to mean the same thing
forever.

Acton's public package index is a discovery index. Packages are hosted
by their owners, and dependencies are recorded as URLs from which a
specific package archive can be downloaded. This is typically a tar.gz
or zip archive from GitHub, GitLab, or a similar source hosting site.
The index helps find packages; `Build.act` records the exact archive and
hash used by a project.

The public index is decentralized in the sense that package authors opt
in from their own GitHub repositories. The index collects repositories
tagged with an `acton-library` or `acton-app` topic and records their
metadata for search and resolution. `acton pkg` commands work with
library packages only: `acton pkg search` shows libraries and
`acton pkg add` resolves libraries as dependencies. App packages are
installable applications and are kept separate from dependency
resolution.

All dependencies are fetched and included, linked statically, at compile time,
so there are no runtime dependencies.

## Project lineage fingerprint

Each project must declare a **fingerprint** in `Build.act` to represent its lineage — the stable identity of the project across versions. This is separate from dependency content hashes:

- Content hashes identify a specific version of a dependency.
- Fingerprints identify the project itself and help Acton deduplicate dependencies and generate consistent build metadata.

Example:
```python
name = "myproject"
fingerprint = 0x1234abcd5678ef00
```

**How it behaves today**
- `name` and `fingerprint` are required in every project.
- Acton validates that the fingerprint matches the name’s lineage prefix.
- If they don’t match or either is missing, the build fails with guidance on how to fix it.

Renaming a project breaks lineage, so generate a new fingerprint for the new name. When you fork a project, also generate a new fingerprint so the fork has its own lineage.
