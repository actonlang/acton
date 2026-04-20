# Package Management

`acton` offers integrated package management to declare dependencies on
other Acton packages and automatically download them from their sources
on the Internet.

This sits next to the project tree, not instead of it. See
[Projects](projects.md) for how local source discovery works and
[Modules](modules.md) for how files under `src/` become module names.

The guiding principle behind Acton's package management is to strive
for determinism, robustness, and safety. This is primarily achieved by
only resolving dependencies at design time. That is, the developer of a
particular Acton package determines its exact dependencies, not
whomever might be downloading and building it. The identity of a
package at a particular point in time, which you can think of as the
version of a package, is the hash of its content. That is the
foundation of Acton's package management. Each dependency has a hash of
its content. A URL is just one place from which this particular version
of a package can be downloaded. The hash of packages is determined and
recorded at design time. Anyone pulling down and building dependencies
will have the hash verified to ensure a deterministic build.

There is no central package repository. Instead, dependencies are
defined as URLs from which the dependency package can be downloaded.
This is typically a tar.gz file from GitHub, GitLab, or a similar
source hosting site. Again, the identity of a version of a package is
the content hash. The URL is only where to get it.

Acton is statically compiled. All dependencies are fetched and
included at compile time, so there are no runtime dependencies.

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
