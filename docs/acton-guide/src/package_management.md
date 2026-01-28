# Package Management

`acton` offers integrated package management to declare dependencies on other Acton packages and automatically download them from their sources on the Internet.

The guiding principle behind Actons package management is to strive for determinism, robustness and safety. This is primarily achieved by only resolving dependencies at design time. That is, it is the developer of a particular Acton package that determines its exact dependencies and not whomever might be downloading and building said package. The very identity of a package at a particular point in time, which can be thought of as the version of a package, is the hash of its content. This is the foundation of Actons package management. Each dependency has a hash of its content. A URL is just one place from which this particular version of a package can be downloaded. The hash of packages is determined and recorded at design time. Anyone pulling down and building dependencies will have the hash verified to ensure a deterministic build.

There is no central package repository, instead dependencies are defined as URLs from which the dependency package can be downloaded. This is typically a tar.gz file from GitHub, GitLab or similar source hosting site. Again, the very identity of a version of a package is the conten hash. The URL is only from where to get it.

Acton is statically compiled, all dependencies are fetched and included at compile time. There are no run time dependencies.

## Project lineage fingerprint

Each project can declare a **fingerprint** in `Build.act` (or `build.act.json`) to represent its lineage — the stable identity of the project across versions. This is separate from dependency content hashes:

- Content hashes identify a specific version of a dependency.
- Fingerprints identify the project itself and help Acton deduplicate dependencies and generate consistent build metadata.

Example:
```python
name = "myproject"
fingerprint = 0x1234abcd5678ef00
```

**How it behaves today**
- Fingerprint is optional for now.
- If both `name` and `fingerprint` are set, Acton validates that the fingerprint matches the name’s lineage prefix.
- If they don’t match, the build fails with a message explaining whether this looks like a rename or a fork and how to fix it.

Renaming a project breaks lineage, so generate a new fingerprint for the new name. When you fork a project, also generate a new fingerprint so the fork has its own lineage.
