# Package Management

`acton` offers integrated package management to declare dependencies on other Acton packages and automatically download them from their sources on the Internet.

The guiding principle behind Actons package management is to strive for determinism, robustness and safety. This is primarily achieved by only resolving dependencies at design time. That is, it is the developer of a particular Acton package that determines its exact dependencies and not whomever might be downloading and building said package. The very identity of a package at a particular point in time, which can be thought of as the version of a package, is the hash of its content. This is the foundation of Actons package management. Each dependency has a hash of its content. A URL is just one place from which this particular version of a package can be downloaded. The hash of packages is determined and recorded at design time. Anyone pulling down and building dependencies will have the hash verified to ensure a deterministic build.

There is no central package repository, instead dependencies are defined as URLs from which the dependency package can be downloaded. This is typically a tar.gz file from GitHub, GitLab or similar source hosting site. Again, the very identity of a version of a package is the conten hash. The URL is only from where to get it.

Acton is statically compiled, all dependencies are fetched and included at compile time. There are no run time dependencies.
