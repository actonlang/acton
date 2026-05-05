# Package Index

The public package index is built from GitHub repository topics. A
repository can opt in as a library, an app, or both. The generated
index records this as a `kinds` list:

- `acton-library` marks an importable library package.
- `acton-app` marks an installable application package.

Use `acton-library` for packages that other projects import and add as
dependencies. These are the only packages used by `acton pkg add` and
shown by `acton pkg search`.

Use `acton-app` for projects that produce executable tools. App packages
are intended for commands that install binaries, for example into
`~/.acton/bin`.

Add both topics when a repository provides useful importable modules and
also builds executable tools. `acton pkg` still uses only the library
entry for dependency resolution.

## Add your package

To have a package discovered by the public index:

1. Put `Build.act` at the root of the repository.
2. Declare the mandatory `name` and `fingerprint` fields in `Build.act`.
3. Publish the repository on GitHub.
4. Add the `acton-library` topic, the `acton-app` topic, or both.

After the public index has refreshed, users can update their local copy:

```console
acton pkg update
```

Libraries can then be found with:

```console
acton pkg search PACKAGE
```

and added as dependencies with:

```console
acton pkg add PACKAGE
```
