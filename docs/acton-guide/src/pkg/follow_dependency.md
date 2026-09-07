# Follow a Dependency

A project must declare every package whose modules it imports. Sometimes
another dependency already selects the package version you want to use.
Declare `follows` to share that selection without specifying another
archive URL, content hash, or local path.

For example, suppose a fictional `garden` package depends on a `seeds`
package. An application that imports modules from both can use:

```python
dependencies = {
    "garden": (path="../garden"),
    "seeds": (follows="garden.seeds"),
}
```

The application can then write `import seeds` or `import seeds.catalog`.
The `seeds` declaration points to the same selected project as
`garden`'s `seeds` dependency. The dependency key must still match the
selected project's name.

The reference is a path through dependency names in `Build.act`. Its
first component names a dependency of the declaring project, and each
remaining component names a dependency of the project reached so far.
Longer references such as `toolbox.garden.seeds` are supported, as
are references whose target is itself a following dependency. The
reference must contain at least two names, and every named dependency
must exist. Cyclic references are errors.

`follows` does not choose a version or override another declaration.
Acton first resolves the concrete path and archive dependencies, including
root pins and project deduplication, then binds following dependencies to
the selected projects. If another concrete declaration causes `garden`
to use a different `seeds` version, the application's following dependency
uses that same version. Adding the following declaration cannot change
which version wins.

Relative paths still belong to the project that declares them. If
`garden` declares `seeds` with `path="../seeds"`, that path is resolved
relative to `garden`'s directory, regardless of where the application
lives.

Use `follows` on its own in a dependency tuple. It cannot be combined with
`path`, `url`, `hash`, `repo_url`, or `repo_ref`. To select a different
version yourself, replace the following declaration with a concrete
dependency.

`acton pkg upgrade` preserves following declarations. Upgrade the dependency
that supplies the concrete version instead. A `--dep seeds=...` override
changes concrete `seeds` declarations; following declarations observe that
selection without becoming concrete dependencies themselves.
