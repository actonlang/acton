# Add Dependency

Add a dependency to your project by using `acton pkg add`. The usual
flow is to refresh the public package index first:

```console
acton pkg update
```

Then add the dependency by package name:

```console
acton pkg add foo
```

You can also provide a GitHub repository URL directly. The first
argument is still the local dependency name:

In this case we add the example `foo` package as a dependency.
```console
acton pkg add foo --repo-url https://github.com/actonlang/foo --repo-ref main
```

This will fetch the dependency and add it to the `dependencies` block in
`Build.act`, resulting in something like:
```python
dependencies = {
  "foo": (
        repo_url="https://github.com/actonlang/foo",
        repo_ref="main",
        url="https://github.com/actonlang/foo/archive/0123456789abcdef0123456789abcdef01234567.zip",
        hash="1220cd47344f8a1e7fe86741c7b0257a63567b4c17ad583bddf690eedd672032abdd",
    ),
}

zig_dependencies = {}
```

```admonish
It is possible to edit `Build.act` by hand, but adding dependencies requires filling in the `hash` field, which is somewhat tricky.
```

The dependency key is the import prefix. In the example above, the key
is `"foo"`, so modules from that dependency are imported under `foo`.

| File in the `foo` dependency | Import from your project |
| --- | --- |
| `src/lib.act` | `import foo` |
| `src/bar.act` | `import foo.bar` |
| `src/a/b.act` | `import foo.a.b` |
| `src/foo.act` | `import foo.foo` |

`src/lib.act` is the dependency's root module. If the `foo` package
defines a `foo` function in `src/lib.act`, we can access it from our
main actor:

```python
import foo

actor main(env):
    print(foo.foo())
    env.exit(0)
```

For a repository containing multiple Acton projects, select a project with
`--subdir`:

```console
acton pkg add widgets --repo-url https://github.com/example/monorepo --subdir packages/widgets
```

You can also combine `--subdir` with `--url` for a direct archive URL. The
dependency declaration records the selection:

```python
dependencies = {
    "widgets": (
        url="https://example.com/monorepo.tar.gz",
        hash="...",  # Filled in by acton pkg add
        subdir="packages/widgets",
    ),
}
```

`subdir` is the path to the project within the archive. Omit the outer wrapper
directory, such as `monorepo-<commit>`, from this path.

Acton preserves the fetched package's directory layout, so the selected project
can use a sibling dependency such as `path="../common"`. Multiple dependencies
can select different projects from the same archive and share its hash.

Omitting `subdir` selects the archive root. `acton pkg upgrade` preserves the
selection. Running `acton pkg add` again also preserves it unless you supply
`--subdir`; use `--subdir .` to select the root explicitly.
