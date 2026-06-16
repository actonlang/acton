# Local Dependencies

It is possible to use dependencies available via a local file system path by setting the `path` attribute. Edit `Build.act` and add or modify an existing dependency. Set the `path` attribute to a relative path, e.g.:

```python
dependencies = {
  "foo": (
        path="../foo"
    ),
  "local_lib": (
        path="deps/local_lib"
    ),
}

zig_dependencies = {}
```

These are best used for dependencies located within the same git repository or similar. All users need to have the same relative path to the dependency so if the paths stretch over multiple repositories, the user needs to keep the paths aligned.

The dependency key is still the import prefix, regardless of whether the
dependency is local or fetched from a repository. In the example above,
`"foo"` makes `../foo/src/lib.act` available as `import foo` and
`../foo/src/client.act` available as `import foo.client`.

```admonish
You can temporarily override the path to a dependency through the `--dep` argument, e.g. `acton build --dep foo=../foo`. This can be useful to fork a library and make local modifications to it before submitting them back upstream.
```
