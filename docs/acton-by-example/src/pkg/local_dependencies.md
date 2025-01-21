# Local Dependencies

It is possible to use dependencies available via a local file system path by setting the `path` attribute. Edit `build.act.json` and add or modify an existing dependency. Set the `path` attribute to a relative path, e.g.:

```json
{
    "dependencies": {
        "foo": {
            "path": "../foo"
        },
        "local_lib": {
            "path": "deps/local_lib"
        }
    },
    "zig_dependencies": {}
}
```

These are best used for dependencies located within the same git repository or similar. All users need to have the same relative path to the dependency so if the paths stretch over multiple repositories, the user needs to keep the paths aligned.

```admonish
You can temporarily override the path to a dependency through the `--dep` argument, e.g. `acton build --dep foo=../foo`. This can be useful to fork a library and make local modifications to it before submitting them back upstream.
```
