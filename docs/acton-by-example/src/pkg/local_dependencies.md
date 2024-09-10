# Local Dependencies

It is possible to use dependencies available via a local file system path by setting the `path` attribute. Edit `build.act.json` and add or modify an existing dependency. Set the `path` attribute to a relative path on the local file system, e.g.:

```json
{
    "dependencies": {
        "foo": {
            "url": "https://github.com/actonlang/foo/archive/refs/heads/main.zip",
            "hash": "1220cd47344f8a1e7fe86741c7b0257a63567b4c17ad583bddf690eedd672032abdd",
            "path": "../foo"
        },
        "local_lib": {
            "path": "deps/local_lib"
        }
    },
    "zig_dependencies": {}
}
```

```admonish
Setting a local `path` will take priority over a remote `url`. This can be useful to fork a library and make local modifications to it before submitting them back upstream.
```
