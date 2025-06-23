# Override the path to a dependency

The configuration in `build.act.json` sets the path or url that is normally used for a dependency. It is possible to temporarily override the path through the `--dep` argument to `acton build`.

Let's say we have the following configuration:

```json
{
    "dependencies": {
        "foo": {
            "url": "https://github.com/actonlang/foo/archive/refs/tags/v1.0.zip",
            "hash": "1220cd47344f8a1e7fe86741c7b0257a63567b4c17ad583bddf690eedd672032abdd"
        }
    },
    "zig_dependencies": {}
}
```

Now we want to make some modifications to the `foo` library, so we clone it to a local path. We can now build our project using `acton build --dep foo=../foo` to temporarily override the `foo` dependency to use the path `../foo` instead of the url in the configuration.
