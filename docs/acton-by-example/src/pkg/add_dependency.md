# Add Dependency

Add a dependency to your project by using `acton pkg add` and providing the URL to the project and a local reference name.

In this case we add the example `foo` package as a dependency.
```console
acton pkg add https://github.com/actonlang/foo/archive/refs/heads/main.zip foo
```

This will fetch the dependency and add it to the `build.act.json` file of your local project, resulting in something like:
```json
{
    "dependencies": {
        "foo": {
            "url": "https://github.com/actonlang/foo/archive/refs/heads/main.zip",
            "hash": "1220cd47344f8a1e7fe86741c7b0257a63567b4c17ad583bddf690eedd672032abdd"
        }
    },
    "zig_dependencies": {}
}
```

```admonish
It is possible to edit `build.act.json` by hand, but adding dependencies, which requires filling in the 'hash' field requires computing the hash which is somewhat tricky.
```

The `foo` package provides a single `foo` module with a `foo` function (that appropriately returns `foo`). We can now access it from our main actor:

```python
import foo

actor main(env):
    print(foo.foo())
    env.exit(0)
```
