# C / C++ / Zig dependencies

Much like dependencies on other Acton packages, an Acton project can depend on a Zig package which could be a C / C++ or Zig library, as long as it has a `build.zig` file.

- `acton zig-pkg add URL NAME --artifact X --artifact Y`
  - list the libraries you want to link with as artifacts
- `acton zig-pkg remove NAME`

~~~admonish example
```
acton zig-pkg add https://github.com/allyourcodebase/zlib/archive/refs/tags/1.3.1.tar.gz zlib --artifacts z
```

```json
{
    "dependencies": {},
    "zig_dependencies": {
        "zlib": {
            "url": "https://github.com/allyourcodebase/zlib/archive/refs/tags/1.3.1.tar.gz",
            "hash": "122034ab2a12adf8016ffa76e48b4be3245ffd305193edba4d83058adbcfa749c107",
            "artifacts": [
                "z"
            ]
        }
    }
}
```

~~~
