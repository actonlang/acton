# Dynamic Module Prototype

This fixture is intentionally small. `Build.act` asks the builder to put module
`a` in its own dynamic library, while module `b` remains an ordinary executable
that imports `a`.

Prototype commands from this directory:

```sh
acton build --color never src/b.act
./out/bin/b
otool -L out/bin/b
# Edit src/a.act to return a different string.
acton build --color never src/a.act
./out/bin/b
```

The executable should print `hello`, and the dynamic loader view should include
`liba.dylib` on macOS or `liba.so` on Linux. After editing and rebuilding only
`src/a.act`, rerunning the existing executable should print the updated string.
The executable rpath searches the default `../lib` output layout, then `lib`
next to the executable, then the executable directory itself.
