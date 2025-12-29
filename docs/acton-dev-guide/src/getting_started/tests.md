# Tests

The top-level Makefile drives test execution and wires together compiler,
stdlib, backend, and runtime suites.

## Run everything

```sh
make test
```

This runs:
- `stack test` for the compiler packages
- stdlib tests (`make test-stdlib`)
- backend tests (`make -C backend test`)
- runtime/db tests (`make -C test`)

## Targeted suites

```sh
make test-compiler
make test-builtins
make test-stdlib
make test-rts
make test-backend
```

## Golden / acceptance tests

Some compiler suites have explicit accept targets that update expected output:

```sh
make test-rebuild
make test-rebuild-accept
make test-syntaxerrors
make test-syntaxerrors-accept
make test-typeerrors
make test-typeerrors-accept
```

`test-rebuild*` requires `dist/bin/actonc` (it is built automatically by the target).

## Project tests with actonc

Run tests for the current Acton project (expects a `Build.act` in the cwd):

```sh
actonc test
```

Example output:

```text
$ actonc test
Building project in /path/to/project
  Final compilation step
   Finished final compilation step in   0.462 s
Test results:
  c.bar:                 OK:  445 runs in 50.160ms
  c.foo:                 OK:  444 runs in 50.188ms
All 2 tests passed ( 0.528 s)
```

## Tips

- Prefer `make test-compiler` when iterating on Haskell changes.
- Use `dist/bin/acton test` for Acton project-level tests in other repos.
