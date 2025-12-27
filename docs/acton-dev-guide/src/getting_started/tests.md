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

## Tips

- Prefer `make test-compiler` when iterating on Haskell changes.
- Use `dist/bin/acton test` for Acton project-level tests in other repos.
