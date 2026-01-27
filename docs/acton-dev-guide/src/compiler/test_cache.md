# Test result cache

Acton caches test results using per-name implementation hashes to decide when a
test must be rerun. The cache is stored in `out/test/cache.json` and is updated
after each `acton test` run.

## Run context hash

Each run computes a context hash that captures inputs that affect test behavior
without changing source code:

- compiler version (`acton` version string)
- target triple
- optimize mode
- test mode (run/perf)
- test runner arguments (iteration and timing limits)

The context hash is SHA-256 over the JSON-encoded context record.

## Per-test hash

Each test’s run hash is SHA-256 over:

```
implHash || depsHash || contextHash
```

Where `implHash` is the per-name implementation hash from the module’s `.ty`
header. `depsHash` is a hash of the test’s current implementation dependencies
(`implDeps`), resolved to their latest impl hashes via the `.ty` headers on the
search path. Test name resolution uses the `.ty` name hash map, and applies
simple fallbacks for generated actor wrappers:

- try the test name as-is
- if it ends in `_wrapper`, try the name without the suffix
- if it begins with `_test_`, try the name without the prefix

If no implementation hash can be resolved, the test is always rerun.

## Cache format

The cache file is JSON:

```
{
  "version": 1,
  "context": { ... },
  "tests": {
    "module:testName": {
      "runHash": "...",
      "implHash": "...",
      "result": { ... }
    }
  }
}
```

The `result` payload mirrors the test runner’s result fields (success/exception,
failures/errors, iterations, and duration).

## Behavior

- If the cached run hash matches the newly computed run hash, the test is
  skipped and its cached result is used.
- Cached failures and errors still contribute to the overall test exit code.
- By default, cached successes are hidden from output; cached failures/errors
  are still shown. Use `--show-cached` to include cached successes.
- Golden file updates and perf data are written only for tests executed in the
  current run.
