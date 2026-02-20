# Snapshot testing

Snapshot tests compare a produced string with a stored expected value. You can produce snapshot output from unit/sync test functions by returning `str`, or from async/env tests by calling `t.success("...")`.

```python
import testing

def _test_rendered_profile() -> str:
    return '{"name":"Alice","role":"admin"}'
```

Acton writes snapshot files in your project under `snapshots/output/<module>/<test_name>` (latest produced value) and `snapshots/expected/<module>/<test_name>` (expected value used for comparison). Test names in those paths use the display test name, so `_test_foo` becomes `foo`.

When an expected snapshot differs (or is missing), running tests normally shows a mismatch:

```sh
$ acton test
Building project in /home/user/example
...
Tests - module main:
   rendered_profile: FAIL           :  254 runs in 50.045ms
    testing.NotEqualError: Test output does not match expected snapshot value.
    @@ -1,1 +1,1 @@
    -{"name":"Alice","role":"user"}
    +{"name":"Alice","role":"admin"}

1 out of 1 tests failed (0.244s)
```

When the new output is correct, accept it as the new expected value with:

```sh
$ acton test --accept
Building project in /home/user/example
...
Tests - module main:
   rendered_profile: UPDATED        :  243 runs in 50.445ms

All 1 tests passed (0.246s)
```

`--accept` is the idiomatic flag (aliases: `--snapshot-update`, `--golden-update`).

For tests that produce snapshot output, `snapshots/output/...` is written on every run. That is intentional: it makes it easy to use external diff tools (`diff`, `vimdiff`, `meld`, etc.) against `snapshots/expected/...` without any extra export step.

For example, to inspect the current snapshot difference for the test above:

```sh
diff -u snapshots/expected/main/rendered_profile snapshots/output/main/rendered_profile
```

## Common workflow

1. Run `acton test`.
2. If there is a snapshot mismatch, inspect it directly, for example:

```sh
diff -u snapshots/expected/main/rendered_profile snapshots/output/main/rendered_profile
```

3. Accept changes with `acton test --accept`.
4. Run `acton test` again to confirm everything passes.

An alternative workflow is to accept first, then inspect version-controlled snapshot changes with Git:

1. Run `acton test --accept`.
2. Review what changed with `git diff -- snapshots/expected`.
3. Keep or revert changes before commit, then run `acton test`.
