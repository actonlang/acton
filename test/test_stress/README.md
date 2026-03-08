# test_stress

Single Acton project used to exercise `acton test stress`.

## Modules

- `test_racy_ffi`: intentionally racy FFI behaviors (value races / publication races)
- `test_segv_ffi`: stress-only parallel overlap path that crashes with `SIGSEGV`

## Run

Normal test mode (expected to pass):

```sh
../../dist/bin/acton test --module test_racy_ffi --module test_segv_ffi
```

Stress mode on race module (expected fail/flaky):

```sh
../../dist/bin/acton test stress --module test_racy_ffi --show-log
```

Stress mode on segfault module (expected process error `-11`):

```sh
../../dist/bin/acton test stress --module test_segv_ffi --show-log
```
