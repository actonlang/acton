# Random numbers in tests

`t.random` is a source of random numbers for a test. Each run of the
test draws from a generator with a new seed, so a test that runs many
times tries many inputs. When a run fails, `acton test` shows its seed
and a command that runs the test with that seed again.

Source:
```python
import testing

def clamp(x: int, lo: int, hi: int) -> int:
    if x < lo:
        return lo
    if x > hi:
        return lo
    return x

def _test_clamp(t: testing.SyncT):
    x = t.random.between(-100, 100)
    testing.assertEqual(clamp(x, -50, 50), min([max([x, -50]), 50]))
```

`clamp` has a bug: it returns `lo` where it should return `hi`. Only
the runs that draw an `x` above 50 find it.

Run:
```sh
acton test
```

Output:
```sh
Tests - module example:
   clamp:           FLAKY FAIL     : 1710 runs in 50.176ms @ 34080.0/s
    outcomes: 1305 ok, 405 fail
    testing.NotEqualError: Expected equal values but they are non-equal. A: -50 B: 50
    @@ -1,1 +1,1 @@
    --50
    +50

    seed: 5562776328888921104 (failed the same way in 3 of 3 replays)
    rerun: acton test --module example --name clamp --seed 5562776328888921104

1 out of 1 tests failed ( 3.150 s)
```

Before it reports a failure, `acton test` runs the test three more
times with the seed of the first failed run, and counts how many of
those replays failed the same way. Here all three did, so the rerun
command repeats the failure in every run:

```sh
acton test --module example --name clamp --seed 5562776328888921104
```

Output:
```sh
Tests - module example:
   clamp:           FAIL           : 1430 runs in 50.192ms @ 28490.6/s
    outcomes: 1430 fail
    testing.NotEqualError: Expected equal values but they are non-equal. A: -50 B: 50
    @@ -1,1 +1,1 @@
    --50
    +50

    seed: 5562776328888921104
    rerun: acton test --module example --name clamp --seed 5562776328888921104

1 out of 1 tests failed ( 0.302 s)
```

With `--seed`, every run of the selected tests draws the same numbers.

## Writing tests that replay

A test replays a failure when everything it depends on comes from
`t.random`: its outcome then follows from the seed. Anything else that
changes between runs, such as `env.random`, the clock, the network or
the order in which actors run, can make a replay go differently, and
the count shows fewer failures than replays:

```sh
    seed: 8031129456631170003 (failed the same way in 1 of 3 replays, so the outcome depends on more than t.random)
```

`t.random` implements the builtin
[`Random`](../environment/random.md#the-random-protocol) protocol. Pass
it to code that takes any `Random`, such as the functions in
[`std.random`](../stdlib/random.md):

```python
import std.random as random
import testing

def _test_sorted(t: testing.SyncT):
    items = [t.random.below(1000) for _ in range(20)]
    testing.assertEqual(sorted(random.shuffled(t.random, items)), sorted(items))
```

A test that starts several actors gives each one a generator of its
own, seeded from `t.random` before the actors start:

```python
w1 = Worker(random.Xoshiro256(t.random.rand_u64()))
w2 = Worker(random.Xoshiro256(t.random.rand_u64()))
```

Each worker then draws the same numbers in every replay. Actors that
share one generator change its state at the same time.

`t.random` is on `testing.SyncT`, `testing.AsyncT` and `testing.EnvT`,
so a test that needs random numbers takes one of them as its argument.

## Cached results

`acton test` runs a test that drew from `t.random` every time, with new
seeds, instead of reusing a cached result. With `--seed`, it runs all
selected tests.
