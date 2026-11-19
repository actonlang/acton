# Random numbers

`env.random` draws secure random numbers from the operating system. Use
it for keys, tokens, nonces and anything else that must not be
guessable.

Source:
```python
def new_token(rand: EnvRandom) -> str:
    return rand.rand_bytes(16).hex()

actor main(env):
    print("Token:", new_token(env.random))
    print("Die:", env.random.between(1, 7))
    env.exit(0)
```

Output, which differs from run to run:
```sh
Token: 139229e30d91695c5d8e345610b7293f
Die: 4
```

## Methods

| Method | Returns |
| --- | --- |
| `rand_u64()` | a random `u64` |
| `rand_int()` | a random `int` `x` with `x >= 0` |
| `rand_float()` | a random `float` `x` with `0.0 <= x < 1.0` |
| `rand_bytes(n)` | `n` random bytes |
| `below(n)` | a random `int` `x` with `0 <= x < n` |
| `between(lo, hi)` | a random `int` `x` with `lo <= x < hi` |

`between(lo, hi)` never returns `hi`, so a die roll is `between(1, 7)`.
Every value in the range is equally likely; the methods do not favour
small values the way `rand_u64() % n` would. Invalid arguments, such as
`n <= 0` or `lo >= hi`, raise `ValueError`.

## The Random protocol

The methods come from the builtin `Random` protocol. A source of random
numbers implements `rand_u64()`, and the protocol builds the other
methods on it. `env.random` implements it, and so do the seeded
generators in [`std.random`](../stdlib/random.md), which also has
`choice`, `sample`, `shuffle` and `shuffled` for sequences.

Write code that needs random numbers to take a source as an argument.
Take any `Random` when the numbers need not be secret, so callers can
pass `env.random` in a program and a seeded generator in a test:

```python
mut def roll[R(Random)](r: R) -> int:
    return r.between(1, 7)
```

Take `EnvRandom` when they must be secure, as `new_token` does above, so
that only `env.random` will do.

## How it works

`env.random` is a capability of type `EnvRandom`: only code that is
given it can draw random numbers from it.

Each thread has its own ChaCha8 generator, seeded from the operating
system the first time the thread draws from it. If the operating system
cannot provide a seed, the methods raise `OSError` rather than fall back
to something weaker.

`env.random` is a class, so its methods run directly in the calling
actor and return their result at once, without a message to `Env`.
