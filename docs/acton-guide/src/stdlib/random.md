# std.random

The `std.random` module has seeded random number generators, and the
functions `choice`, `sample`, `shuffle` and `shuffled`, which pick from or
reorder a sequence. Both work with any source of random numbers that
implements the builtin
[`Random`](../environment/random.md#the-random-protocol) protocol, including
`env.random`.

Source:
```python
import std.random as random

mut def roll[R(Random)](r: R) -> int:
    return r.between(1, 7)

actor main(env):
    print("Secure roll:", roll(env.random))

    g = random.Xoshiro256(42)
    print("Seeded rolls:", [roll(g) for _ in range(8)])

    cards = ["ace", "king", "queen", "jack", "ten"]
    random.shuffle(g, cards)
    print("Shuffled:", cards)
    print("Drawn:", random.sample(g, cards, 2))
    print("Scrambled:", random.shuffled(g, "acton"))
    env.exit(0)
```

Output:
```sh
Secure roll: 2
Seeded rolls: [2, 6, 1, 5, 6, 2, 3, 1]
Shuffled: ['jack', 'king', 'queen', 'ten', 'ace']
Drawn: ['king', 'queen']
Scrambled: ctoan
```

The secure roll differs from run to run. Everything drawn from
`Xoshiro256(42)` is the same every time.

## Seeded generators

`Xoshiro256(seed)` produces a fixed sequence from a seed. It is fast but
predictable, so it suits simulations, tests and sampling, but never secrets;
use [`env.random`](../environment/random.md) for those. It has the same
methods as `env.random`: `rand_u64()`, `rand_int()`, `rand_float()`,
`rand_bytes(n)`, `below(n)` and `between(lo, hi)`.

For a sequence that differs between runs but can be replayed, seed a
generator from `env.random` and record the seed:

```python
seed = env.random.rand_u64()
print("seed:", seed)
g = random.Xoshiro256(seed)
```

## Picking from a sequence

| Function | Returns |
| --- | --- |
| `choice(r, items)` | a random element of `items` |
| `sample(r, items, k)` | a `list` of `k` elements from distinct positions of `items`, in random order |
| `shuffle(r, items)` | nothing; puts the elements of `items` in a random order, in place |
| `shuffled(r, items)` | a copy of `items`, of the same type, with the elements in a random order |

`r` is any source of random numbers, such as `env.random` or a seeded
generator. `items` is any sequence with a length and `int` indexes, such as
a `list`, `ilist`, `str`, `bytes` or `bytearray`. `shuffle` changes `items`,
so it takes a mutable sequence, such as a `list` or `bytearray`; for others,
use `shuffled`, which leaves `items` as it is. With the same draws from `r`,
`shuffled` gives the order that `shuffle` would.

`choice` of an empty sequence, or a `k` outside `0` to `len(items)`, raises
`ValueError`.

## Reproducibility

The same seed gives the same sequence on every platform. The methods of
`Random` and the functions in `std.random` are implemented in Acton itself,
not taken from a library that might change them, so a seeded sequence also
stays the same from one Acton release to the next. `Xoshiro256(seed)`
matches Zig's `std.Random.Xoshiro256.init(seed)`.
