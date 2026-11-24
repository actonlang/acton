# std.crypto.hash.sha256

The `std.crypto.hash.sha256` module computes SHA-256 digests. SHA-256 maps
data of any length to a digest of 32 bytes. The same data always gives the
same digest, and no practical way is known to find two different inputs with
the same digest. Programs use SHA-256 digests to name data by its content and
to check that a file or a message is unchanged.

The implementation comes from Zig's standard library. Whether it uses the
CPU's SHA-256 instructions is decided when the program is built, from the CPU
that the build names (`--cpu`): the default build for ARM64 uses them, the
default build for x86_64 does not.

Source:
```python
import std.crypto.hash.sha256 as sha256

actor main(env):
    proc def __startup__():
        digest = sha256.hash(b"abc")
        print(len(digest), "bytes:", digest.hex())

        h = sha256.Hasher()
        h.update(b"a")
        h.update(b"bc")
        print("In pieces:", h.finalize().hex())

        env.exit(0)
```

Output:
```sh
32 bytes: ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
In pieces: ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
```

## Digests

`hash(data)` returns the digest of a `bytes` value as 32 bytes, the value of
the constant `DIGEST_SIZE`. `digest.hex()` gives the usual text form of 64
hexadecimal digits.

## Data in pieces

A `Hasher` computes the digest of data that arrives in pieces, such as a file
that is read block by block, without holding all of it in memory.
`update(data)` adds a piece after the pieces given so far. The digest depends
only on the bytes and their order, not on where the data was split into
pieces.

`finalize()` returns the digest of the data given so far and does not change
the hasher. `update` can add more data afterwards, and the next `finalize`
returns the digest of all of it.

## What a digest shows

SHA-256 has no key, so anyone can compute the digest of any data. A digest
shows that data is unchanged only when the digest itself comes from a
trusted place.

Do not store passwords as SHA-256 digests. SHA-256 is fast, which makes
guessing passwords fast too.
