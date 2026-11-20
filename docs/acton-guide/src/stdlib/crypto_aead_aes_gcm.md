# std.crypto.aead.aes_gcm

The `std.crypto.aead.aes_gcm` module encrypts data with AES-GCM, the
authenticated encryption mode used by TLS 1.3 and most other modern
protocols. Authenticated means that decryption fails if the ciphertext was
modified, truncated or encrypted under another key, instead of returning
garbage. The implementation comes from Zig's standard library and uses the
CPU's AES instructions where the target has them.

Source:
```python
import std.crypto.aead.aes_gcm as aes_gcm

actor main(env):
    key = aes_gcm.generate_key(env.random)

    ciphertext = aes_gcm.encrypt(env.random, key, b"attack at dawn", b"order 17")
    print("Ciphertext is", len(ciphertext), "bytes")

    plaintext = aes_gcm.decrypt(key, ciphertext, b"order 17")
    print("Decrypted:", plaintext.decode())

    try:
        aes_gcm.decrypt(key, ciphertext, b"order 18")
    except aes_gcm.DecryptionError as e:
        print("Rejected:", e.error_message)

    env.exit(0)
```

Output:
```sh
Ciphertext is 42 bytes
Decrypted: attack at dawn
Rejected: AES-GCM authentication failed
```

## Keys

`generate_key(env.random)` returns a random 32-byte AES-256 key drawn from
[`env.random`](../environment/random.md), the secure random numbers from the
operating system. `encrypt` and `decrypt` also accept a 16-byte AES-128 key,
for data shared with systems that use AES-128. Any other key length raises
`ValueError`. Keep keys secret and store them apart from the data they
protect.

## Ciphertext format

`encrypt(env.random, key, plaintext, associated_data=b"")` draws a new
random 12-byte nonce from `env.random` for every call and returns

```
nonce (12 bytes) | encrypted data | tag (16 bytes)
```

so the result is 28 bytes longer than the plaintext, and encrypting the same
plaintext twice gives different ciphertexts. Any AES-GCM implementation can
decrypt it by splitting off the first 12 bytes as the nonce and the last 16
bytes as the tag. The constants `KEY_SIZE`, `NONCE_SIZE` and `TAG_SIZE` give
these sizes.

Encrypt at most 2<sup>32</sup> messages with one key. Beyond that the chance
that two random nonces collide becomes significant, and a repeated nonce
reveals the XOR of the two plaintexts and lets an attacker forge messages.

## Associated data

`associated_data` is authenticated but not encrypted, and it is not stored
in the ciphertext. Use it to bind a ciphertext to its context, such as a
record ID, so the ciphertext cannot be moved to another record.
`decrypt(key, ciphertext, associated_data=b"")` must be given the same
associated data that `encrypt` was given.

## Errors

`decrypt` raises `DecryptionError` when the ciphertext does not
authenticate. That happens when the key or associated data differs from what
`encrypt` was given, or when the ciphertext was modified or truncated;
AES-GCM cannot tell these cases apart. `DecryptionError` is a subclass of
`ValueError`.
