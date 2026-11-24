// SHA-256 itself comes from Zig's std.crypto, see base/__root.zig. This file
// allocates the state of a Hasher and the digests.

size_t zig_crypto_hash_sha256_size(void);
void zig_crypto_hash_sha256_init(void *storage);
void zig_crypto_hash_sha256_update(void *hasher, const unsigned char *data, size_t len);
void zig_crypto_hash_sha256_finalize(const void *hasher, unsigned char *out);
void zig_crypto_hash_sha256_hash(const unsigned char *data, size_t len, unsigned char *out);

#define SHA256_DIGEST_SIZE 32

void stdQ_cryptoQ_hashQ_sha256Q___ext_init__() {}

// The state of a hasher and a digest hold no pointers, so they are allocated
// atomic.
static void *sha256_alloc(size_t size) {
    void *buf = acton_malloc_atomic(size);
    if (buf == NULL)
        RAISE(B_MemoryError, actStrFromCString("Out of memory in SHA-256"));
    return buf;
}

B_NoneType stdQ_cryptoQ_hashQ_sha256Q_HasherD__init (stdQ_cryptoQ_hashQ_sha256Q_Hasher self) {
    // The state comes from acton_malloc_atomic, as the Hasher object comes
    // from acton_malloc: both are memory of the collector, or neither is.
    void *hasher = sha256_alloc(zig_crypto_hash_sha256_size());
    zig_crypto_hash_sha256_init(hasher);
    // A GC pointer in an int attribute: the barrier is up to this code
    self->_hasher = (uint64_t)(uintptr_t)hasher;
    ACTON_WBAR(self, &self->_hasher, hasher);
    return B_None;
}

B_NoneType stdQ_cryptoQ_hashQ_sha256Q_HasherD_update (stdQ_cryptoQ_hashQ_sha256Q_Hasher self, B_bytes data) {
    zig_crypto_hash_sha256_update((void *)(uintptr_t)self->_hasher, data->str, (size_t)data->nbytes);
    return B_None;
}

B_bytes stdQ_cryptoQ_hashQ_sha256Q_HasherD_finalize (stdQ_cryptoQ_hashQ_sha256Q_Hasher self) {
    unsigned char *digest = sha256_alloc(SHA256_DIGEST_SIZE);
    zig_crypto_hash_sha256_finalize((const void *)(uintptr_t)self->_hasher, digest);
    return actBytesFromCStringLength((const char *)digest, SHA256_DIGEST_SIZE);
}

B_bytes stdQ_cryptoQ_hashQ_sha256Q_hash (B_bytes data) {
    unsigned char *digest = sha256_alloc(SHA256_DIGEST_SIZE);
    zig_crypto_hash_sha256_hash(data->str, (size_t)data->nbytes, digest);
    return actBytesFromCStringLength((const char *)digest, SHA256_DIGEST_SIZE);
}
