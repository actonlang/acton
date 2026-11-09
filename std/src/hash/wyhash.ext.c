void stdQ_hashQ_wyhashQ___ext_init__() {}

void *zig_hash_wyhash_init(uint64_t seed);
void zig_hash_wyhash_update(void *hasher, const uint8_t *ptr, size_t len);
uint64_t zig_hash_wyhash_final(void *hasher);
uint64_t zig_hash_wyhash_hash(uint64_t seed, B_bytes data);

B_NoneType stdQ_hashQ_wyhashQ_HasherD__init (stdQ_hashQ_wyhashQ_Hasher self, uint64_t seed) {
    // A GC pointer in an int attribute: the barrier is up to this code
    void *hasher = zig_hash_wyhash_init(seed);
    self->_hasher = (uint64_t)(uintptr_t)hasher;
    ACTON_WBAR(self, &self->_hasher, hasher);
    return B_None;
}

B_NoneType stdQ_hashQ_wyhashQ_HasherD_update (stdQ_hashQ_wyhashQ_Hasher self, B_bytes data) {
    zig_hash_wyhash_update((void *)(uintptr_t)self->_hasher, data->str, data->nbytes);
    return B_None;
}

uint64_t stdQ_hashQ_wyhashQ_HasherD_finalize (stdQ_hashQ_wyhashQ_Hasher self) {
    return zig_hash_wyhash_final((void *)(uintptr_t)self->_hasher);
}

uint64_t stdQ_hashQ_wyhashQ_hash (uint64_t seed, B_bytes data) {
    uint64_t result = zig_hash_wyhash_hash(seed, data);
    return result;
}
