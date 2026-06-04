void hashQ_wyhashQ___ext_init__() {}

void *zig_hash_wyhash_init(uint64_t seed);
void zig_hash_wyhash_update(void *hasher, B_bytes data);
uint64_t zig_hash_wyhash_final(void *hasher);
uint64_t zig_hash_wyhash_hash(uint64_t seed, B_bytes data);

B_NoneType hashQ_wyhashQ_HasherD__init (hashQ_wyhashQ_Hasher self, uint64_t seed) {
    self->_hasher = (uint64_t)(uintptr_t)zig_hash_wyhash_init(seed);
    return B_None;
}

B_NoneType hashQ_wyhashQ_HasherD_update (hashQ_wyhashQ_Hasher self, B_bytes data) {
    zig_hash_wyhash_update((void *)(uintptr_t)self->_hasher, data);
    return B_None;
}

uint64_t hashQ_wyhashQ_HasherD_finalize (hashQ_wyhashQ_Hasher self) {
    return zig_hash_wyhash_final((void *)(uintptr_t)self->_hasher);
}

uint64_t hashQ_wyhashQ_hash (uint64_t seed, B_bytes data) {
    uint64_t result = zig_hash_wyhash_hash(seed, data);
    return result;
}
