void hashQ_wyhashQ___ext_init__() {}

void *zig_hash_wyhash_init(uint64_t seed);
void zig_hash_wyhash_update(void *hasher, B_bytes data);
uint64_t zig_hash_wyhash_final(void *hasher);
uint64_t zig_hash_wyhash_hash(uint64_t seed, B_bytes data);

B_NoneType hashQ_wyhashQ_HasherD__init (hashQ_wyhashQ_Hasher self, B_u64 seed) {
    self->_hasher = zig_hash_wyhash_init(fromB_u64(seed));
    return B_None;
}

B_NoneType hashQ_wyhashQ_HasherD_update (hashQ_wyhashQ_Hasher self, B_bytes data) {
    zig_hash_wyhash_update(self->_hasher, data);
    return B_None;
}

B_u64 hashQ_wyhashQ_HasherD_finalize (hashQ_wyhashQ_Hasher self) {
    uint64_t h = zig_hash_wyhash_final(self->_hasher);
    B_u64 result = toB_u64(h);
    return result;
}

uint64_t hashQ_wyhashQ_U_1hash (uint64_t seed, B_bytes data) {
    uint64_t result = zig_hash_wyhash_hash(seed, data);
    return result;
}
