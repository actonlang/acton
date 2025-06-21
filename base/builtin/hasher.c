B_NoneType B_hasherD___init__ (B_hasher self, B_u64 seed) {
    self->_hasher = zig_hash_wyhash_init(seed ? fromB_u64(seed) : 0);
    return B_None;
}

B_NoneType B_hasherD_update (B_hasher self, B_bytes data) {
    zig_hash_wyhash_update(self->_hasher, data);
    return B_None;
}

B_u64 B_hasherD_finalize (B_hasher self) {
    uint64_t h = zig_hash_wyhash_final(self->_hasher);
    B_u64 result = toB_u64(h);
    return result;
}

B_bool B_hasherD___bool__(B_hasher h) {
    return B_True;
}

B_str B_hasherD___str__(B_hasher self) {
    return $FORMAT("<hasher object at %p>", self);
}

B_str B_hasherD___repr__(B_hasher self) {
    return $FORMAT("<hasher object at %p>",self);
}

B_hasher B_hasherG_new(B_u64 seed) {
    return $NEW(B_hasher, seed);
}
 
void B_hasherD___serialize__(B_hasher self, $Serial$state state) {
    // TODO
}

B_hasher B_hasherD___deserialize__(B_hasher self, $Serial$state state) {
    // TODO
    return B_hasherG_new(toB_u64(0));
}

