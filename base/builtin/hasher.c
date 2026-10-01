B_NoneType B_hasherD___init__ (B_hasher self, uint64_t seed) {
    self->_hasher = zig_hash_wyhash_init(seed);
    return B_None;
}

B_NoneType B_hasherD_update (B_hasher self, B_bytes data) {
    zig_hash_wyhash_update(self->_hasher, data->str, data->nbytes);
    return B_None;
}

uint64_t B_hasherD_finalize (B_hasher self) {
    uint64_t h = zig_hash_wyhash_final(self->_hasher);
    return h;
}

bool B_hasherD___bool__(B_hasher h) {
    return true;
}

B_str B_hasherD___str__(B_hasher self) {
    return $FORMAT("<hasher object at %p>", self);
}

B_str B_hasherD___repr__(B_hasher self) {
    return $FORMAT("<hasher object at %p>",self);
}

B_hasher B_hasherG_new(uint64_t seed) {
    return $NEW(B_hasher, seed);
}

uint64_t B_hash(B_Hashable wit, $WORD value) {
    // These builtins each hash one contiguous buffer.  Use Wyhash's one-shot
    // operation so these common dict/set paths do not allocate an Acton hasher
    // and its Zig state.  Other Hashable implementations retain the streaming
    // protocol.
    if (wit == (B_Hashable)B_HashableD_intG_witness) {
        B_int int_value = (B_int)value;
        return zig_hash_wyhash_hash_buffer(
            0, (const uint8_t *)&int_value->val, sizeof(int_value->val));
    }
    if (wit == (B_Hashable)B_HashableD_u64G_witness) {
        B_u64 u64_value = (B_u64)value;
        return zig_hash_wyhash_hash_buffer(
            0, (const uint8_t *)&u64_value->val, sizeof(u64_value->val));
    }
    if (wit == (B_Hashable)B_HashableD_strG_witness) {
        B_str str_value = (B_str)value;
        return zig_hash_wyhash_hash_buffer(0, str_value->str, str_value->nbytes);
    }
    if (wit == (B_Hashable)B_HashableD_bytesG_witness) {
        B_bytes bytes_value = (B_bytes)value;
        return zig_hash_wyhash_hash_buffer(0, bytes_value->str, bytes_value->nbytes);
    }

    B_hasher h = B_hasherG_new(NULL);
    wit->$class->hash(wit, value, h);
    return B_hasherD_finalize(h);
}

void B_hasherD___serialize__(B_hasher self, $Serial$state state) {
    // TODO
}

B_hasher B_hasherD___deserialize__(B_hasher self, $Serial$state state) {
    // TODO
    return B_hasherG_new(0);
}
