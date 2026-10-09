B_NoneType B_hasherD___init__ (B_hasher self, B_u64 seed) {  // seed is optional
    self->_hasher = zig_hash_wyhash_init(seed ? fromB_u64(seed) : 0);
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

B_hasher B_hasherG_new(B_u64 seed) {
    return $NEW(B_hasher, seed);
}

uint64_t B_hash(B_Hashable wit, $WORD value) {
    // These builtins each hash one contiguous buffer.  Use Wyhash's one-shot
    // operation so these common dict/set paths do not allocate an Acton hasher
    // and its Zig state.  Other Hashable implementations retain the streaming
    // protocol.
#define HASH_SCALAR(type)                                                        \
    if (wit == (B_Hashable)B_HashableD_##type##G_witness) {                     \
        B_##type scalar = (B_##type)value;                                       \
        return zig_hash_wyhash_hash_buffer(                                      \
            0, (const uint8_t *)&scalar->val, sizeof(scalar->val));              \
    }

    HASH_SCALAR(int)
    HASH_SCALAR(u64)
    if (wit == (B_Hashable)B_HashableD_strG_witness) {
        B_str str_value = (B_str)value;
        return zig_hash_wyhash_hash_buffer(0, str_value->str, str_value->nbytes);
    }
    if (wit == (B_Hashable)B_HashableD_bytesG_witness) {
        B_bytes bytes_value = (B_bytes)value;
        return zig_hash_wyhash_hash_buffer(0, bytes_value->str, bytes_value->nbytes);
    }
    if (wit == (B_Hashable)B_HashableD_floatG_witness) {
        B_float float_value = (B_float)value;
        // Equal positive and negative zero must hash identically.
        double val = float_value->val == 0.0 ? 0.0 : float_value->val;
        return zig_hash_wyhash_hash_buffer(
            0, (const uint8_t *)&val, sizeof(val));
    }
    if (wit == (B_Hashable)B_HashableD_complexG_witness) {
        B_complex complex_value = (B_complex)value;
        double parts[2];
        B_complex_hash_parts(complex_value->val, parts);
        return zig_hash_wyhash_hash_buffer(
            0, (const uint8_t *)parts, sizeof(parts));
    }
    HASH_SCALAR(bool)
    HASH_SCALAR(i32)
    HASH_SCALAR(i16)
    HASH_SCALAR(i8)
    HASH_SCALAR(u32)
    HASH_SCALAR(u16)
    HASH_SCALAR(u8)
    HASH_SCALAR(u1)

#undef HASH_SCALAR

    B_hasher h = B_hasherG_new(NULL);
    wit->$class->hash(wit, value, h);
    return B_hasherD_finalize(h);
}

void B_hasherD___serialize__(B_hasher self, $Serial$state state) {
    // TODO
}

B_hasher B_hasherD___deserialize__(B_hasher self, $Serial$state state) {
    // TODO
    return B_hasherG_new(toB_u64(0));
}
