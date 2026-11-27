void optional_scalarsQ___ext_init__(void) {}
$WORD optional_scalarsQ_round_trip($WORD value) {
    return $deserialize($serialize(($Serializable)value, NULL), NULL);
}
B_float optional_scalarsQ_NativeD_read(optional_scalarsQ_Native self) { return self->item; }
static const uint64_t float_patterns[] = {
    UINT64_C(0), UINT64_C(0x8000000000000000),
    UINT64_C(0x7ff0000000000000), UINT64_C(0xfff0000000000000),
    UINT64_C(0x7ff8123456789abc), UINT64_C(0xfff8000000000001),
    UINT64_C(0x0000000000000001), UINT64_C(0x7fefffffffffffff)
};
B_float optional_scalarsQ_float_sample(int64_t index) {
    B_float value = acton_malloc(sizeof *value);
    value->$class = &B_floatG_methods;
    memcpy(&value->val, &float_patterns[index], sizeof value->val);
    return value;
}
uint64_t optional_scalarsQ_expected_bits(int64_t index) { return float_patterns[index]; }
uint64_t optional_scalarsQ_float_bits(B_float value) {
    uint64_t bits;
    assert(value);
    memcpy(&bits, &value->val, sizeof bits);
    return bits;
}
bool optional_scalarsQ_layout_checks(void) {
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->integer), int64_t), "int payload width");
    _Static_assert(sizeof(B_OptionalInt) == 16, "initialized tagged int storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->real), double), "float payload width");
    _Static_assert(sizeof(B_OptionalFloat) == 16, "initialized tagged float storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->flag), bool), "bool payload width");
    _Static_assert(sizeof(B_OptionalBool) == 16, "initialized tagged bool storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->si32), int32_t), "i32 payload width");
    _Static_assert(sizeof(B_OptionalI32) == 16, "initialized tagged i32 storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->si16), int16_t), "i16 payload width");
    _Static_assert(sizeof(B_OptionalI16) == 16, "initialized tagged i16 storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->si8), int8_t), "i8 payload width");
    _Static_assert(sizeof(B_OptionalI8) == 16, "initialized tagged i8 storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->ui64), uint64_t), "u64 payload width");
    _Static_assert(sizeof(B_OptionalU64) == 16, "initialized tagged u64 storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->ui32), uint32_t), "u32 payload width");
    _Static_assert(sizeof(B_OptionalU32) == 16, "initialized tagged u32 storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->ui16), uint16_t), "u16 payload width");
    _Static_assert(sizeof(B_OptionalU16) == 16, "initialized tagged u16 storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->ui8), uint8_t), "u8 payload width");
    _Static_assert(sizeof(B_OptionalU8) == 16, "initialized tagged u8 storage");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Scalars)0)->bit), uint8_t), "u1 payload width");
    _Static_assert(sizeof(B_OptionalU1) == 16, "initialized tagged u1 storage");
    _Static_assert(sizeof(struct optional_scalarsQ_Scalars) <= 64, "mixed fields share presence bits");
    _Static_assert(__builtin_types_compatible_p(__typeof__(((optional_scalarsQ_Derived)0)->item), $WORD), "generic parent keeps its ABI");
    return true;
}
