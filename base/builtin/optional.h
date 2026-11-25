#pragma once

// Tagged locals and raw constructor arguments occupy two initialized words.
// Small payloads are widened here to avoid unspecified padding in GC-scanned
// locals. Object fields use their natural widths and share presence words.
#define B_OPTIONAL_SCALAR(NAME, TYPE, CTYPE, STORAGE) \
    typedef struct { uint64_t present; STORAGE value; } TYPE; \
    static inline TYPE B_optional_##NAME##_none(void) { \
        return (TYPE){ .present = 0, .value = 0 }; \
    } \
    static inline TYPE B_optional_##NAME##_some(CTYPE value) { \
        return (TYPE){ .present = 1, .value = value }; \
    } \
    static inline TYPE B_optional_##NAME##_from_box($WORD value) { \
        return value ? B_optional_##NAME##_some(((B_##NAME)value)->val) \
                     : B_optional_##NAME##_none(); \
    } \
    static inline $WORD B_optional_##NAME##_box(TYPE value) { \
        return value.present ? ($WORD)toB_##NAME((CTYPE)value.value) : NULL; \
    } \
    static inline TYPE B_optional_##NAME##_identity(TYPE value) { return value; } \
    static inline void B_optional_##NAME##_store_inline(void *owner, void *slot, TYPE value) { \
        memcpy(slot, &value, sizeof value); \
    } \
    static inline void B_optional_##NAME##_store_boxed(void *owner, void *slot, TYPE value) { \
        $WORD boxed = B_optional_##NAME##_box(value); \
        memcpy(slot, &boxed, sizeof boxed); \
        ACTON_WBAR(owner, slot, boxed); \
    } \
    static inline TYPE B_optional_##NAME##_load_packed(const CTYPE *slot, \
                                                       const uint64_t *presence, uint64_t bit) { \
        return (*presence & bit) ? B_optional_##NAME##_some(*slot) : B_optional_##NAME##_none(); \
    } \
    static inline void B_optional_##NAME##_store_packed(CTYPE *slot, uint64_t *presence, \
                                                        uint64_t bit, TYPE value) { \
        *slot = value.present ? (CTYPE)value.value : (CTYPE)0; \
        *presence = (*presence & ~bit) | (value.present ? bit : 0); \
    }

B_OPTIONAL_SCALAR(int, B_OptionalInt, int64_t, int64_t)
B_OPTIONAL_SCALAR(float, B_OptionalFloat, double, double)
B_OPTIONAL_SCALAR(bool, B_OptionalBool, bool, uint64_t)
B_OPTIONAL_SCALAR(i32, B_OptionalI32, int32_t, int64_t)
B_OPTIONAL_SCALAR(i16, B_OptionalI16, int16_t, int64_t)
B_OPTIONAL_SCALAR(i8, B_OptionalI8, int8_t, int64_t)
B_OPTIONAL_SCALAR(u64, B_OptionalU64, uint64_t, uint64_t)
B_OPTIONAL_SCALAR(u32, B_OptionalU32, uint32_t, uint64_t)
B_OPTIONAL_SCALAR(u16, B_OptionalU16, uint16_t, uint64_t)
B_OPTIONAL_SCALAR(u8, B_OptionalU8, uint8_t, uint64_t)
B_OPTIONAL_SCALAR(u1, B_OptionalU1, uint8_t, uint64_t)
#undef B_OPTIONAL_SCALAR

// Imported and generic fields may still hold boxes. Each load's expected
// scalar type chooses its unboxer; the actual C field type selects identity
// for an already-tagged value. The argument is evaluated once.
#define B_OPTIONAL_LOAD(NAME, TYPE, VALUE) \
    _Generic((VALUE), TYPE: B_optional_##NAME##_identity, \
             default: B_optional_##NAME##_from_box)((VALUE))

#define B_optional_int_load($value) B_OPTIONAL_LOAD(int, B_OptionalInt, ($value))
#define B_optional_float_load($value) B_OPTIONAL_LOAD(float, B_OptionalFloat, ($value))
#define B_optional_bool_load($value) B_OPTIONAL_LOAD(bool, B_OptionalBool, ($value))
#define B_optional_i32_load($value) B_OPTIONAL_LOAD(i32, B_OptionalI32, ($value))
#define B_optional_i16_load($value) B_OPTIONAL_LOAD(i16, B_OptionalI16, ($value))
#define B_optional_i8_load($value) B_OPTIONAL_LOAD(i8, B_OptionalI8, ($value))
#define B_optional_u64_load($value) B_OPTIONAL_LOAD(u64, B_OptionalU64, ($value))
#define B_optional_u32_load($value) B_OPTIONAL_LOAD(u32, B_OptionalU32, ($value))
#define B_optional_u16_load($value) B_OPTIONAL_LOAD(u16, B_OptionalU16, ($value))
#define B_optional_u8_load($value) B_OPTIONAL_LOAD(u8, B_OptionalU8, ($value))
#define B_optional_u1_load($value) B_OPTIONAL_LOAD(u1, B_OptionalU1, ($value))

static inline $WORD B_optional_box_identity($WORD value) { return value; }

#define B_optional_box_field($value) \
    _Generic(($value), \
        B_OptionalInt: B_optional_int_box, \
        B_OptionalFloat: B_optional_float_box, \
        B_OptionalBool: B_optional_bool_box, \
        B_OptionalI32: B_optional_i32_box, \
        B_OptionalI16: B_optional_i16_box, \
        B_OptionalI8: B_optional_i8_box, \
        B_OptionalU64: B_optional_u64_box, \
        B_OptionalU32: B_optional_u32_box, \
        B_OptionalU16: B_optional_u16_box, \
        B_OptionalU8: B_optional_u8_box, \
        B_OptionalU1: B_optional_u1_box, \
        default: B_optional_box_identity)(($value))

// The incoming tagged value supplies the scalar type, even for a field A in
// Generic[A]. Boxed fields retain their write barrier; tagged fields contain
// no pointers. RHS and receiver are each evaluated once, in that order.
#define B_OPTIONAL_STORE_CASE(NAME, TYPE, FIELD) \
    TYPE: _Generic((FIELD), TYPE: B_optional_##NAME##_store_inline, \
                   default: B_optional_##NAME##_store_boxed)

#define B_optional_store_field($receiver, $field, $value) do { \
    __auto_type $optional_value = ($value); \
    __auto_type $optional_receiver = ($receiver); \
    _Generic(($optional_value), \
        B_OPTIONAL_STORE_CASE(int, B_OptionalInt, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(float, B_OptionalFloat, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(bool, B_OptionalBool, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(i32, B_OptionalI32, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(i16, B_OptionalI16, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(i8, B_OptionalI8, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(u64, B_OptionalU64, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(u32, B_OptionalU32, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(u16, B_OptionalU16, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(u8, B_OptionalU8, $optional_receiver->$field), \
        B_OPTIONAL_STORE_CASE(u1, B_OptionalU1, $optional_receiver->$field) \
    )($optional_receiver, &$optional_receiver->$field, $optional_value); \
} while (0)
