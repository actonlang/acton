#include <stddef.h>
#include <string.h>

void scalar_hash_paddingQ___ext_init__() {
}

$WORD scalar_hash_paddingQ_with_padding($WORD value, uint8_t padding) {
    // Keep the class and value, but give the trailing padding known bytes.
#define COPY_PADDED(T) \
    if ($ISINSTANCE0(value, T)) { \
        T copy = acton_malloc(sizeof(struct T)); \
        memcpy(copy, value, sizeof(struct T)); \
        size_t end = offsetof(struct T, val) + sizeof(copy->val); \
        memset((char *)copy + end, padding, sizeof(struct T) - end); \
        return ($WORD)copy; \
    }
    COPY_PADDED(B_bool)
    COPY_PADDED(B_u1)
    COPY_PADDED(B_u8)
    COPY_PADDED(B_u16)
    COPY_PADDED(B_u32)
#undef COPY_PADDED
    $RAISE((B_BaseException)$NEW(B_ValueError, to$str("Unexpected scalar type")));
    return NULL;
}
