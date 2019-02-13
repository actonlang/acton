#define __LCHT_KEY_TYPE int
#define __LCHT_MAPPED_TYPE void*

#define CUCKOO_TABLE_NAME __LCHT_TABLE_NAME
#define CUCKOO_KEY_TYPE __LCHT_KEY_TYPE
#define CUCKOO_MAPPED_TYPE __LCHT_MAPPED_TYPE

#include "deps/libcuckoo/libcuckoo-c/cuckoo_table_template.h"

#ifndef CUCKOO_TABLE_IMPL
    #define HASHTABLE_INT_TEMPLATE(name, value_type) \
        typedef __LCHT_TABLE_NAME name; \
        extern inline name* name##_init(size_t t) { return (name*)__LCHT_TABLE_NAME##_init(t); } \
        extern inline void name##_free(name* table) { __LCHT_TABLE_NAME##_free((__LCHT_TABLE_NAME*)table); } \
        extern inline bool name##_empty(name* table) { return __LCHT_TABLE_NAME##_empty((__LCHT_TABLE_NAME*)table); } \
        extern inline size_t name##_size(name* table) { return __LCHT_TABLE_NAME##_size((__LCHT_TABLE_NAME*)table); } \
        extern inline bool name##_find(name* table, __LCHT_KEY_TYPE* key, value_type* value) { return __LCHT_TABLE_NAME##_find((__LCHT_TABLE_NAME*)table, key, (__LCHT_MAPPED_TYPE*)value); } \
        extern inline bool name##_contains(name* table, __LCHT_KEY_TYPE* key) { return __LCHT_TABLE_NAME##_contains((__LCHT_TABLE_NAME*)table, key); } \
        extern inline bool name##_update(name* table, __LCHT_KEY_TYPE* key, value_type* value) { return __LCHT_TABLE_NAME##_update((__LCHT_TABLE_NAME*)table, key, (__LCHT_MAPPED_TYPE*)value); } \
        extern inline bool name##_insert(name* table, __LCHT_KEY_TYPE* key, value_type* value) { return __LCHT_TABLE_NAME##_insert((__LCHT_TABLE_NAME*)table, key, (__LCHT_MAPPED_TYPE*)value); } \
        extern inline bool name##_insert_or_assign(name* table, __LCHT_KEY_TYPE* key, value_type* value) { return __LCHT_TABLE_NAME##_insert_or_assign((__LCHT_TABLE_NAME*)table, key, (__LCHT_MAPPED_TYPE*)value); } \
        extern inline bool name##_erase(name* table, __LCHT_KEY_TYPE* key) { return __LCHT_TABLE_NAME##_erase((__LCHT_TABLE_NAME*)table, key); } \
        extern inline void name##_clear(name* table) { __LCHT_TABLE_NAME##_clear((__LCHT_TABLE_NAME*)table); }

    #undef CUCKOO_TABLE_NAME
    #undef CUCKOO_KEY_TYPE
    #undef CUCKOO_MAPPED_TYPE
#endif
