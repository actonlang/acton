#include <acton_gc_config.h>
#include <gc.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void gc_tuningQ_mainQ___ext_init__() {}

static void gc_tuning_require(bool condition, const char *message) {
    if (!condition) {
        fprintf(stderr, "GC tuning test: %s\n", message);
        exit(1);
    }
}

static void *gc_tuning_active_backend(void *result) {
    *(unsigned *)result = GC_get_actual_vdb();
    return NULL;
}

int64_t gc_tuningQ_mainQ_check(int64_t page_hash_log2, bool generational) {
    unsigned default_log2 = sizeof(void *) == 8 ? 21 : 20;
    gc_tuning_require(acton_gc_get_page_hash_table_log2()
                          == (page_hash_log2 ? page_hash_log2 : default_log2),
                      "collector page-hash size does not match the root");
    unsigned supported = GC_get_supported_vdbs();
    gc_tuning_require((supported & GC_VDB_MANUAL) != 0,
                      "manual dirty-bit support is missing");
    gc_tuning_require((supported & (GC_VDB_MPROTECT | GC_VDB_SOFT | GC_VDB_UFFDWP)) == 0,
                      "the collector is built with page protection");
    unsigned actual = GC_VDB_NONE;
    GC_call_with_alloc_lock(gc_tuning_active_backend, &actual);
    gc_tuning_require(actual == (generational ? GC_VDB_MANUAL : GC_VDB_NONE),
                      "unexpected active dirty-bit backend");
    GC_gcollect();
    GC_gcollect();
    return 0;
}
