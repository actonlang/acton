#include <acton_gc_config.h>
#include <gc.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

int64_t gc_tuningQ_mainQ_check(int64_t backend, bool incremental) {
    const char *selected = backend == GC_VDB_SOFT ? "soft_dirty"
                           : backend == GC_VDB_UFFDWP ? "userfaultfd"
                                                    : "auto";
    gc_tuning_require(strcmp(ACTON_GC_DIRTY_TRACKING_BACKEND, selected) == 0,
                      "collector backend metadata does not match the root");
    unsigned supported = GC_get_supported_vdbs();
    if (backend == GC_VDB_SOFT) {
        gc_tuning_require((supported & GC_VDB_SOFT) != 0,
                          "soft-dirty support is missing");
        gc_tuning_require((supported & GC_VDB_UFFDWP) == 0,
                          "soft-dirty selection retained userfaultfd");
    } else if (backend == GC_VDB_UFFDWP) {
        gc_tuning_require((supported & GC_VDB_UFFDWP) != 0,
                          "userfaultfd support is missing");
        gc_tuning_require((supported & GC_VDB_SOFT) == 0,
                          "userfaultfd selection retained soft-dirty");
    }
    if (backend != 0)
        gc_tuning_require((supported & GC_VDB_MPROTECT) == 0,
                          "explicit backend selection permits mprotect fallback");
    unsigned actual = GC_VDB_NONE;
    GC_call_with_alloc_lock(gc_tuning_active_backend, &actual);
    gc_tuning_require(actual == (incremental ? (unsigned)backend : GC_VDB_NONE),
                      "unexpected active dirty-page backend");
    GC_gcollect();
    GC_gcollect();
    return 0;
}
