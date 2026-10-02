#include <gc.h>
#include <stdbool.h>
#include <stdint.h>

void gc_optionsQ_mainQ___ext_init__() {}

// With end padding, a 16-byte object gets a 32-byte slot.
int64_t gc_optionsQ_mainQ_small_object_size() {
    return (int64_t)GC_size(GC_malloc(16));
}

// Move a 16-byte object eight times by growing it to 256 bytes, and tell
// whether the collector's count of explicitly freed bytes grew each time.
// Collections are disabled, as they reset the count. When GC_realloc does
// not free, a GC_free by another thread could still raise the count during
// one of the moves, but not during all eight.
bool gc_optionsQ_mainQ_realloc_frees_moved() {
    int freed = 0;
    GC_disable();
    for (int i = 0; i < 8; i++) {
        void *p = GC_malloc(16);
        size_t before = GC_get_expl_freed_bytes_since_gc();
        void *q = GC_realloc(p, 256);
        if (q != p && GC_get_expl_freed_bytes_since_gc() > before)
            freed++;
    }
    GC_enable();
    return freed == 8;
}
