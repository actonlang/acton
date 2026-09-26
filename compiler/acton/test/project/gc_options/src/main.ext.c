#include <gc.h>
#include <stdint.h>

void gc_optionsQ_mainQ___ext_init__() {}

// With end padding, a 16-byte object gets a 32-byte slot.
int64_t gc_optionsQ_mainQ_small_object_size() {
    return (int64_t)GC_size(GC_malloc(16));
}
