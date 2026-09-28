#include <stdlib.h>
#include <stdint.h>

void malloc_optionQ_sharedQ___ext_init__() {}

int64_t malloc_optionQ_sharedQ_marker() {
    return 1;
}

void *malloc_option_shared_alloc(size_t size) {
    return malloc(size);
}

void malloc_option_shared_free(void *ptr) {
    free(ptr);
}
