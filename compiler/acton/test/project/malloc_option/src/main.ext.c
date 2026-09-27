#include <gc.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// The mimalloc object overrides this fallback when linked. A definition also
// works with linkers that reject unresolved weak imports on macOS.
__attribute__((weak, noinline)) bool mi_is_in_heap_region(const void *ptr) {
    (void)ptr;
    return false;
}
extern void *malloc_option_shared_alloc(size_t size);
extern void malloc_option_shared_free(void *ptr);

void malloc_optionQ_mainQ___ext_init__() {}

static void require(int condition, const char *message) {
    if (!condition) {
        fprintf(stderr, "malloc option test: %s\n", message);
        exit(1);
    }
}

static void check_owner(void *ptr) {
    const char *expected = getenv("ACTON_TEST_MIMALLOC");
    require(ptr != NULL, "allocation failed");
    bool mimalloc = mi_is_in_heap_region(ptr);
    require(mimalloc == (expected != NULL && strcmp(expected, "1") == 0),
            "allocation did not use the selected heap");
    require(GC_base(ptr) == NULL, "ordinary malloc unexpectedly used the GC heap");
}

int64_t malloc_optionQ_mainQ_check() {
    unsigned char *ptr = malloc(128);
    check_owner(ptr);
    memset(ptr, 42, 128);
    ptr = realloc(ptr, 4096);
    check_owner(ptr);
    for (int i = 0; i < 128; i++) require(ptr[i] == 42, "realloc lost data");
    // Allocate in the executable and free in a separately linked library.
    malloc_option_shared_free(ptr);

    ptr = calloc(128, 1);
    check_owner(ptr);
    for (int i = 0; i < 128; i++) require(ptr[i] == 0, "calloc did not zero memory");
    free(ptr);

    char *copy = strdup("allocator");
    check_owner(copy);
    require(strcmp(copy, "allocator") == 0, "strdup lost data");
    free(copy);

    void *aligned = NULL;
    require(posix_memalign(&aligned, 64, 256) == 0, "aligned allocation failed");
    check_owner(aligned);
    require((uintptr_t)aligned % 64 == 0, "allocation was not aligned");
    free(aligned);

    // Allocate in the shared library and free in the executable.
    ptr = malloc_option_shared_alloc(512);
    check_owner(ptr);
    free(ptr);

    void *managed = acton_malloc(128);
    require(GC_base(managed) == managed, "Acton objects must remain GC-managed");
    require(!mi_is_in_heap_region(managed),
            "Acton allocation moved to mimalloc");
    acton_free(managed);
    return 0;
}
