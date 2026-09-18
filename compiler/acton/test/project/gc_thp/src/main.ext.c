#include <gc.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

void gc_thpQ_mainQ___ext_init__() {}

static void thp_require(int condition, const char *message) {
    if (!condition) {
        fprintf(stderr, "GC THP test: %s\n", message);
        exit(1);
    }
}

static int has_nohugepage(uintptr_t address) {
    FILE *smaps = fopen("/proc/self/smaps", "r");
    thp_require(smaps != NULL, "cannot read /proc/self/smaps");
    char line[1024];
    int selected = 0;
    while (fgets(line, sizeof(line), smaps)) {
        unsigned long start, end;
        if (sscanf(line, "%lx-%lx", &start, &end) == 2) {
            selected = address >= start && address < end;
        } else if (selected && strncmp(line, "VmFlags:", 8) == 0) {
            int nohugepage = 0;
            for (char *flag = strtok(line + 8, " \n"); flag; flag = strtok(NULL, " \n")) {
                if (strcmp(flag, "nh") == 0)
                    nohugepage = 1;
            }
            fclose(smaps);
            return nohugepage;
        }
    }
    fclose(smaps);
    thp_require(0, "allocation was not found in smaps");
    return 0;
}

static void check_range(void *space, size_t size, int enabled) {
    thp_require(has_nohugepage((uintptr_t)space) == enabled,
                "unexpected THP policy at start of GC allocation");
    thp_require(has_nohugepage((uintptr_t)space + size - 1) == enabled,
                "unexpected THP policy at end of GC allocation");
}

int64_t gc_thpQ_mainQ_check(bool enabled) {
    GC_on_os_get_mem_proc hook = GC_get_on_os_get_mem();
    thp_require((hook != NULL) == enabled, "unexpected startup hook");
    if (hook)
        hook(NULL, 4096); // Failed allocation notifications must be harmless.

    void *small = GC_malloc(32);
    thp_require(small != NULL, "small allocation failed");
    check_range(small, 32, enabled);

    const size_t size = 32 * 1024 * 1024;
    void *large = GC_malloc_atomic(size);
    thp_require(large != NULL, "large allocation failed");
    memset(large, 0x5a, size);
    check_range(large, size, enabled);
    const uintptr_t previous = (uintptr_t)large;
    GC_free(large);
    large = NULL;
    GC_gcollect_and_unmap();
    GC_word heap_before, unmapped_before;
    GC_get_heap_usage_safe(&heap_before, NULL, &unmapped_before, NULL, NULL);
    thp_require(unmapped_before >= size, "collector did not release physical pages");
    check_range((void *)previous, size, enabled);

    large = GC_malloc_atomic(size);
    thp_require(large != NULL, "allocation after release failed");
    memset(large, 0xa5, size);
    GC_word heap_after, unmapped_after;
    GC_get_heap_usage_safe(&heap_after, NULL, &unmapped_after, NULL, NULL);
    thp_require(heap_after + unmapped_after == heap_before + unmapped_before,
                "allocation grew the GC heap instead of reusing it");
    thp_require(unmapped_after < unmapped_before,
                "allocation did not reuse released physical pages");
    check_range(large, size, enabled);
    GC_free(large);
    GC_free(small);

    void *ordinary = mmap(NULL, size, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    thp_require(ordinary != MAP_FAILED, "ordinary mmap failed");
    thp_require(!has_nohugepage((uintptr_t)ordinary), "GC setting affected ordinary mmap");
    thp_require(munmap(ordinary, size) == 0, "ordinary munmap failed");
    return 0;
}
