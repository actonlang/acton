#include <gc.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

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

static void check_released(void *space, size_t size) {
    const size_t page_size = (size_t)getpagesize();
    const uintptr_t start = ((uintptr_t)space + page_size - 1) / page_size * page_size;
    const uintptr_t end = ((uintptr_t)space + size) / page_size * page_size;
    const size_t pages = (end - start) / page_size;
    thp_require(pages > 0, "allocation contains no whole pages");
    unsigned char *resident = malloc(pages);
    thp_require(resident != NULL, "resident page allocation failed");
    thp_require(mincore((void *)start, end - start, resident) == 0, "mincore failed");
    for (size_t i = 0; i < pages; i++)
        thp_require((resident[i] & 1) == 0, "collector did not release physical pages");
    free(resident);
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
    // Limit extra space reserved for blacklisting, so reuse needs this range.
    void *large = GC_malloc_atomic_ignore_off_page(size);
    thp_require(large != NULL, "large allocation failed");
    memset(large, 0x5a, size);
    check_range(large, size, enabled);
    const uintptr_t previous = (uintptr_t)large;
    GC_free(large);
    large = NULL;
    GC_gcollect_and_unmap();
    check_released((void *)previous, size);
    check_range((void *)previous, size, enabled);

    // Leave room to split the released range, and bypass conservative
    // blacklisting of addresses retained on this test's stack.
    const size_t reuse_size = size - 1024 * 1024;
    large = GC_malloc_atomic_uncollectable(reuse_size);
    thp_require(large != NULL, "allocation after release failed");
    const uintptr_t reused = (uintptr_t)large;
    const uintptr_t overlap_start = reused > previous ? reused : previous;
    const uintptr_t reused_end = reused + reuse_size;
    const uintptr_t previous_end = previous + size;
    const uintptr_t overlap_end = reused_end < previous_end ? reused_end : previous_end;
    const size_t page_size = (size_t)getpagesize();
    const uintptr_t page_start = (overlap_start + page_size - 1) / page_size * page_size;
    const uintptr_t page_end = overlap_end / page_size * page_size;
    thp_require(page_end > page_start,
                "allocation did not reuse the released range");
    memset(large, 0xa5, reuse_size);
    check_range(large, reuse_size, enabled);
    GC_free(large);
    GC_free(small);

    void *ordinary = mmap(NULL, size, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    thp_require(ordinary != MAP_FAILED, "ordinary mmap failed");
    thp_require(!has_nohugepage((uintptr_t)ordinary), "GC setting affected ordinary mmap");
    thp_require(munmap(ordinary, size) == 0, "ordinary munmap failed");
    return 0;
}
