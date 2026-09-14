#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <assert.h>

#include "perf_memory.c"

static uint64_t observe(int pid) {
    uint64_t total, available, process;
    char error[512];
    if (acton_perf_memory(pid, &total, &available, &process, error, sizeof(error))) {
        fprintf(stderr, "%s\n", error);
        exit(1);
    }
    printf("pid=%d total=%" PRIu64 " available=%" PRIu64 " process=%" PRIu64 "\n",
           pid, total, available, process);
    if (!total || available > total || (pid == 0 && process != 0) || (pid != 0 && process == 0)) exit(2);
    return process;
}

int main(void) {
#if defined(__APPLE__) && defined(__MACH__)
    /* A busy 48 GiB host can have little free RAM but a large reusable cache.
     * Anonymous pages outnumber inactive pages, which defeated the old estimate. */
    const uint64_t gib = UINT64_C(1024) * 1024 * 1024, page_size = 16384;
    vm_statistics64_data_t vm = {
        .free_count = 2 * gib / page_size,
        .speculative_count = gib / page_size,
        .external_page_count = 15 * gib / page_size,
        .purgeable_count = 3 * gib / page_size,
        .inactive_count = 17 * gib / page_size,
        .internal_page_count = 21 * gib / page_size,
        .compressor_page_count = 5 * gib / page_size
    };
    assert(available_memory(&vm, 48 * gib, page_size) == 19 * gib);
    vm.external_page_count = vm.purgeable_count = vm.speculative_count = 0;
    assert(available_memory(&vm, 48 * gib, page_size) == 2 * gib);
    /* Counter snapshots need not describe the exact same instant. */
    vm.speculative_count = 3 * gib / page_size;
    assert(available_memory(&vm, 48 * gib, page_size) == 0);
    vm.external_page_count = 50 * gib / page_size;
    assert(available_memory(&vm, 48 * gib, page_size) == 48 * gib);
#endif
    observe(0);
    uint64_t before = observe((int)getpid());
    const size_t size = 32 * 1024 * 1024;
    volatile char *memory = malloc(size);
    if (!memory) return 3;
    for (size_t i = 0; i < size; i += 4096) memory[i] = 1;
    uint64_t after = observe((int)getpid());
    if (after < before + size / 2) return 4;
    free((void *)memory);
    uint64_t total, available, process;
    char error[512];
    if (!acton_perf_memory(2147483647, &total, &available, &process, error, sizeof(error))) return 5;
    printf("missing PID rejected: %s\n", error);
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    for (int i = 0; i < 1000; i++) {
        if (acton_perf_memory((int)getpid(), &total, &available, &process, error, sizeof(error))) {
            fprintf(stderr, "%s\n", error);
            return 6;
        }
    }
    clock_gettime(CLOCK_MONOTONIC, &end);
    double seconds = end.tv_sec - start.tv_sec + (end.tv_nsec - start.tv_nsec) / 1e9;
    printf("sampling cost: %.1f microseconds/call across 1000 calls\n", seconds * 1000.0);
    puts("native memory probe passed");
    return 0;
}
