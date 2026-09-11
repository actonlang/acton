#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

extern int acton_perf_memory(int, uint64_t *, uint64_t *, uint64_t *, char *, size_t);

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
