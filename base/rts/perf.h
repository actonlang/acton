#pragma once

#include <stdbool.h>
#include <stdint.h>

struct rts_perf_sample {
    bool cpu_available;
    bool hardware_available;
    uint64_t user_ns, system_ns;
    uint64_t instructions, cycles;
    uint64_t instructions_enabled, instructions_running;
    uint64_t cycles_enabled, cycles_running;
};

void rts_perf_init(void);
void rts_perf_read(struct rts_perf_sample *sample);
const char *rts_perf_backend(void);
const char *rts_perf_scope(void);
const char *rts_perf_status(void);
