// Intentionally buggy C implementations used to stress-test Acton FFI calls.
// These are expected to be flaky under concurrent execution.

#include <stdio.h>
#include <time.h>

static int64_t shared_accumulator = 0;
static char shared_fmt_buf[128];
static int lazy_reset_counter = 0;

static struct {
    int ready;
    int64_t hi;
    int64_t lo;
} lazy_magic_state = {0, 0, 0};

static struct {
    int valid;
    int64_t key;
    int64_t value;
} subtle_cache_state = {0, 0, 0};

static inline void tiny_pause(int loops) {
    for (volatile int i = 0; i < loops; i++) {
    }
}

static inline int64_t mix_formula(int64_t n) {
    return (n * 1103515245LL + 12345LL) & 0x7fffffffLL;
}

void racy_ffiQ___ext_init__() {
    shared_accumulator = 0;
    shared_fmt_buf[0] = '\0';
    lazy_reset_counter = 0;
    lazy_magic_state.ready = 0;
    lazy_magic_state.hi = 0;
    lazy_magic_state.lo = 0;
    subtle_cache_state.valid = 0;
    subtle_cache_state.key = 0;
    subtle_cache_state.value = 0;
}

// Intentionally racy: shared mutable state in a function that should behave as
// a pure computation.
int64_t racy_ffiQ_U_racy_sum(int64_t U_1n) {
    shared_accumulator = 0;
    for (int64_t i = 1; i <= U_1n; i++) {
        shared_accumulator += i;
        if ((i & 15) == 0) {
            struct timespec ts = {0, 50000};  // 50 us
            nanosleep(&ts, NULL);
        }
    }
    return shared_accumulator;
}

// Intentionally racy: shared global formatting buffer used across threads.
B_str racy_ffiQ_U_2racy_format(int64_t U_3n) {
    int64_t sq = U_3n * U_3n;
    snprintf(shared_fmt_buf, sizeof(shared_fmt_buf), "value:%lld:%lld",
             (long long)U_3n, (long long)sq);

    // Increase the race window so another worker can overwrite shared_fmt_buf.
    struct timespec ts = {0, 200000};  // 200 us
    nanosleep(&ts, NULL);

    return to$str(shared_fmt_buf);
}

// Subtle publication race:
// We deliberately set "ready" before fully initializing the payload.
// Under concurrency, another worker can observe ready=1 and read partial state.
int64_t racy_ffiQ_U_4subtle_lazy_magic() {
    int rc = lazy_reset_counter++;
    if ((rc & 1023) == 0) {
        lazy_magic_state.ready = 0;
        lazy_magic_state.hi = 0;
        lazy_magic_state.lo = 0;
    }

    if (!lazy_magic_state.ready) {
        lazy_magic_state.ready = 1;  // intentionally published too early
        tiny_pause(1600);
        lazy_magic_state.hi = 0x5A00;
        tiny_pause(2200);
        lazy_magic_state.lo = 0x5A;
    }

    tiny_pause(500);
    return lazy_magic_state.hi + lazy_magic_state.lo;
}

// Subtle torn cache race:
// key/valid/value are updated in a non-atomic order. Under stress, readers can
// observe key from one write and value from another.
int64_t racy_ffiQ_U_5subtle_cache_mix(int64_t U_6n) {
    if (subtle_cache_state.valid && subtle_cache_state.key == U_6n) {
        tiny_pause(300 + (int)(U_6n & 31) * 16);
        return subtle_cache_state.value;
    }

    int64_t val = mix_formula(U_6n);
    subtle_cache_state.valid = 1;  // intentionally published early
    subtle_cache_state.key = U_6n;
    tiny_pause(400 + (int)(U_6n & 31) * 18);
    subtle_cache_state.value = val;
    tiny_pause(300 + (int)((U_6n + 7) & 31) * 12);
    return val;
}
