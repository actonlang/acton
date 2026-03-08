// Intentionally crashy FFI helper for stress-testing.
// Passes in normal single-worker mode, segfaults when concurrent calls overlap.

#include <signal.h>

static int active_calls = 0;

static inline void hold_window() {
    for (volatile int i = 0; i < 300000; i++) {
    }
}

void segv_ffiQ___ext_init__() {
    active_calls = 0;
}

int64_t segv_ffiQ_U_maybe_crash_if_parallel(int64_t U_1n) {
    __sync_add_and_fetch(&active_calls, 1);
    hold_window();
    if (__sync_fetch_and_add(&active_calls, 0) > 1) {
        signal(SIGSEGV, SIG_DFL);
        raise(SIGSEGV);
    }
    hold_window();
    __sync_sub_and_fetch(&active_calls, 1);
    return U_1n + 1;
}
