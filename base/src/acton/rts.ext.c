#include <time.h>
#include <uv.h>
#include "gc.h"

void actonQ_rtsQ___ext_init__() {
    // NOP
}

B_NoneType actonQ_rtsQ_gc () {
    GC_gcollect();
    return B_None;
}

B_NoneType actonQ_rtsQ_sleep (B_float sleep_time) {
    double st = fromB_float(sleep_time);
    struct timespec ts;
    ts.tv_sec = (int)st;
    ts.tv_nsec = (st - (float)ts.tv_sec)*1e9;

    // Handle overflow of nanoseconds into seconds
    if (ts.tv_nsec >= 1e9) {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1e9;
    }

    // Unlike usleep, nanosleep() tolerates signal interrupts and will write the
    // remaining time (that it didn't sleep) into the third argument. We spin
    // until it completes successfully.
    while (1) {
        int r = nanosleep(&ts, &ts);
        if (r == 0)
            break;
    }
    return B_None;
}

B_int actonQ_rtsQ_rss (B_WorldAuth auth) {
    size_t rsm;
    int r = uv_resident_set_memory(&rsm);
    return to$int(rsm);
}
