#include "time.h"

$float time$$monotonic() {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$float(ts.tv_sec + ts.tv_nsec);
}

$int time$$monotonic_ns() {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}

$float time$$time() {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$float(ts.tv_sec + 0.000000001 * ts.tv_nsec);
}

$int time$$time_ns() {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}

int time$$done$ = 0;
void time$$__init__() {
    if (time$$done$)
        return;
    time$$done$ = 1;
}
