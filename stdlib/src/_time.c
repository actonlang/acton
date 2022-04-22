#include "_time.h"

$float _time$$monotonic () {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$float(ts.tv_sec + ts.tv_nsec);
}

$int _time$$monotonic_ns () {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}

$float _time$$time () {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$float(ts.tv_sec + 0.000000001*ts.tv_nsec);
}

$int _time$$time_ns () {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to get time"))));
    }
    return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}


int _time$$done$ = 0;
void _time$$__init__ () {
    if (_time$$done$) return;
    _time$$done$ = 1;
}
