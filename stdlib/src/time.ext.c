void timeQ___ext_init__() {
    // NOP
}

B_float timeQ_monotonic () {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == -1) {
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("Unable to get time"))));
    }
    return to$float(ts.tv_sec + 0.000000001*ts.tv_nsec);
}

B_int timeQ_monotonic_ns () {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == -1) {
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("Unable to get time"))));
    }
    return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}

B_float timeQ_time () {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("Unable to get time"))));
    }
    return to$float(ts.tv_sec + 0.000000001*ts.tv_nsec);
}

B_int timeQ_time_ns () {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("Unable to get time"))));
    }
    return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}
