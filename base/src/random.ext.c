#include <stdlib.h>

void randomQ___ext_init__() {
    // seed the random number generator with nanoseconds since the epoch and our
    // PID
    uv_timespec64_t ts;
    uv_clock_gettime(UV_CLOCK_MONOTONIC, &ts);
    srand(ts.tv_nsec ^ pid);
}

// NOTE: the standard srand / rand functions are not thread safe, but what does
// that really mean in this context? While we could use thread safe functions
// for deterministic random numbers (based on the seed), we won't guarantee
// deterministic scheduling of actors on our RTS threads and so there still
// would not be a guarantee for thread safe random numbers. Also, having to call
// srand to seed reflects the C implementation, which we're not keen on doing -
// rather we want it Pythonic - you should be able to get a random number with a
// single call to random.randint(1, 3). So instead we just seed on startup with
// the time, which is prolly good enough for now. In a future, we could cook up
// something better.

long randlong (long min, long max) {
    // ensure we have a valid range where min is smaller than max
    if (min > max) {
        $RAISE(((B_BaseException)B_ValueErrorG_new(to$str("min value must be smaller than max"))));
    }
    // upper end of the range we want when "based to 0"
    long range = max - min;
    // chop off all values that would cause skew, leaving only things within the
    // range that maps up to a multiple of the specified range
    long end = RAND_MAX / range;
    // new end
    end *= range;
    // run rand() until we get a value below end
    long r;
    // spin getting new values until we find one in range
    while ((r = rand()) >= end);
    // normalize back to the requested range
    return min + r%range;
}

B_int randomQ_randint (B_int min, B_int max) {
    return to$int(randlong(from$int(min),from$int(max)));
}

B_i64 randomQ_randi64 (B_i64 min, B_i64 max) {
    return toB_i64(randlong(fromB_i64(min),fromB_i64(max)));
}
