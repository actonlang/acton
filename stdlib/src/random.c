#include "random.h"
#include <stdlib.h>

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

$int random$$randint($int min, $int max) {
    // ensure we have a valid range where min is smaller than max
    if (min->val > max->val) {
        $RAISE((($BaseException)$ValueError$new(to$str("min value must be smaller than max"))));
    }
    // upper end of the range we want when "based to 0"
    int range = max->val - min->val;
    // chop off all values that would cause skew, leaving only things within the
    // range that maps up to a multiple of the specified range
    int end = RAND_MAX / range;
    // new end
    end *= range;
    // run rand() until we get a value below end
    int r;
    // spin getting new values until we find one in range
    while ((r = rand()) >= end)
        ;
    // normalize back to the requested range
    return to$int(min->val + r % range);
}

int random$$done$ = 0;
int random$$seeded = 0;
void random$$__init__() {
    // default to just seeding
    srand(time(NULL));
    if (random$$done$)
        return;
    random$$done$ = 1;
}
