#include "time/time.h"
$int time$$time () {
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        perror("clock_gettime");
        // TODO: this feels superbad, what should we do? how do we gently
        // recover in Acton-C? throw exception?
        exit(EXIT_FAILURE);
    }
    return to$int(ts.tv_sec * 1000000000 + ts.tv_nsec);
}
int time$$done$ = 0;
void time$$__init__ () {
    if (time$$done$) return;
    time$$done$ = 1;
}
