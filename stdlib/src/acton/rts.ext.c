#include <uv.h>
#include <unistd.h>

void acton$rts$D___ext_init__() {
    // NOP
}

$NoneType acton$rts$$sleep (B_float sleep_time) {
    int to_sleep = sleep_time->val*1000000;
    usleep(to_sleep);
    return $None;
}

B_int acton$rts$$rss ($WorldAuth auth) {
    size_t rsm;
    int r = uv_resident_set_memory(&rsm);
    return toB_int(rsm);
}
