#include <uv.h>
#include <unistd.h>

void acton$rts$$__ext_init__() {
    // NOP
}

$NoneType acton$rts$$sleep ($float sleep_time) {
    int to_sleep = sleep_time->val*1000000;
    usleep(to_sleep);
    return $None;
}

$int acton$rts$$rss ($WorldAuth auth) {
    size_t rsm;
    int r = uv_resident_set_memory(&rsm);
    return to$int(rsm);
}
