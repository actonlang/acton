#include "src/acton/rts.h"
#include <unistd.h>
$NoneType acton$rts$$sleep ($float sleep_time) {
    int to_sleep = sleep_time->val*1000000;
    usleep(to_sleep);
    return $None;
}
int acton$rts$$done$ = 0;
void acton$rts$$__init__ () {
    if (acton$rts$$done$) return;
    acton$rts$$done$ = 1;
}
