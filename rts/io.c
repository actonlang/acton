#include "io.h"

#include <uv.h>
#include <unistd.h>

#include "log.h"
#include "log.h"

extern char rts_exit;

uv_loop_t *get_uv_loop() {
    return (uv_loop_t *)pthread_getspecific(pkey_uv_loop);
}
