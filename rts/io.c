#include "io.h"

#include <uv.h>

#include "log.h"

extern char rts_exit;
uv_async_t stop_event;

bool ioloop_started = false;

void stop_ioloop() {
    // We are not allowed to call uv_async_send before the ioloop has started.
    // We set ioloop_stop_request so that if the loop has not yet started, it
    // will see this value once it has started up and immediately exit.
    if (ioloop_started)
        uv_async_send(&stop_event);
}
void ioloop_init_cb(uv_timer_t *timer) {
    // startup takes time and if we are asked to stop before we have started
    // (and this function is called) we will then stop immediately
    ioloop_started = true;
    uv_timer_stop(timer);
    if (rts_exit == 1) {
        log_debug("RTS exiting before ioloop started, stopping ioloop now...");
        uv_stop(uv_default_loop());
    }
}

void stop_cb(uv_async_t *ev) {
    uv_stop(uv_default_loop());
}

int ioloop(void *arg) {
    log_info("Starting ioloop");

#if defined(IS_MACOS)
    pthread_setname_np("IO");
#else
    pthread_setname_np(pthread_self(), "IO");
#endif

    uv_timer_t init_timer;
    uv_timer_init(uv_default_loop(), &init_timer);
    uv_timer_start(&init_timer, ioloop_init_cb, 0, 0);

    uv_async_init(uv_default_loop(), &stop_event, stop_cb);

    log_debug("starting uv_run");
    int r = uv_run(uv_default_loop(), UV_RUN_DEFAULT);
    log_debug("uv_run has stopped");
    return r;
}
