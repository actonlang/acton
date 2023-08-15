#define GC_THREADS 1
#include <gc.h>

#include <time.h>
#include <uv.h>

#include "rts/io.h"
#include "rts/log.h"

void actonQ_rtsQ___ext_init__() {
    // NOP
}

B_NoneType actonQ_rtsQ_gc (B_SysCap cap) {
    GC_gcollect();
    return B_None;
}

B_NoneType actonQ_rtsQ_sleep (B_SysCap cap, B_float sleep_time) {
    double st = fromB_float(sleep_time);
    struct timespec ts;
    ts.tv_sec = (int)st;
    ts.tv_nsec = (st - (float)ts.tv_sec)*1e9;

    // Handle overflow of nanoseconds into seconds
    if (ts.tv_nsec >= 1e9) {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1e9;
    }

    // Unlike usleep, nanosleep() tolerates signal interrupts and will write the
    // remaining time (that it didn't sleep) into the third argument. We spin
    // until it completes successfully.
    while (1) {
        int r = nanosleep(&ts, &ts);
        if (r == 0)
            break;
    }
    return B_None;
}

B_int actonQ_rtsQ_rss (B_SysCap cap) {
    size_t rsm;
    int r = uv_resident_set_memory(&rsm);
    return to$int(rsm);
}

struct actonQ_rtsQ_io_handles_walk_res {
    B_dict d;
    B_Hashable wit;
};

void actonQ_rtsQ_io_handles_walk_cb (uv_handle_t *handle, void *arg) {
    struct actonQ_rtsQ_io_handles_walk_res *walk_res = (struct actonQ_rtsQ_io_handles_walk_res *)arg;
    B_tuple val;
    if (uv_handle_get_type(handle) == UV_TCP) {
        $Actor hactor = handle->data;
        val = (B_tuple)$NEWTUPLE(2,
                                to$str((char *)uv_handle_type_name(uv_handle_get_type(handle))),
                                toB_u64((unsigned long)hactor)
                                );
    } else {
        val = (B_tuple)$NEWTUPLE(2,
                                to$str((char *)uv_handle_type_name(uv_handle_get_type(handle))),
                                toB_u64(0) // TODO: should be None type instead, right?
                                );
    }

    B_dictD_setitem(walk_res->d, walk_res->wit, toB_u64((long)handle), val);
}

B_dict actonQ_rtsQ__io_handles (B_SysCap cap) {
    B_Hashable wit = (B_Hashable)B_HashableD_u64G_witness;
    B_dict d = $NEW(B_dict, wit, NULL, NULL);

    struct actonQ_rtsQ_io_handles_walk_res walk_res;
    walk_res.wit = wit;
    walk_res.d = d;
    uv_walk(get_uv_loop(), actonQ_rtsQ_io_handles_walk_cb, (void *)&walk_res);

    return d;
}

$R actonQ_rtsQ_WThreadMonitorD__init (actonQ_rtsQ_WThreadMonitor self, $Cont C_cont) {
    set_actor_affinity(from$int(self->wthread_id));
    return $RU_CONT(C_cont, B_None);
}
