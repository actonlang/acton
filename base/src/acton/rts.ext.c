#define GC_THREADS 1
#include <gc.h>

#include <time.h>
#include <uv.h>

#include "rts/io.h"
#include "rts/log.h"

#ifdef ACTON_DB
#include <backend/client_api.h>
#endif

void actonQ_rtsQ___ext_init__() {
    // NOP
}

B_NoneType actonQ_rtsQ_gc (B_SysCap cap) {
    GC_gcollect_and_unmap();
    return B_None;
}

B_NoneType actonQ_rtsQ_enable_gc (B_SysCap cap) {
    GC_enable();
    return B_None;
}

B_NoneType actonQ_rtsQ_disable_gc (B_SysCap cap) {
    GC_disable();
    return B_None;
}

// def start_gc_performance_measurement(cap: SysCap) -> None:
B_NoneType actonQ_rtsQ_start_gc_performance_measurement (B_SysCap cap) {
    GC_start_performance_measurement();
    return B_None;
}

// def get_gc_time(cap: SysCap) -> (total: u64, mark: u64, sweep: u64):
B_tuple actonQ_rtsQ_get_gc_time (B_SysCap cap) {
    unsigned long total = GC_get_full_gc_total_time();
    unsigned long mark = GC_get_stopped_mark_total_time();
    unsigned long sweep = total - mark;

    B_tuple res = $NEWTUPLE(3,
                            toB_u64(total),
                            toB_u64(mark),
                            toB_u64(sweep)
                            );
    return res;
}

B_u64 actonQ_rtsQ_get_heap_size (B_SysCap cap) {
    return toB_u64(GC_get_heap_size());
}

B_u64 actonQ_rtsQ_get_mem_usage (B_SysCap cap) {
    return toB_u64(GC_get_heap_size() - GC_get_free_bytes());
}

// def get_gc_total_bytes(cap: SysCap) -> u64:
B_u64 actonQ_rtsQ_get_gc_total_bytes (B_SysCap cap) {
    return toB_u64(GC_get_total_bytes());
}

//def get_gc_bytes_since_gc(cap: SysCap) -> u64:
B_u64 actonQ_rtsQ_get_gc_bytes_since_gc (B_SysCap cap) {
    return toB_u64(GC_get_bytes_since_gc());
}

// def get_rss(cap: SysCap) -> u64:
B_u64 actonQ_rtsQ_get_rss (B_SysCap cap) {
    size_t rsm;
    int r = uv_resident_set_memory(&rsm);
    return toB_u64(rsm);
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

    // Use high precision nanosleep where available, otherwise fall back to uv_sleep.
#if defined(__linux__) || defined(__APPLE__)
    // Unlike usleep, nanosleep() tolerates signal interrupts and will write the
    // remaining time (that it didn't sleep) into the third argument. We spin
    // until it completes successfully.
    while (nanosleep(&ts, &ts) != 0) {
        // Continue sleeping with remaining time
    }
#else
    // For other platforms, fall back to uv_sleep (millisecond granularity)
    uv_sleep((uint64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
#endif
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

B_dict actonQ_rtsQ_db_actors (B_SysCap cap) {
#ifdef ACTON_DB
    B_Hashable wit = (B_Hashable)B_HashableD_u64G_witness;
    B_dict d = $NEW(B_dict, wit, NULL, NULL);
    for (snode_t *crt = HEAD(db->actors); crt!=NULL; crt = NEXT(crt)) {
        actor_descriptor *a = (actor_descriptor *)crt->value;

        B_tuple actor_info = $NEWTUPLE(2,
                                       to$str(a->host_rts->id),
                                       a->is_local?B_True:B_False,
                                       to_str_noc(Actor_status_name[a->status])
                                       );
        B_dictD_setitem(d, wit, toB_i64(a->actor_id), actor_info);
    }
    return d;
#else
    $RAISE(((B_BaseException)B_RuntimeErrorG_new($FORMAT("Not possible to list Actors in DB. Recompile with --db"))));
#endif
}

B_dict actonQ_rtsQ_db_servers (B_SysCap cap) {
#ifdef ACTON_DB
    B_Hashable wit = (B_Hashable)B_HashableD_u64G_witness;
    B_dict d = $NEW(B_dict, wit, NULL, NULL);
    for (snode_t *crt = HEAD(db->servers); crt!=NULL; crt = NEXT(crt)) {
        remote_server *rs = (remote_server *)crt->value;

        B_tuple member_info = $NEWTUPLE(3,
                                        to$str("DDB"),
                                        to$str(rs->hostname),
                                        to_str_noc(RS_status_name[rs->status])
                                        );
        B_dictD_setitem(d, wit, to$str(rs->id), member_info);
    }
    return d;
#else
    $RAISE(((B_BaseException)B_RuntimeErrorG_new($FORMAT("Not possible to list DB servers. Recompile with --db"))));
#endif
}

B_dict actonQ_rtsQ_db_rtses (B_SysCap cap) {
#ifdef ACTON_DB
    B_Hashable wit = (B_Hashable)B_HashableD_u64G_witness;
    B_dict d = $NEW(B_dict, wit, NULL, NULL);
    for (snode_t *crt = HEAD(db->rtses); crt!=NULL; crt = NEXT(crt)) {
        rts_descriptor *nd = (rts_descriptor *)crt->value;
        B_tuple member_info = $NEWTUPLE(6,
                                        to$str("RTS"),
                                        to$str(nd->hostname),
                                        to_str_noc(RS_status_name[nd->status]),
                                        toB_u64(nd->local_rts_id),
                                        toB_u64(nd->dc_id),
                                        toB_u64(nd->rack_id)
                                        );
        B_dictD_setitem(d, wit, to$str(nd->id), member_info);
    }
    return d;
#else
    $RAISE(((B_BaseException)B_RuntimeErrorG_new($FORMAT("Not possible to list DB RTSes. Recompile with --db"))));
#endif
}

B_dict actonQ_rtsQ_rts_stats (B_SysCap cap) {
    B_Hashable wit = (B_Hashable)B_HashableD_u64G_witness;
    B_dict d = $NEW(B_dict, wit, NULL, NULL);
    for (int i; i <= num_wthreads; i++) {
        B_tuple stats = $NEWTUPLE(28,
                            to_str_noc(WT_State_name[wt_stats[i].state]),
                            toB_u64(wt_stats[i].sleeps),
                            toB_u64(wt_stats[i].conts_count),
                            toB_u64(wt_stats[i].conts_sum),
                            toB_u64(wt_stats[i].conts_100ns),
                            toB_u64(wt_stats[i].conts_1us),
                            toB_u64(wt_stats[i].conts_10us),
                            toB_u64(wt_stats[i].conts_100us),
                            toB_u64(wt_stats[i].conts_1ms),
                            toB_u64(wt_stats[i].conts_10ms),
                            toB_u64(wt_stats[i].conts_100ms),
                            toB_u64(wt_stats[i].conts_1s),
                            toB_u64(wt_stats[i].conts_10s),
                            toB_u64(wt_stats[i].conts_100s),
                            toB_u64(wt_stats[i].conts_inf),
                            toB_u64(wt_stats[i].bkeep_count),
                            toB_u64(wt_stats[i].bkeep_sum),
                            toB_u64(wt_stats[i].bkeep_100ns),
                            toB_u64(wt_stats[i].bkeep_1us),
                            toB_u64(wt_stats[i].bkeep_10us),
                            toB_u64(wt_stats[i].bkeep_100us),
                            toB_u64(wt_stats[i].bkeep_1ms),
                            toB_u64(wt_stats[i].bkeep_10ms),
                            toB_u64(wt_stats[i].bkeep_100ms),
                            toB_u64(wt_stats[i].bkeep_1s),
                            toB_u64(wt_stats[i].bkeep_10s),
                            toB_u64(wt_stats[i].bkeep_100s),
                            toB_u64(wt_stats[i].bkeep_inf)
                            );
        B_dictD_setitem(d, wit, toB_u64(i), stats);
    }
    return d;
}

$R actonQ_rtsQ_WThreadMonitorD__initG_local (actonQ_rtsQ_WThreadMonitor self, $Cont C_cont) {
    set_actor_affinity(from$int(self->wthread_id));
    return $R_CONT(C_cont, B_None);
}
