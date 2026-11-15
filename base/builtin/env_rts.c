#include <gc.h>
#include <acton_gc_config.h>

#include <time.h>
#include <uv.h>

#include "rts/io.h"
#include "rts/log.h"
#include "rts/perf.h"

B_NoneType B_EnvRTSD_gc (B_EnvRTS self) {
    GC_gcollect();
    return B_None;
}

B_NoneType B_EnvRTSD_enable_gc (B_EnvRTS self) {
    GC_enable();
    return B_None;
}

B_NoneType B_EnvRTSD_disable_gc (B_EnvRTS self) {
    GC_disable();
    return B_None;
}

B_NoneType B_EnvRTSD_start_gc_performance_measurement (B_EnvRTS self) {
    GC_start_performance_measurement();
    return B_None;
}

struct acton_gc_info {
    struct GC_prof_stats_s stats;
    struct GC_timeval_s time_limit;
    unsigned backend;
    unsigned supported_backends;
    int incremental;
    GC_word free_space_divisor;
    int full_frequency;
    GC_word heap_growth_divisor;
    GC_word alloc_budget_percent;
    GC_word memory_limit;
    GC_word heap_ceiling_percent;
    int realloc_no_free;
    int no_thread_local_warmup;
    unsigned tl_refill_max;
};

static void *GC_CALLBACK read_gc_info(void *data) {
    struct acton_gc_info *info = data;
#if ACTON_GC_THREADS
    GC_get_prof_stats_unsafe(&info->stats, sizeof(info->stats));
#else
    GC_get_prof_stats(&info->stats, sizeof(info->stats));
#endif
#if defined(__wasi__)
    info->time_limit.tv_ms = GC_get_time_limit();
    info->time_limit.tv_nsec = 0;
#else
    info->time_limit = GC_get_time_limit_tv();
#endif
    info->backend = GC_get_actual_vdb();
    info->supported_backends = GC_get_supported_vdbs();
    info->incremental = GC_is_incremental_mode();
    info->free_space_divisor = GC_get_free_space_divisor();
    info->full_frequency = GC_get_full_freq();
    info->heap_growth_divisor = GC_get_heap_growth_divisor();
    info->alloc_budget_percent = GC_get_alloc_budget_percent();
    info->memory_limit = GC_get_memory_limit();
    info->heap_ceiling_percent = GC_get_heap_ceiling_percent();
    info->realloc_no_free = GC_get_realloc_no_free();
    info->no_thread_local_warmup = GC_get_no_thread_local_warmup();
    info->tl_refill_max = GC_get_tl_refill_max();
    return NULL;
}

static const struct {
    unsigned flag;
    const char *name;
} gc_backends[] = {
    {GC_VDB_MPROTECT, "mprotect"},
    {GC_VDB_MANUAL, "manual"},
    {GC_VDB_DEFAULT, "default"},
    {GC_VDB_GWW, "get_write_watch"},
    {GC_VDB_PROC, "proc"},
    {GC_VDB_SOFT, "soft_dirty"},
    {GC_VDB_UFFDWP, "userfaultfd"},
};

B_tuple B_EnvRTSD_gc_info (B_EnvRTS self) {
    struct acton_gc_info info;
#if ACTON_GC_THREADS
    GC_call_with_reader_lock(read_gc_info, &info, 0);
#else
    read_gc_info(&info);
#endif
    // The getter takes the allocator lock itself.
    size_t bytes_per_marker = GC_get_bytes_per_marker();

    // Construct Acton values only after releasing the collector lock.
    // The collector's incremental mode is generational mode, whether the
    // collector marks each collection in steps or in one stop.
    const char *mode = info.incremental ? "generational" : "ordinary";
    const char *backend = info.backend == GC_VDB_NONE ? "none" : "unknown";
    B_list supported = B_listG_new(NULL, NULL);
    B_SequenceD_list sequence = B_SequenceD_listG_witness;
    for (size_t i = 0; i < sizeof(gc_backends) / sizeof(gc_backends[0]); i++) {
        if (info.backend == gc_backends[i].flag)
            backend = gc_backends[i].name;
        if (info.supported_backends & gc_backends[i].flag)
            sequence->$class->append(sequence, supported,
                                    actStrFromCString(gc_backends[i].name));
    }
    // The warm-up and refill settings have no effect without thread-local
    // allocation, which a zero limit tells.
    unsigned thread_local_size_limit = acton_gc_get_thread_local_size_limit();
    B_float pause_target = (B_float)B_None;
    if (info.incremental && info.time_limit.tv_ms != GC_TIME_UNLIMITED)
        pause_target = to$float((double)info.time_limit.tv_ms
                               + (double)info.time_limit.tv_nsec / 1000000.0);

    return $NEWTUPLE(22,
        actStrFromCString(mode), actStrFromCString(backend), supported,
        toB_u64(acton_gc_get_page_hash_table_log2()),
        toB_u64(acton_gc_get_block_size()),
        toB_bool(acton_gc_get_end_padding()),
        toB_u64(thread_local_size_limit),
        toB_bool(thread_local_size_limit != 0 && info.no_thread_local_warmup),
        toB_u64(thread_local_size_limit != 0 ? info.tl_refill_max : 0),
        toB_bool(info.realloc_no_free),
        toB_u64(info.stats.markers_m1 + 1), toB_u64(bytes_per_marker),
        toB_bool(acton_gc_get_mark_range_stealing()),
        toB_bool(acton_gc_get_mark_bits()),
        toB_u64(acton_gc_get_initial_mark_stack_size()), pause_target,
        toB_u64(info.free_space_divisor), toB_u64(info.full_frequency),
        toB_u64(info.heap_growth_divisor), toB_u64(info.alloc_budget_percent),
        toB_u64(info.memory_limit), toB_u64(info.heap_ceiling_percent));
}

struct rts_gc_stats {
    struct GC_prof_stats_s heap;
    uint64_t total_ms;
    uint64_t mark_ms;
};

static void *GC_CALLBACK read_gc_stats(void *data) {
    struct rts_gc_stats *stats = data;
#if ACTON_GC_THREADS
    GC_get_prof_stats_unsafe(&stats->heap, sizeof(stats->heap));
#else
    GC_get_prof_stats(&stats->heap, sizeof(stats->heap));
#endif
    stats->total_ms = GC_get_full_gc_total_time();
    stats->mark_ms = GC_get_stopped_mark_total_time();
    return NULL;
}

B_tuple B_EnvRTSD_gc_stats(B_EnvRTS self) {
    struct rts_gc_stats stats;
#if ACTON_GC_THREADS
    GC_call_with_reader_lock(read_gc_stats, &stats, 0);
#else
    read_gc_stats(&stats);
#endif
    return $NEWTUPLE(9,
        toB_u64(stats.heap.heapsize_full), toB_u64(stats.heap.free_bytes_full),
        toB_u64(stats.heap.unmapped_bytes),
        toB_u64(stats.heap.heapsize_full - stats.heap.free_bytes_full),
        toB_u64(stats.heap.allocd_bytes_before_gc + stats.heap.bytes_allocd_since_gc),
        toB_u64(stats.heap.bytes_allocd_since_gc),
        toB_u64(stats.total_ms), toB_u64(stats.mark_ms),
        toB_u64(stats.total_ms - stats.mark_ms));
}

B_tuple B_EnvRTSD_process_stats(B_EnvRTS self) {
    size_t rss;
    uint64_t peak;
    B_u64 resident = uv_resident_set_memory(&rss) == 0 ? toB_u64(rss) : (B_u64)B_None;
    B_u64 peak_rss = rts_perf_peak_rss(&peak) ? toB_u64(peak) : (B_u64)B_None;
    uv_rusage_t usage;
    int available = uv_getrusage(&usage) == 0;
    return $NEWTUPLE(4, resident, peak_rss,
        available ? toB_u64((uint64_t)usage.ru_utime.tv_sec * UINT64_C(1000000000)
                           + (uint64_t)usage.ru_utime.tv_usec * 1000) : (B_u64)B_None,
        available ? toB_u64((uint64_t)usage.ru_stime.tv_sec * UINT64_C(1000000000)
                           + (uint64_t)usage.ru_stime.tv_usec * 1000) : (B_u64)B_None);
}

B_tuple B_EnvRTSD_perf_stats (B_EnvRTS self) {
    struct rts_perf_sample s;
    rts_perf_read(&s);
    return $NEWTUPLE(8,
        s.cpu_available ? toB_u64(s.user_ns) : (B_u64)B_None,
        s.cpu_available ? toB_u64(s.system_ns) : (B_u64)B_None,
        s.hardware_available ? toB_u64(s.instructions) : (B_u64)B_None,
        s.hardware_available ? toB_u64(s.cycles) : (B_u64)B_None,
        toB_u64(s.instructions_enabled), toB_u64(s.instructions_running),
        toB_u64(s.cycles_enabled), toB_u64(s.cycles_running));
}

B_dict B_EnvRTSD_perf_info (B_EnvRTS self) {
    B_Hashable wit = (B_Hashable)B_HashableD_strG_witness;
    B_dict info = $NEW(B_dict, wit, NULL, NULL);
    B_dictD_setitem(info, actStrFromCString("version"), actStrFromCString("1"));
    B_dictD_setitem(info, actStrFromCString("backend"), actStrFromCString(rts_perf_backend()));
    B_dictD_setitem(info, actStrFromCString("scope"), actStrFromCString(rts_perf_scope()));
    B_dictD_setitem(info, actStrFromCString("status"), actStrFromCString(rts_perf_status()));
    uv_utsname_t system;
    if (uv_os_uname(&system) == 0) {
        B_dictD_setitem(info, actStrFromCString("os"), actStrFromCStringCopy(system.sysname));
        B_dictD_setitem(info, actStrFromCString("release"), actStrFromCStringCopy(system.release));
        B_dictD_setitem(info, actStrFromCString("arch"), actStrFromCStringCopy(system.machine));
    }
    uv_cpu_info_t *cpus;
    int count;
    if (uv_cpu_info(&cpus, &count) == 0) {
        if (count > 0)
            B_dictD_setitem(info, actStrFromCString("cpu"), actStrFromCStringCopy(cpus[0].model));
        uv_free_cpu_info(cpus, count);
    }
    return info;
}

B_NoneType B_EnvRTSD_sleep (B_EnvRTS self, double sleep_time) {
    double st = sleep_time;
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

struct rts_io_handles_walk_res {
    B_dict d;
};

static void rts_io_handles_walk_cb (uv_handle_t *handle, void *arg) {
    struct rts_io_handles_walk_res *walk_res = (struct rts_io_handles_walk_res *)arg;
    B_tuple val;
    if (uv_handle_get_type(handle) == UV_TCP) {
        $Actor hactor = handle->data;
        val = (B_tuple)$NEWTUPLE(2,
                                actStrFromCString(uv_handle_type_name(uv_handle_get_type(handle))),
                                toB_u64((uint64_t)(uintptr_t)hactor)
                                );
    } else {
        val = (B_tuple)$NEWTUPLE(2,
                                actStrFromCString(uv_handle_type_name(uv_handle_get_type(handle))),
                                toB_u64(0) // TODO: should be None type instead, right?
                                );
    }

    B_dictD_setitem(walk_res->d, toB_u64((uint64_t)(uintptr_t)handle), val);
}

B_dict B__rts_io_handles (void) {
    B_Hashable wit = (B_Hashable)B_HashableD_u64G_witness;
    B_dict d = $NEW(B_dict, wit, NULL, NULL);

    struct rts_io_handles_walk_res walk_res;
    walk_res.d = d;
    uv_walk(get_uv_loop(), rts_io_handles_walk_cb, (void *)&walk_res);

    return d;
}

B_dict B_EnvRTSD_worker_info(B_EnvRTS self) {
    B_dict res = $NEW(B_dict, (B_Hashable)B_HashableD_intG_witness, NULL, NULL);
    for (int64_t i = 0; i <= num_wthreads; i++) {
        B_str name = i == 0 ? actStrFromCString("main") : $FORMAT("Worker %" PRId64, i);
        B_dictD_setitem(res, toB_int(i), $NEWTUPLE(1, name));
    }
    return res;
}

B_dict B_EnvRTSD_worker_stats(B_EnvRTS self) {
    B_dict res = $NEW(B_dict, (B_Hashable)B_HashableD_intG_witness, NULL, NULL);
#define WT(f) WT_STAT_GET(wt_stats[i].f)
    for (int64_t i = 0; i <= num_wthreads; i++) {
        B_tuple stats = $NEWTUPLE(47,
            actStrFromCString(WT_State_name[WT(state)]), toB_bool(record_waits),
            toB_u64(WT(sleeps)),
            toB_u64(WT(conts_count)),
            toB_u64(WT(conts_sum)),
            toB_u64(WT(conts_100ns)),
            toB_u64(WT(conts_1us)),
            toB_u64(WT(conts_10us)),
            toB_u64(WT(conts_100us)),
            toB_u64(WT(conts_1ms)),
            toB_u64(WT(conts_10ms)),
            toB_u64(WT(conts_100ms)),
            toB_u64(WT(conts_1s)),
            toB_u64(WT(conts_10s)),
            toB_u64(WT(conts_100s)),
            toB_u64(WT(conts_inf)),
            toB_u64(WT(bkeep_count)),
            toB_u64(WT(bkeep_sum)),
            toB_u64(WT(bkeep_100ns)),
            toB_u64(WT(bkeep_1us)),
            toB_u64(WT(bkeep_10us)),
            toB_u64(WT(bkeep_100us)),
            toB_u64(WT(bkeep_1ms)),
            toB_u64(WT(bkeep_10ms)),
            toB_u64(WT(bkeep_100ms)),
            toB_u64(WT(bkeep_1s)),
            toB_u64(WT(bkeep_10s)),
            toB_u64(WT(bkeep_100s)),
            toB_u64(WT(bkeep_inf)),
            toB_u64(WT(wait_count)),
            toB_u64(WT(wait_sum)),
            toB_u64(WT(wait_max)),
            toB_u64(WT(wait_1us)),
            toB_u64(WT(wait_10us)),
            toB_u64(WT(wait_100us)),
            toB_u64(WT(wait_1ms)),
            toB_u64(WT(wait_10ms)),
            toB_u64(WT(wait_100ms)),
            toB_u64(WT(wait_1s)),
            toB_u64(WT(wait_inf)),
            toB_u64(WT(steal_ops)),
            toB_u64(WT(steal_actors)),
            toB_u64(WT(balance_ops)),
            toB_u64(WT(balance_actors)),
            toB_u64(WT(balance_looks)),
            toB_u64(WT(balance_young)),
            toB_u64(WT(balance_stale)));
        B_dictD_setitem(res, toB_int(i), stats);
    }
#undef WT
    return res;
}

$R B__RTSWorkerMonitorD__initG_local (B__RTSWorkerMonitor self, $Cont C_cont) {
    set_actor_affinity((int)self->wthread_id);
    return $R_CONT(C_cont, B_None);
}
