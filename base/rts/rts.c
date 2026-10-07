/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifdef __linux__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#endif

#ifdef ACTON_THREADS
#define GC_THREADS 1
#endif
#include <gc.h>
#include <acton_gc_config.h>

#if defined(_WIN32) || defined(_WIN64)
#else
#include <termios.h>
#endif
#include <unistd.h>
#ifdef ACTON_THREADS
#include <pthread.h>
#endif
#include <stdio.h>
#include <stdarg.h>
#ifdef ACTON_DB
#include <uuid/uuid.h>
#endif
#include <signal.h>

#include <time.h>
#include <stdlib.h>

// Windows
#ifdef _WIN32
#else
#include <sys/un.h>
#include <sys/time.h>
#include <sys/wait.h>
#endif
#ifdef __APPLE__
#include <fcntl.h>
#include <mach/mach_time.h>
#endif
#ifdef __x86_64__
#include <cpuid.h>
#endif
#ifdef __linux__
#include <sys/prctl.h>
#ifdef ACTON_GC_DISABLE_THP
#include <sys/mman.h>
#endif
#endif

#include "common.h"
#include "common.c"

#include "yyjson.h"
#include "rts.h"

#include <uv.h>

#include "q.c"

#include "io.c"

#include "log.c"
#include "perf.c"
#include "netstring.h"
#include "../builtin/env.h"
#include "../builtin/function.h"

#ifdef ACTON_DB
#include "../backend/client_api.h"
#include "../backend/fastrand.h"
extern struct dbc_stat dbc_stats;
#endif

#ifndef _WIN32
struct sigaction sa_abrt, sa_ill, sa_int, sa_pipe, sa_segv, sa_term;
#endif

char rts_verbose = 0;
char rts_debug = 0;
long num_wthreads = -1;

char rts_exit = 0;
int return_val = 0;

char *appname = NULL;
pid_t pid;

uv_loop_t *aux_uv_loop = NULL;
uv_loop_t *uv_loops[MAX_WTHREADS];
uv_async_t stop_ev[MAX_WTHREADS];
uv_async_t wake_ev[MAX_WTHREADS];
uv_check_t work_ev[MAX_WTHREADS];
WorkerCtx wctxs[MAX_WTHREADS];

char *mon_log_path = NULL;
int mon_log_period = 30;
char *mon_socket_path = NULL;


struct wt_stat wt_stats[MAX_WTHREADS];

// Conveys current thread status, like what is it doing?
enum WT_State {WT_NoExist = 0, WT_Working = 1, WT_Idle = 2, WT_Sleeping = 3};
static const char *WT_State_name[] = {"poof", "work", "idle", "sleep"};

/*
 * Custom printf macros for printing verbose and debug information
 * RTS Debug Printf   = rtsd_printf
 */
#ifdef DEV
#define rtsd_printf(...) if (rts_debug) log_debug(__VA_ARGS__)
#else
#define rtsd_printf(...)
#endif


#if defined(IS_MACOS)
#include <sys/types.h>
#include <sys/sysctl.h>
#include <mach/mach_init.h>
#include <mach/thread_policy.h>

#define SYSCTL_CORE_COUNT   "machdep.cpu.core_count"

typedef struct cpu_set {
  uint32_t    count;
} cpu_set_t;

static inline void
CPU_ZERO(cpu_set_t *cs) { cs->count = 0; }

static inline void
CPU_SET(int num, cpu_set_t *cs) { cs->count |= (1 << num); }

static inline int
CPU_ISSET(int num, cpu_set_t *cs) { return (cs->count & (1 << num)); }

int sched_getaffinity(pid_t pid, size_t cpu_size, cpu_set_t *cpu_set)
{
    int32_t core_count = 0;
    size_t  len = sizeof(core_count);
    int ret = sysctlbyname(SYSCTL_CORE_COUNT, &core_count, &len, 0, 0);
    if (ret) {
        fprintf(stderr, "error getting the core count %d\n", ret);
        return -1;
    }
    cpu_set->count = 0;
    for (int i = 0; i < core_count; i++) {
        cpu_set->count |= (1 << i);
    }

    return 0;
}

kern_return_t thread_policy_set(
                    thread_t thread,
                    thread_policy_flavor_t flavor,
                    thread_policy_t policy_info,
                    mach_msg_type_number_t count);

int pthread_setaffinity_np(pthread_t thread, size_t cpu_size, cpu_set_t *cpu_set) {
    int core = 0;
    for (core = 0; core < 8 * cpu_size; core++) {
        if (CPU_ISSET(core, cpu_set))
            break;
    }
    thread_affinity_policy_data_t policy = { core };

    thread_port_t mach_thread = pthread_mach_thread_np(thread);
    thread_policy_set(mach_thread, THREAD_AFFINITY_POLICY, (thread_policy_t)&policy, 1);

    return 0;
}
#endif

extern void $ROOTINIT();
extern $Actor $ROOT();

struct mpmcq rqs[NUM_RQS];

$Actor root_actor = NULL;
B_Env env_actor = NULL;

struct TimedMsg {
    B_Msg msg;
    uint64_t sequence;
};

// Stable min-heap.
static struct TimedMsg *timerQ = NULL;
static size_t timerQ_len = 0;
static size_t timerQ_capacity = 0;
static uint64_t timerQ_sequence = 0;
static $Lock timerQ_lock;

// Every actor and message has a key, a negative number that is unique within
// the process. BOOTSTRAP() gives env and root the keys ENV_KEY and ROOT_KEY,
// so that deserialize_system() finds them after a restart. All other keys come
// from get_next_key() and start at key_top: FIRST_KEY, or below the smallest
// key that deserialize_system() restored.
#define ENV_KEY (-11)
#define ROOT_KEY (-12)
#define FIRST_KEY (-1024)
static int64_t key_top;
// The next key for threads that are not workers (see get_next_key())
static _Atomic int64_t other_key;

int64_t timer_consume_hd = 0;       // Lacks protection, although spinlocks wouldn't help concurrent increments. Must fix in db!

// The runtime's clock, in microseconds. Message baselines, and the timer
// queue that orders messages by them, are times on this clock, and thread 0's
// timer for the timed message queue counts on the same clock. It is
// CLOCK_BOOTTIME on Linux, or CLOCK_MONOTONIC on kernels whose timerfd
// rejects CLOCK_BOOTTIME, mach_continuous_time() on macOS, and uv_hrtime(),
// the clock that libuv's timers count on, elsewhere. The operating system
// keeps these clocks. They never go back, setting the wall clock does not
// move them, and they keep their count across a sleep, also on machines where
// a sleep resets the CPU's own counter. Serialized messages carry their
// baseline as wall clock time (B_MsgD___serialize__).
#if defined(__linux__)
// Set by init_clock()
static clockid_t rts_clock = CLOCK_BOOTTIME;
#elif defined(__APPLE__)
// Converts mach time to nanoseconds. Set by init_clock().
static mach_timebase_info_data_t rts_timebase;
#endif

time_t current_time() {
#if defined(__linux__)
    struct timespec ts;
    clock_gettime(rts_clock, &ts);
    return (int64_t)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
#elif defined(__APPLE__)
    return mach_continuous_time() * rts_timebase.numer / rts_timebase.denom / 1000;
#else
    return uv_hrtime() / 1000;
#endif
}

// The wall clock's time minus current_time(), in microseconds
static time_t wall_offset_us(void) {
    uv_timespec64_t ts;
    if (uv_clock_gettime(UV_CLOCK_REALTIME, &ts) != 0) {
        log_error("uv_clock_gettime() failed");
        return 0;
    }
    return ts.tv_sec * 1000000 + ts.tv_nsec / 1000 - current_time();
}

#ifdef ACTON_THREADS
pthread_key_t self_key;
pthread_key_t pkey_wctx;

pthread_mutex_t rts_exit_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t rts_exit_signal = PTHREAD_COND_INITIALIZER;

#define NUM_THREADS num_wthreads+1

WorkerCtx get_wctx() {
    WorkerCtx wctx = GET_WCTX();
    assert(wctx != NULL);
    return wctx;
}

int get_wtid() {
    return (int)get_wctx()->id;
}

void pin_actor_affinity() {
    $Actor a = ($Actor)pthread_getspecific(self_key);
    long i = get_wctx()->id;
    log_debug("Pinning affinity for %s actor %" PRId64 " to current WT %d", a->$class->$GCINFO, a->$globkey, i);
    a->$affinity = i;
}

void set_actor_affinity(int wthread_id) {
    $Actor a = ($Actor)pthread_getspecific(self_key);
    log_debug("Setting affinity for %s actor %" PRId64 " to WT %d", a->$class->$GCINFO, a->$globkey, wthread_id);
    a->$affinity = wthread_id;
}
#else // ACTON_THREADS
$Actor self_actor;

#define NUM_THREADS 1

WorkerCtx get_wctx() { return GET_WCTX(); }
int get_wtid() { return (int)get_wctx()->id; }
void pin_actor_affinity() { }
void set_actor_affinity(int wthread_id) { }
#endif // ACTON_THREADS

#ifdef ACTON_THREADS
static pthread_mutex_t sync_pause_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t sync_pause_cond = PTHREAD_COND_INITIALIZER;
// Every worker reads the request flag before each continuation, and it is
// written only when a pause starts or ends. It has a cache line of its own
// so that the check stays a load from the reader's own cache. The flag is
// written with atomic_exchange(): if it were only loaded and stored, the
// compiler could replace the struct with its field and drop the padding.
static struct {
    _Atomic int requested;
} __attribute__((aligned(128))) sync_pause_flag;
static int sync_pause_owner = -1;
static int sync_pause_parked_count = 0;
static int sync_pause_workers_are_started = 0;
static int sync_pause_parked[MAX_WTHREADS];

static void sync_pause_clear_parked(void) {
    for (int i = 0; i <= num_wthreads; i++) {
        sync_pause_parked[i] = 0;
    }
}

static void wake_all_wt(void) {
    // Workers read the pause request without a lock (maybe_sync_pause), so
    // order it before uv_async_send()'s read of a pending wake, as in
    // wake_loop()
    atomic_thread_fence(memory_order_seq_cst);
    for (int i = 0; i <= num_wthreads; i++) {
        uv_async_send(&wake_ev[i]);
    }
}

static void sync_pause_wait(void) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    // Use a bounded wait so pause owners and parked workers re-check rts_exit
    // even if shutdown starts without a matching condition broadcast.
    ts.tv_nsec += 10 * 1000 * 1000;
    if (ts.tv_nsec >= 1000 * 1000 * 1000) {
        ts.tv_sec++;
        ts.tv_nsec -= 1000 * 1000 * 1000;
    }
    pthread_cond_timedwait(&sync_pause_cond, &sync_pause_lock, &ts);
}

int acton_sync_pause_begin(void) {
    WorkerCtx wctx = GET_WCTX();
    if (wctx == NULL || wctx->id < 0 || wctx->id > num_wthreads) {
        return -1;
    }
    int owner = (int)wctx->id;

    pthread_mutex_lock(&sync_pause_lock);
    if (!sync_pause_workers_are_started || sync_pause_flag.requested) {
        pthread_mutex_unlock(&sync_pause_lock);
        return -1;
    }
    if (rts_exit) {
        pthread_mutex_unlock(&sync_pause_lock);
        return -1;
    }

    atomic_exchange(&sync_pause_flag.requested, 1);
    sync_pause_owner = owner;
    sync_pause_parked_count = 0;
    sync_pause_clear_parked();

    wake_all_wt();
    while (sync_pause_parked_count < num_wthreads && !rts_exit) {
        sync_pause_wait();
    }
    if (rts_exit) {
        atomic_exchange(&sync_pause_flag.requested, 0);
        sync_pause_owner = -1;
        sync_pause_parked_count = 0;
        pthread_cond_broadcast(&sync_pause_cond);
        pthread_mutex_unlock(&sync_pause_lock);
        return -1;
    }

    pthread_mutex_unlock(&sync_pause_lock);
    return 0;
}

void acton_sync_pause_end(void) {
    WorkerCtx wctx = GET_WCTX();
    if (wctx == NULL || wctx->id < 0 || wctx->id > num_wthreads) {
        return;
    }
    int owner = (int)wctx->id;

    pthread_mutex_lock(&sync_pause_lock);
    if (!sync_pause_flag.requested || sync_pause_owner != owner) {
        pthread_mutex_unlock(&sync_pause_lock);
        return;
    }

    atomic_exchange(&sync_pause_flag.requested, 0);
    sync_pause_owner = -1;
    sync_pause_parked_count = 0;
    pthread_cond_broadcast(&sync_pause_cond);
    pthread_mutex_unlock(&sync_pause_lock);
}

// Called from the worker loop between actor continuations. If a sync pause is
// active, non-owner workers park here while the owner runs the synchronized op.
static void maybe_sync_pause(void) {
    // Without a pause request this is one load. A worker that reads the
    // flag just before a pause starts sees it at its next check; the pause
    // owner waits until every worker has parked.
    if (!atomic_load_explicit(&sync_pause_flag.requested, memory_order_relaxed))
        return;
    WorkerCtx wctx = GET_WCTX();
    if (wctx == NULL || wctx->id < 0 || wctx->id >= MAX_WTHREADS) {
        return;
    }
    int id = (int)wctx->id;

    pthread_mutex_lock(&sync_pause_lock);
    while (sync_pause_flag.requested && id != sync_pause_owner && !rts_exit) {
        if (!sync_pause_parked[id]) {
            sync_pause_parked[id] = 1;
            sync_pause_parked_count++;
            pthread_cond_broadcast(&sync_pause_cond);
        }
        sync_pause_wait();
    }
    pthread_mutex_unlock(&sync_pause_lock);
}

static void sync_pause_workers_started(void) {
    pthread_mutex_lock(&sync_pause_lock);
    sync_pause_workers_are_started = 1;
    pthread_mutex_unlock(&sync_pause_lock);
}
#else
int acton_sync_pause_begin(void) { return 0; }
void acton_sync_pause_end(void) { }
static void maybe_sync_pause(void) { }
static void sync_pause_workers_started(void) { }
#endif

// Wake the loop that ev belongs to, after the caller made something visible
// that its thread reads without a lock: an actor in its pinned queue, whose
// head it reads before taking the lock. uv_async_send() sends nothing while
// an earlier wake is still pending, and it learns that from a plain read.
// Without a full fence, that read can complete before the caller's stores
// are visible to the loop's thread. The loop may then take the earlier wake,
// clear it, look and find nothing, while we see the wake as pending and send
// nothing. The fence keeps the read after the caller's stores: either we
// send a wake, or the loop has yet to take the pending one and sees the
// stores when it does.
static inline void wake_loop(uv_async_t *ev) {
    atomic_thread_fence(memory_order_seq_cst);
    uv_async_send(ev);
}

void wake_wt(int wtid) {
    // We are sometimes optimistically called, i.e. the caller sometimes does
    // not really know whether there is new work or not. We check and if there
    // is not, then there is no need to wake anyone up.
    if (!rqs[wtid].head)
        return;

#ifdef ACTON_THREADS
    // wake up corresponding worker threads....
    if (wtid == SHARED_RQ) {
        // When the caller has just put an actor on the shared queue,
        // releasing the queue lock does not order that store before our
        // reads of wt_stats[].state below: on Arm those reads may complete
        // before the actor is visible to other threads. This fence orders
        // the enqueue before those reads. It pairs with the fence a worker
        // issues after storing WT_Idle and before it reads the queues for
        // the last time (wt_work_cb()): either that worker finds the actor,
        // or we read WT_Idle and wake it.
        atomic_thread_fence(memory_order_seq_cst);
        for (int i = 1; i <= num_wthreads; i++) {
            if (wt_stats[i].state == WT_Idle) {
                uv_async_send(&wake_ev[i]);
                return;
            }
        }
    } else {
        // thread specific queue
        wake_loop(&wake_ev[wtid]);
    }
#else
    uv_async_send(&wake_ev[wtid]);
#endif

}

void reset_timeout() {
    // Wake up timerQ thread
    uv_async_send(&wake_ev[0]);
}
// Keys only need to be unique. Worker w hands out key_top - w and then every
// key_step-th number below it, where key_step is the number of workers plus
// one. The worker keeps its next key in its context, so taking a key writes
// no shared memory. The sequence that is left over is for threads that are
// not workers. They take their keys from other_key with an atomic
// subtraction. A finalizer can send a message, and it runs on whichever thread
// allocates memory. For example, with workers 0 to 3 (num_wthreads 3),
// key_step is 5. From key_top -1024, worker 0 hands out -1024, -1029, -1034
// and so on, worker 3 hands out -1027, -1032, -1037, and threads that are not
// workers take -1028, -1033, -1038.
int64_t get_next_key() {
    WorkerCtx wctx = GET_WCTX();
    if (!wctx)
        return atomic_fetch_sub_explicit(&other_key, num_wthreads + 2, memory_order_relaxed);
    int64_t key = wctx->key_next;
    wctx->key_next = key - wctx->key_step;
    return key;
}

static void start_worker_keys(WorkerCtx wctx) {
    wctx->key_next = key_top - wctx->id;
    wctx->key_step = num_wthreads + 2;
}

// Makes all keys from now on start at top. Only the main thread runs at this
// point; the other workers start their keys in main_loop().
static void start_keys(int64_t top) {
    key_top = top;
    atomic_store_explicit(&other_key, top - num_wthreads - 1, memory_order_relaxed);
    start_worker_keys(get_wctx());
}

#define ACTORS_TABLE    ($WORD)0
#define MSGS_TABLE      ($WORD)1
#define MSG_QUEUE       ($WORD)2

#define TIMER_QUEUE     0           // Special key in table MSG_QUEUE

#ifdef ACTON_DB
remote_db_t * db = NULL;
#endif


#if defined(_WIN32) || defined(_WIN64)
// TODO: termios support in windows?
#else
struct termios old_stdin_attr;
#endif

////////////////////////////////////////////////////////////////////////////////////////

/* 

The strangeness of the next 30 lines are caused by the unfortunate presence of Msg in __builtin__.act.

-- This generates a stub of B_MsgD___init__ with wrong parameters, and its presence in the method table, so we define it here, but never use it.
-- The out-commented version is how __init__ should really be defined
-- The B_msgG_newXX function now inlines the proper __init__; it has to be renamed because of a generated and improper B_msgG_new.

*/

B_NoneType B_MsgD___init__ (B_Msg G_1p) {
    // Must (and will) never be called!
    return B_None;
}

/*
void B_MsgD___init__(B_Msg m, $Actor to, $Cont cont, time_t baseline, $WORD value) {
    m->$next = NULL;
    m->$to = to;
    m->$cont = cont;
    m->$waiting = NULL;
    m->$baseline = baseline;
    m->value = value;
    atomic_store(&m->$wait_lock, 0);
    m->$globkey = get_next_key();
}
*/

B_Msg B_MsgG_newXX( $Actor to, $Cont cont, time_t baseline, $WORD value) {
    B_Msg m = GC_malloc(sizeof(struct B_Msg));
    m->$class = &B_MsgG_methods;
    m->$next = NULL;
    m->$to = to;
    m->$cont = cont;
    m->$waiting = NULL;
    m->$baseline = baseline;
    m->value = value;
    // No other thread can see the new message yet, so the store needs no
    // ordering
    atomic_store_explicit(&m->$wait_lock, 0, memory_order_relaxed);
    m->$globkey = get_next_key();
    return m;
}


////////////////////////////////////////////////////////////////////////

bool B_MsgD___bool__(B_Msg self) {
  return true;
}

B_str B_MsgD___str__(B_Msg self) {
  return $FORMAT("<B_Msg object at %p>", self);
}

B_str B_MsgD___repr__(B_Msg self) {
  return B_MsgD___str__(self);
}

void B_MsgD___serialize__(B_Msg self, $Serial$state state) {
    $step_serialize(self->$to,state);
    $step_serialize(self->$cont,state);
    time_t wall = self->$baseline + wall_offset_us();
    $val_serialize(ITEM_ID,&wall,state);
    $step_serialize(self->value,state);
}


B_Msg B_MsgD___deserialize__(B_Msg res, $Serial$state state) {
    if (!res) {
        if (!state) {
            res = GC_malloc(sizeof (struct B_Msg));
            res->$class = &B_MsgG_methods;
            return res;
        }
        res = $DNEW(B_Msg,state);
    }
    res->$next = NULL;
    res->$to = $step_deserialize(state);
    res->$cont = $step_deserialize(state);
    res->$waiting = NULL;
    res->$baseline = (time_t)$val_deserialize(state) - wall_offset_us();
    res->value = $step_deserialize(state);
    atomic_store_explicit(&res->$wait_lock, 0, memory_order_relaxed);
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $ActorD___init__($Actor a) {
    a->$next = NULL;
    a->B_Msg = NULL;
    a->$outgoing = NULL;
    a->$waitsfor = NULL;
    a->$consume_hd = 0;
    a->$catcher = NULL;
    // No other thread can see the new actor yet
    atomic_store_explicit(&a->B_Msg_lock, 0, memory_order_relaxed);
    a->$globkey = get_next_key();
    a->$affinity = SHARED_RQ;
    rtsd_printf("# New Actor %" PRId64 " at %p of class %s", a->$globkey, a, a->$class->$GCINFO);
}

bool $ActorD___bool__($Actor self) {
  return true;
}

B_str $ActorD___str__($Actor self) {
  return $FORMAT("<$Actor %" PRId64 " %s at %p>", self->$globkey, self->$class->$GCINFO, self);
}

B_NoneType $ActorD___resume__($Actor self) {
  return B_None;
}

B_NoneType $ActorD___cleanup__($Actor self) {
  return B_None;
}

void $ActorD___serialize__($Actor self, $Serial$state state) {
    $step_serialize(self->$waitsfor,state);
    $val_serialize(ITEM_ID,&self->$consume_hd,state);
    $step_serialize(self->$catcher,state);
}

$Actor $ActorD___deserialize__($Actor res, $Serial$state state) {
    if (!res) {
        if (!state) {
            res = GC_malloc(sizeof(struct $Actor));
            res->$class = &$ActorG_methods;
            return res;
        }
        res = $DNEW($Actor, state);
    }
    res->$next = NULL;
    res->B_Msg = NULL;
    res->$outgoing = NULL;
    res->$waitsfor = $step_deserialize(state);
    res->$consume_hd = (long)$val_deserialize(state);
    res->$catcher = $step_deserialize(state);
    atomic_store_explicit(&res->B_Msg_lock, 0, memory_order_relaxed);
    if (res->$affinity > 0)
        res->$affinity = SHARED_RQ;
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $CatcherD___init__($Catcher c, $Cont cont) {
    c->$next = NULL;
    c->$cont = cont;
    c->xval = NULL;
}

bool $CatcherD___bool__($Catcher self) {
  return true;
}

B_str $CatcherD___str__($Catcher self) {
  return $FORMAT("<$Catcher object at %p>", self);
}

void $CatcherD___serialize__($Catcher self, $Serial$state state) {
    $step_serialize(self->$next,state);
    $step_serialize(self->$cont,state);
    $step_serialize(self->xval,state);
}

$Catcher $CatcherD___deserialize__($Catcher self, $Serial$state state) {
    $Catcher res = $DNEW($Catcher,state);
    res->$next = $step_deserialize(state);
    res->$cont = $step_deserialize(state);
    res->xval = $step_deserialize(state);
    return res;
}
///////////////////////////////////////////////////////////////////////////////////////

void $ConstContD___init__($ConstCont $this, $WORD val, $Cont cont) {
    $this->val = val;
    $this->cont = cont;
}

bool $ConstContD___bool__($ConstCont self) {
  return true;
}

B_str $ConstContD___str__($ConstCont self) {
  return $FORMAT("<$ConstCont object at %p>", self);
}

void $ConstContD___serialize__($ConstCont self, $Serial$state state) {
    $step_serialize(self->val,state);
    $step_serialize(self->cont,state);
}

$ConstCont $ConstContD___deserialize__($ConstCont self, $Serial$state state) {
    $ConstCont res = $DNEW($ConstCont,state);
    res->val = $step_deserialize(state);
    res->cont = $step_deserialize(state);
    return res;
}

$R $ConstContD___call__($ConstCont $this, $WORD _ignore) {
    $Cont cont = $this->cont;
    return cont->$class->__call__(cont, $this->val);
}

$Cont $CONSTCONT($WORD val, $Cont cont){
    $ConstCont obj = GC_malloc(sizeof(struct $ConstCont));
    obj->$class = &$ConstContG_methods;
    $ConstContG_methods.__init__(obj, val, cont);
    return ($Cont)obj;
}

////////////////////////////////////////////////////////////////////////////////////////

/*
struct B_MsgG_class B_MsgG_methods = {
    MSG_HEADER,
    UNASSIGNED,
    NULL,
    NULL,
    B_MsgD___serialize__,
    B_MsgD___deserialize__,
    B_MsgD___bool__,
    B_MsgD___str__,
    B_MsgD___str__
};
*/

struct $ActorG_class $ActorG_methods = {
    ACTOR_HEADER,
    UNASSIGNED,
    NULL,
    $ActorD___init__,
    $ActorD___serialize__,
    $ActorD___deserialize__,
    $ActorD___bool__,
    $ActorD___str__,
    $ActorD___str__,
    $ActorD___resume__,
    $ActorD___cleanup__
};

struct $CatcherG_class $CatcherG_methods = {
    CATCHER_HEADER,
    UNASSIGNED,
    NULL,
    $CatcherD___init__,
    $CatcherD___serialize__,
    $CatcherD___deserialize__,
    $CatcherD___bool__,
    $CatcherD___str__,
    $CatcherD___str__
};

struct $ConstContG_class $ConstContG_methods = {
    "$ConstCont",
    UNASSIGNED,
    NULL,
    $ConstContD___init__,
    $ConstContD___serialize__,
    $ConstContD___deserialize__,
    $ConstContD___bool__,
    $ConstContD___str__,
    $ConstContD___str__,
    $ConstContD___call__
};

////////////////////////////////////////////////////////////////////////////////////////


#define MARK_RESULT         NULL
#define MARK_EXCEPTION      ($Cont)1

#define EXCEPTIONAL(m)      (m->$cont == MARK_EXCEPTION)
#define FROZEN(m)           (m->$cont == MARK_RESULT || EXCEPTIONAL(m))

// Atomically add actor "a" to the waiting list of messasge "m" if it is not frozen (and return true),
// else immediately return false.
bool ADD_waiting($Actor a, B_Msg m) {
    bool did_add = false;

    assert(m != NULL);

    spinlock_lock(&m->$wait_lock);
    if (!FROZEN(m)) {
        a->$next = m->$waiting;
        m->$waiting = a;
        a->$waitsfor = m;
        did_add = true;
    }
    spinlock_unlock(&m->$wait_lock);
    return did_add;
}

// Atomically freeze message "m" using "mark", and return its list of waiting actors.
$Actor FREEZE_waiting(B_Msg m, $Cont mark) {
    spinlock_lock(&m->$wait_lock);
    m->$cont = mark;
    $Actor res = m->$waiting;
    m->$waiting = NULL;
    spinlock_unlock(&m->$wait_lock);
    return res;
}

static bool timed_before(struct TimedMsg a, struct TimedMsg b) {
    if (a.msg->$baseline != b.msg->$baseline)
        return a.msg->$baseline < b.msg->$baseline;
    return a.sequence < b.sequence;
}

// Atomically enqueue timed message "m" onto the global timer-queue, at position
// given by "m->baseline".
bool ENQ_timed(B_Msg m) {
    spinlock_lock(&timerQ_lock);
    bool new_head = timerQ_len == 0 || m->$baseline < timerQ[0].msg->$baseline;
    if (timerQ_len == timerQ_capacity) {
        size_t capacity = timerQ_capacity ? 2 * timerQ_capacity : 64;
        struct TimedMsg *queue = GC_realloc(timerQ, capacity * sizeof(*timerQ));
        if (!queue) {
            log_fatal("Unable to grow timer queue");
            exit(1);
        }
        timerQ = queue;
        timerQ_capacity = capacity;
    }
    struct TimedMsg entry = {m, timerQ_sequence++};
    m->$next = NULL;
    size_t pos = timerQ_len++;
    while (pos > 0) {
        size_t parent = (pos - 1) / 2;
        if (!timed_before(entry, timerQ[parent]))
            break;
        timerQ[pos] = timerQ[parent];
        pos = parent;
    }
    timerQ[pos] = entry;
    spinlock_unlock(&timerQ_lock);
    return new_head;
}

// Atomically dequeue and return the first message from the global timer-queue if 
// its baseline is less or equal to "now", else return NULL.
B_Msg DEQ_timed(time_t now) {
    spinlock_lock(&timerQ_lock);
    B_Msg res = NULL;
    if (timerQ_len && timerQ[0].msg->$baseline <= now) {
        res = timerQ[0].msg;
        struct TimedMsg entry = timerQ[--timerQ_len];
        // Drop GC references to delivered messages.
        timerQ[timerQ_len] = (struct TimedMsg){0};
        if (timerQ_len) {
            size_t pos = 0;
            while (2 * pos + 1 < timerQ_len) {
                size_t child = 2 * pos + 1;
                if (child + 1 < timerQ_len && timed_before(timerQ[child + 1], timerQ[child]))
                    child++;
                if (!timed_before(timerQ[child], entry))
                    break;
                timerQ[pos] = timerQ[child];
                pos = child;
            }
            timerQ[pos] = entry;
        } else {
            timerQ_sequence = 0;
        }
        if (timerQ_capacity > 64 && timerQ_len <= timerQ_capacity / 4) {
            size_t capacity = timerQ_capacity / 2;
            // GC_realloc may not shrink the allocation.
            struct TimedMsg *queue = GC_malloc(capacity * sizeof(*queue));
            if (queue) {
                memcpy(queue, timerQ, timerQ_len * sizeof(*queue));
                GC_free(timerQ);
                timerQ = queue;
                timerQ_capacity = capacity;
            }
        }
    }
    spinlock_unlock(&timerQ_lock);
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////
char *RTAG_name($RTAG tag) {
    switch (tag) {
        case $RDONE: return "RDONE"; break;
        case $RFAIL: return "RFAIL"; break;
        case $RCONT: return "RCONT"; break;
        case $RWAIT: return "RWAIT"; break;
    }
}
////////////////////////////////////////////////////////////////////////////////////////
$R $DoneD___call__($Cont $this, $WORD val) {
    return $R_DONE(val);
}

bool $DoneD___bool__($Cont self) {
  return true;
}

B_str $DoneD___str__($Cont self) {
  return $FORMAT("<$Done object at %p>", self);
}

void $Done__serialize__($Cont self, $Serial$state state) {
  return;
}

$Cont $Done__deserialize__($Cont self, $Serial$state state) {
  $Cont res = $DNEW($Cont,state);
  res->$class = &$DoneG_methods;
  return res;
}

struct $ContG_class $DoneG_methods = {
    "$Done",
    UNASSIGNED,
    NULL,
    $ContD___init__,
    $Done__serialize__,
    $Done__deserialize__,
    $DoneD___bool__,
    $DoneD___str__,
    $DoneD___str__,
    $DoneD___call__
};
struct $Cont $Done$instance = {
    &$DoneG_methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $FailD___call__($Cont $this, $WORD ex) {
    return $R_FAIL(ex);
}

bool $FailD___bool__($Cont self) {
  return true;
}

B_str $FailD___str__($Cont self) {
  return $FORMAT("<$Fail object at %p>", self);
}

void $Fail__serialize__($Cont self, $Serial$state state) {
  return;
}

$Cont $Fail__deserialize__($Cont self, $Serial$state state) {
  $Cont res = $DNEW($Cont,state);
  res->$class = &$FailG_methods;
  return res;
}

struct $ContG_class $FailG_methods = {
    "$Fail",
    UNASSIGNED,
    NULL,
    $ContD___init__,
    $Fail__serialize__,
    $Fail__deserialize__,
    $FailD___bool__,
    $FailD___str__,
    $FailD___str__,
    $FailD___call__
};
struct $Cont $Fail$instance = {
    &$FailG_methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $InitRootD___call__ ($Cont $this, $WORD val) {
    typedef $R(*ROOT__init__t)($Actor, $Cont, B_Env);    // Assumed type of the ROOT actor's __init__ method
    return ((ROOT__init__t)root_actor->$class->__init__)(root_actor, ($Cont)val, env_actor);
}

struct $ContG_class $InitRootG_methods = {
    "$InitRoot",
    UNASSIGNED,
    NULL,
    $ContD___init__,
    $ContD___serialize__,
    $ContD___deserialize__,
    $ContD___bool__,
    $ContD___str__,
    $ContD___str__,
    $InitRootD___call__
};
struct $Cont $InitRoot$cont = {
    &$InitRootG_methods
};
////////////////////////////////////////////////////////////////////////////////////////

#ifdef ACTON_DB
void dummy_callback(queue_callback_args * qca) { }
void queue_group_message_callback(queue_callback_args * qca) {
//    rtsd_printf("   # There are messages in actor queues for group %d, subscriber %d, status %d\n", (int) qca->group_id, (int) qca->consumer_id, qca->status);
}

void create_db_queue(int64_t key) {
    int minority_status = 0;
    while(!rts_exit) {
        int ret = remote_create_queue_in_txn(MSG_QUEUE, ($WORD)key, &minority_status, NULL, db);
        rtsd_printf("#### Create queue %" PRId64 " returns %d", key, ret);
        if(ret == NO_QUORUM_ERR) {
            sleep(3);
            continue;
        }
        if(ret == 0 || ret == CLIENT_ERR_SUBSCRIPTION_EXISTS)
            break;
    }
}

void init_db_queue(int64_t key) {
    if (db)
        create_db_queue(key);
}

void register_actor(int64_t key) {
    if (db) {
        int status = add_actor_to_membership(key, db);
        assert(status == 0);
    }
}
#endif

void PUSH_outgoing($Actor self, B_Msg m) {
    m->$next = self->$outgoing;
    self->$outgoing = m;
}

void PUSH_catcher($Actor a, $Catcher c) {
    c->$next = a->$catcher;
    a->$catcher = c;
}

$Catcher POP_catcher($Actor a) {
    $Catcher c = a->$catcher;
    if (c) {
        a->$catcher = c->$next;
        c->$next = NULL;
    }
    return c;
}

B_Msg $ASYNC($Actor to, $Cont cont) {
    $Actor self = GET_SELF();
    time_t baseline = 0;
    B_Msg m = B_MsgG_newXX(to, cont, baseline, &$Done$instance);
    if (self) {                                         // $ASYNC called by actor code
        m->$baseline = self->B_Msg->$baseline;
        PUSH_outgoing(self, m);
    } else {                                            // $ASYNC called by the event loop
        m->$baseline = current_time();
        if (ENQ_msg(m, to)) {
           int wtid = ENQ_ready(to);
           wake_wt(wtid);
        }
    }
    return m;
}

B_Msg $AFTER(B_float sec, $Cont cont) {
    $Actor self = GET_SELF();
    rtsd_printf("# AFTER by %" PRId64, self->$globkey);
    time_t baseline = self->B_Msg->$baseline + sec->val * 1000000;
    B_Msg m = B_MsgG_newXX(self, cont, baseline, &$Done$instance);
    PUSH_outgoing(self, m);
    return m;
}

// Like $AFTER, but the delay counts from the current time instead of from the
// baseline of the message being handled
B_Msg $AFTER_NOW(B_float sec, $Cont cont) {
    $Actor self = GET_SELF();
    rtsd_printf("# AFTER_NOW by %" PRId64, self->$globkey);
    time_t baseline = current_time() + sec->val * 1000000;
    B_Msg m = B_MsgG_newXX(self, cont, baseline, &$Done$instance);
    PUSH_outgoing(self, m);
    return m;
}

$R $AWAIT($Cont cont, B_Msg m) {
    return $R_WAIT(cont, m);
}

$R $PUSH_C($Cont cont) {
    $Actor self = GET_SELF();
    $Catcher c = $NEW($Catcher, cont);
    PUSH_catcher(self, c);
    return $R_CONT(cont, B_True);                   // True indicates the "try" branch
}

B_BaseException $POP_C() {
    $Actor self = GET_SELF();
    $Catcher c = POP_catcher(self);
    B_BaseException ex = c->xval;
    return ex;
}

void $DROP_C() {
    $Actor self = GET_SELF();
    POP_catcher(self);
}

JumpBuf $PUSH_BUF() {
    WorkerCtx wctx = GET_WCTX();
    assert(wctx != NULL);
    JumpBuf current = wctx->jump_top;
    JumpBuf new = (JumpBuf)GC_malloc(sizeof(struct JumpBuf));
    new->prev = current;
    wctx->jump_top = new;
    return new;
}

B_BaseException $POP() {
    WorkerCtx wctx = GET_WCTX();
    assert(wctx != NULL);
    JumpBuf current = wctx->jump_top;
    assert(current != NULL);
    //    assert(current->prev != NULL);
    wctx->jump_top = current->prev;
    return current->xval;
}

void $DROP() {
    WorkerCtx wctx = GET_WCTX();
    assert(wctx != NULL);
    JumpBuf current = wctx->jump_top;
    assert(current != NULL);
    //   (current->prev != NULL);
    wctx->jump_top = current->prev;
}

void $RAISE(B_BaseException e) {
    WorkerCtx wctx = GET_WCTX();
    JumpBuf jump = wctx->jump_top;
    jump->xval = e;
    longjmp(jump->buf, 1);
}

#ifdef ACTON_DB
void create_all_actor_queues() {
    for(snode_t * node = HEAD(db->actors); node!=NULL; node=NEXT(node)) {
        create_db_queue((long) node->key);
    }
}

int handle_status_and_schema_mismatch(int ret, int minority_status, int64_t key)
{
    // If schema on any of the DB servers needs updating (based on minority_status), do that.
    // If there was a quorum of healthy servers, we can go on after this, the operation succeeded.
    // If schema was missing on a majority of servers, we'll in addition get NO_QUORUM_ERR, and
    // we also need to retry the operation.
    int queues_created = 0;
    switch(minority_status) {
        case DB_ERR_NO_QUEUE:
        case DB_ERR_NO_CONSUMER:
        case VAL_STATUS_ABORT_SCHEMA: {
            // Schema errs:
//            create_all_actor_queues();
            create_db_queue(key);
            queues_created = 1;
            break;
        }
        case QUEUE_STATUS_READ_INCOMPLETE:
        case QUEUE_STATUS_READ_COMPLETE:
        case DB_ERR_DUPLICATE_CONSUMER:
        case DB_ERR_QUEUE_COMPLETE:
        case DB_ERR_DUPLICATE_QUEUE: {
            // These are OK:
            break;
        }
        default: { // DB_ERR_QUEUE_HEAD_INVALID, DB_ERR_NO_TABLE
            assert(0);
        }
    }

    if(ret == VAL_STATUS_ABORT_SCHEMA && !queues_created) {
//        create_all_actor_queues();
        create_db_queue(key);
    }

    if(ret == NO_QUORUM_ERR) {
        sleep(3);
        return 1;
    }

    return 0;
}
#endif

void reverse_outgoing_queue($Actor self) {
    B_Msg prev = NULL;
    B_Msg m = self->$outgoing;
    while (m) {
        B_Msg next = m->$next;
        m->$next = prev;
        prev = m;
        m = next;
    }
    self->$outgoing = prev;
}

#ifdef ACTON_DB
// Send all buffered messages of the sender to global DB queues in a single txn, and retry it until success
// Leaves no side effects in local queues if txns need to abort
// Assumes the actor's outgoing queue has already been reversed in FIFO order
void FLUSH_outgoing_db($Actor self, uuid_t *txnid) {
    rtsd_printf("#### FLUSH_outgoing messages from %" PRId64 " to DB queues", self->$globkey);
    B_Msg m = self->$outgoing;
    while (m) {
        int64_t dest = (m->$baseline == self->B_Msg->$baseline)? m->$to->$globkey : 0;
        int ret = 0, minority_status = 0;
        while(!rts_exit) {
            ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)dest, &minority_status, txnid, db);
            if (dest) {
                    rtsd_printf("   # enqueue msg %" PRId64 " to queue %" PRId64 " returns %d, minority_status=%d", m->$globkey, dest, ret, minority_status);
            } else {
                    rtsd_printf("   # enqueue msg %" PRId64 " to TIMER_QUEUE returns %d, minority_status=%d", m->$globkey, ret, minority_status);
            }
            if(!handle_status_and_schema_mismatch(ret, minority_status, dest))
                break;
        }
        m = m->$next;
    }
}
#endif

// Actually send all buffered messages of the sender, using internal queues only
// Assumes the actor's outgoing queue has already been reversed in FIFO order
void FLUSH_outgoing_local($Actor self) {
    rtsd_printf("#### FLUSH_outgoing messages from %" PRId64 " to RTS-internal queues", self->$globkey);
    B_Msg m = self->$outgoing;
    self->$outgoing = NULL;
    while (m) {
        B_Msg next = m->$next;
        m->$next = NULL;
        int64_t dest;
        if (m->$baseline == self->B_Msg->$baseline) {
            $Actor to = m->$to;
            if (ENQ_msg(m, to)) {
                ENQ_ready(to);
            }
            dest = to->$globkey;
        } else {
            if (ENQ_timed(m))
                reset_timeout();
            dest = 0;
        }
        m = next;
    }
}

// Get the due time of the first timed message. Returns false if there is
// none.
bool next_timeout(time_t *due) {
    spinlock_lock(&timerQ_lock);
    bool found = timerQ_len > 0;
    if (found)
        *due = timerQ[0].msg->$baseline;
    spinlock_unlock(&timerQ_lock);
    return found;
}

// Deliver the first timed message if it is due at time now. Returns
// whether there was one.
bool handle_timeout(time_t now) {
    B_Msg m = DEQ_timed(now);
    if (m) {
        rtsd_printf("## Dequeued timed msg with baseline %ld (now is %ld)", m->$baseline, now);
        if (ENQ_msg(m, m->$to)) {
            int wtid = ENQ_ready(m->$to);
            wake_wt(wtid);
        }
#ifdef ACTON_DB
        if (db) {
                int success = 0;
                while(!success && !rts_exit)
                {
                uuid_t *txnid = remote_new_txn(db);
                if(txnid == NULL)
                    continue;
                timer_consume_hd++;

                int64_t key = TIMER_QUEUE;
                snode_t *m_start, *m_end;
                int entries_read = 0, minority_status = 0;
                int64_t read_head = -1;

                int ret0 = remote_read_queue_in_txn(($WORD)db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, &minority_status, NULL, db);
                rtsd_printf("   # dummy read msg from TIMER_QUEUE returns %d, entries read: %d", ret0, entries_read);
                if(handle_status_and_schema_mismatch(ret0, minority_status, key))
                    continue;

                int ret1 = remote_consume_queue_in_txn(($WORD)db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key, read_head, &minority_status, txnid, db);
                rtsd_printf("   # consume msg %" PRId64 " from TIMER_QUEUE returns %d", m->$globkey, ret1);
                if(handle_status_and_schema_mismatch(ret1, minority_status, key))
                    continue;

                int ret2 = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)m->$to->$globkey, &minority_status, txnid, db);
                rtsd_printf("   # (timed) enqueue msg %" PRId64 " to queue %" PRId64 " returns %d", m->$globkey, m->$to->$globkey, ret2);
                if(handle_status_and_schema_mismatch(ret2, minority_status, key))
                    continue;

                int ret3 = remote_commit_txn(txnid, &minority_status, db);
                rtsd_printf("############## Commit returned %d, minority_status %d", ret3, minority_status);
                if(handle_status_and_schema_mismatch(ret3, minority_status, key))
                    continue;
                if(ret3 == VAL_STATUS_COMMIT)
                    success = 1;
                }
        }
#endif
    }
    return m != NULL;
}

////////////////////////////////////////////////////////////////////////////////////////

B_dict globdict = NULL;

$WORD try_globdict($WORD w) {
    int64_t key = (int64_t)w;
    $WORD obj = B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, toB_int(key), NULL);
    return obj;
}

#ifdef ACTON_DB
int64_t read_queued_msg(int64_t key, int64_t *read_head) {
    snode_t *m_start, *m_end;
    int entries_read = 0, minority_status = 0, ret = 0;
    
    while(!rts_exit) {
        ret = remote_read_queue_in_txn(($WORD)db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key,
                                           1, &entries_read, read_head, &m_start, &m_end, &minority_status, NULL, db);
        rtsd_printf("   # read msg from queue %" PRId64 " returns %d, entries read: %d, minority_status: %d", key, ret, entries_read, minority_status);
        if(!handle_status_and_schema_mismatch(ret, minority_status, key))
            break;
    }

    if (!entries_read)
        return 0;
    db_row_t *r = (db_row_t*)m_start->value;
    rtsd_printf("# r %p, key: %" PRId64 ", cells: %p, columns: %p, no_cols: %d, blobsize: %d", r, (int64_t)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
    return (int64_t)r->column_array[0];
}
#endif

typedef struct BlobHd {           // C.f. $ROW
    int class_id;
    int blob_size;
} BlobHd;

$ROW extract_row($WORD *blob, size_t blob_size) {
    int words_left = blob_size / sizeof($WORD);
    if (words_left == 0)
        return NULL;
    BlobHd* head = (BlobHd*)blob;
    $ROW fst = GC_malloc(sizeof(struct $ROW) + head->blob_size*sizeof($WORD));
    $ROW row = fst;
    while (!rts_exit) {
        long size = 1 + head->blob_size;
        memcpy(&row->class_id, blob, size*sizeof($WORD));
        blob += size;
        words_left -= size;
        if (words_left == 0)
            break;
        head = (BlobHd*)blob;
        row->next = GC_malloc(sizeof(struct $ROW) + head->blob_size*sizeof($WORD));
        row = row->next;
    };
    row->next = NULL;
    return fst;
}

void print_rows($ROW row) {
    int n = 0;
    while (row) {
        char b[1024];
        int len = 0;
        for (int i = 0; i < row->blob_size; i++)
            len += sprintf(b+len, "%ld ", (long)row->blob[i]);
        sprintf(b+len, ".");

        rtsd_printf("--- %2d: class_id %6d, blob_size: %3d, blob: %s", n, row->class_id, row->blob_size, b);
        n++;
        row = row->next;
    }
}

void print_msg(B_Msg m) {
    rtsd_printf("==== Message %p", m);
    rtsd_printf("     next: %p", m->$next);
    rtsd_printf("     to: %p", m->$to);
    rtsd_printf("     cont: %p", m->$cont);
    rtsd_printf("     waiting: %p", m->$waiting);
    rtsd_printf("     baseline: %ld", m->$baseline);
    rtsd_printf("     value: %p", m->value);
    rtsd_printf("     globkey: %" PRId64, m->$globkey);
}

void print_actor($Actor a) {
    rtsd_printf("==== Actor %p", a);
    rtsd_printf("     next: %p", a->$next);
    rtsd_printf("     msg: %p", a->B_Msg);
    rtsd_printf("     outgoing: %p", a->$outgoing);
    rtsd_printf("     waitsfor: %p", a->$waitsfor);
    rtsd_printf("     consume_hd: %ld", (long)a->$consume_hd);
    rtsd_printf("     catcher: %p", a->$catcher);
    rtsd_printf("     globkey: %" PRId64, a->$globkey);
}

#ifdef ACTON_DB
void deserialize_system(snode_t *actors_start) {
    rtsd_printf("Deserializing system");
    queue_callback * gqc = get_queue_callback(queue_group_message_callback);
    int ret = 0,  minority_status = 0, no_items = 0;
    rtsd_printf("### remote_subscribe_group(consumer_id = %d, group_id = %d)\n", (int) db->local_rts_id, (int) db->local_rts_id);
    while(!rts_exit) {
        ret = remote_subscribe_group((WORD) db->local_rts_id, NULL, NULL, (WORD) db->local_rts_id, gqc, &minority_status, db);
        if(!handle_status_and_schema_mismatch(ret, minority_status, 0))
            break;
    }
    snode_t *msgs_start, *msgs_end;
    while(!rts_exit) {
        ret = remote_read_full_table_in_txn(&msgs_start, &msgs_end, MSGS_TABLE, &no_items, &minority_status, NULL, db);
        if(!handle_status_and_schema_mismatch(ret, minority_status, 0))
            break;
    }
    
    globdict = $NEW(B_dict,(B_Hashable)B_HashableD_intG_witness,NULL,NULL);

    int64_t min_key = 0;

    rtsd_printf("#### Msg allocation:");
    for(snode_t * node = msgs_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        rtsd_printf("# r %p, key: %" PRId64 ", cells: %p, columns: %p, no_cols: %d, blobsize: %d", r, (int64_t)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
        int64_t key = (int64_t)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            rtsd_printf("# r2 %p, key: %" PRId64 ", cells: %p, columns: %p, no_cols: %d, blobsize: %d", r2, (int64_t)r2->key, r2->cells, r2->column_array, r2->no_columns, r2->last_blob_size);
            BlobHd *head = (BlobHd*)r2->column_array[0];
            B_Msg msg = (B_Msg)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            msg->$globkey = key;
            B_dictD_setitem(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), msg);
            rtsd_printf("# Allocated Msg %p = %" PRId64 " of class %s = %d", msg, msg->$globkey, msg->$class->$GCINFO, msg->$class->$class_id);
            if (key < min_key)
                min_key = key;
        }
    }
    rtsd_printf("#### Actor allocation:");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        rtsd_printf("# r %p, key: %" PRId64 ", cells: %p, columns: %p, no_cols: %d, blobsize: %d", r, (int64_t)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
        int64_t key = (int64_t)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            rtsd_printf("# r2 %p, key: %" PRId64 ", cells: %p, columns: %p, no_cols: %d, blobsize: %d", r2, (int64_t)r2->key, r2->cells, r2->column_array, r2->no_columns, r2->last_blob_size);
            BlobHd *head = (BlobHd*)r2->column_array[0];
            $Actor act = ($Actor)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            act->$globkey = key;
            B_dictD_setitem(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), act);
            rtsd_printf("# Allocated Actor %p = %" PRId64 " of class %s = %d", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            if (key < min_key)
                min_key = key;
        }
        register_actor(key);
    }
    start_keys(min_key <= FIRST_KEY ? min_key - 1 : FIRST_KEY);

    rtsd_printf("#### Msg contents:");
    for(snode_t * node = msgs_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        int64_t key = (int64_t)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            $WORD *blob = ($WORD*)r2->column_array[0];
            int blob_size = r2->last_blob_size;
            $ROW row = extract_row(blob, blob_size);
            B_Msg msg = (B_Msg)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), NULL);
            rtsd_printf("####### Deserializing msg %p = %" PRId64 " of class %s = %d", msg, msg->$globkey, msg->$class->$GCINFO, msg->$class->$class_id);
            print_rows(row);
            $glob_deserialize(($Serializable)msg, row, try_globdict);
            print_msg(msg);
        }
    }

    rtsd_printf("#### Actor contents:");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        int64_t key = (int64_t)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            $WORD *blob = ($WORD*)r2->column_array[0];
            int blob_size = r2->last_blob_size;
            $ROW row = extract_row(blob, blob_size);
            $Actor act = ($Actor)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), NULL);
            rtsd_printf("####### Deserializing actor %p = %" PRId64 " of class %s = %d", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            print_rows(row);
            $glob_deserialize(($Serializable)act, row, try_globdict);

            B_Msg m = act->$waitsfor;
            if (m && !FROZEN(m)) {
                ADD_waiting(act, m);
                rtsd_printf("# Adding Actor %" PRId64 " to wait for Msg %" PRId64, act->$globkey, m->$globkey);
            }
            else {
                act->$waitsfor = NULL;
            }

            rtsd_printf("#### Reading msgs queue %" PRId64 " contents:", key);
            int64_t prev_read_head = -1; //, prev_consume_head = -1;
            int ret = 0, minority_status = 0;
            while (!rts_exit) {
                    int64_t msg_key = read_queued_msg(key, &prev_read_head);
                if (!msg_key)
                    break;
                m = B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(msg_key), NULL);
                rtsd_printf("# Adding Msg %" PRId64 " to Actor %" PRId64, m->$globkey, act->$globkey);
                ENQ_msg(m, act);
            }
            if (act->B_Msg && !act->$waitsfor) {
                ENQ_ready(act);
                rtsd_printf("# Adding Actor %" PRId64 " to the readyQ", act->$globkey);
            }
            print_actor(act);
        }
    }

    rtsd_printf("#### Actor resume:");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        int64_t key = (int64_t)r->key;
        $Actor act = ($Actor)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), NULL);
        rtsd_printf("####### Resuming actor %p = %" PRId64 " of class %s = %d", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
        act->$class->__resume__(act);
    }

    rtsd_printf("#### Reading timer queue contents:");
    time_t now = current_time();
    int64_t prev_read_head = -1;
    while(!rts_exit) {
        int64_t msg_key = read_queued_msg(TIMER_QUEUE, &prev_read_head);
        if (!msg_key)
            break;
        B_Msg m = B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(msg_key), NULL);
        if (m->$baseline < now)
            m->$baseline = now;
        rtsd_printf("# Adding Msg %" PRId64 " to the timerQ", m->$globkey);
        ENQ_timed(m);
    }

    env_actor  = (B_Env)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(ENV_KEY), NULL);
    root_actor = ($Actor)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(ROOT_KEY), NULL);
    globdict = NULL;
    rtsd_printf("System deserialized");
}
#endif

$WORD try_globkey($WORD obj) {
    $SerializableG_class c = (($Serializable)obj)->$class;
    if (c->$class_id == MSG_ID) {
        int64_t key = ((B_Msg)obj)->$globkey;
        return ($WORD)key;
    } else if (c->$class_id == ACTOR_ID || c->$superclass && c->$superclass->$class_id == ACTOR_ID) {
        int64_t key = (($Actor)obj)->$globkey;
        return ($WORD)key;
    }
    return 0;
}

long $total_rowsize($ROW r) {           // In words
    long size = 0;
    while (r) {
        size += 1 + r->blob_size;       // Two ints == one $WORD
        r = r->next;
    }
    return size;
}

#ifdef ACTON_DB
void insert_row(int64_t key, size_t total, $ROW row, $WORD table, uuid_t *txnid) {
    $WORD column[2] = {($WORD)key, 0};
    $WORD blob[total];
    $WORD *p = blob;
    int row_no = 0;
    while (row) {
        //printf("   # row %d: class %d, blob_size %d\n", row_no, row->class_id, row->blob_size);
        long size = 1 + row->blob_size;
        memcpy(p, &row->class_id, size*sizeof($WORD));
        row_no++;
        p += size;
        row = row->next;
    }
    BlobHd *end = (BlobHd*)p;

    char b[1024];
    int len = 0;
    for (int i = 0; i < total; i++)
        len += sprintf(b+len, "%lu ", (unsigned long)blob[i]);
    sprintf(b+len, ".");
    rtsd_printf("## Built blob, size: %ld, blob: %s", total, b);

    //printf("\n## Sanity check extract row:\n");
    //$ROW row1 = extract_row(blob, total*sizeof($WORD));
    //print_rows(row1);
    int ret = 0, minority_status = 0;
    while(!rts_exit) {
        ret = remote_insert_in_txn(column, 2, 1, 1, blob, total*sizeof($WORD), table, &minority_status, txnid, db);
        rtsd_printf("   # insert to table %ld, row %" PRId64 ", returns %d", (long)table, key, ret);
        if(!handle_status_and_schema_mismatch(ret, minority_status, 0))
            break;
    }
}

void serialize_msg(B_Msg m, uuid_t *txnid) {
    rtsd_printf("#### Serializing Msg %" PRId64, m->$globkey);
    $ROW row = $glob_serialize(($Serializable)m, try_globkey);
    print_rows(row);
    insert_row(m->$globkey, $total_rowsize(row), row, MSGS_TABLE, txnid);
}

void serialize_actor($Actor a, uuid_t *txnid) {
    rtsd_printf("#### Serializing Actor %" PRId64, a->$globkey);
    $ROW row = $glob_serialize(($Serializable)a, try_globkey);
    print_rows(row);
    insert_row(a->$globkey, $total_rowsize(row), row, ACTORS_TABLE, txnid);

    B_Msg out = a->$outgoing;
    while (out) {
        serialize_msg(out, txnid);
        out = out->$next;
    }
}
#endif

void serialize_state_shortcut($Actor a) {
#ifdef ACTON_DB
    if (db) {
            int success = 0, ret = 0, minority_status = 0;
            while(!success && !rts_exit) {
            uuid_t * txnid = remote_new_txn(db);
            if(txnid == NULL)
                continue;
            serialize_actor(a, txnid);
            ret = remote_commit_txn(txnid, &minority_status, db);
            rtsd_printf("############## Commit returned %d, minority_status %d", ret, minority_status);
            if(handle_status_and_schema_mismatch(ret, minority_status, a->$globkey))
                continue;
            if(ret == VAL_STATUS_COMMIT)
                success = 1;
            }
    }
#endif
}

void BOOTSTRAP(int argc, char *argv[]) {
    B_list args = B_listG_new(NULL,NULL);
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    for (int i=0; i< argc; i++)
        wit->$class->append(wit,args,actStrFromCStringCopy(argv[i]));

    // env and root get the fixed keys ENV_KEY and ROOT_KEY. B_EnvG_newactor()
    // and $ROOT() take one key each, for the actor they create, so the actor
    // gets this worker's next key.
    WorkerCtx wctx = get_wctx();
    int64_t key_next = wctx->key_next;
    wctx->key_next = ENV_KEY;
    env_actor = B_EnvG_newactor(B_WorldCapG_new(), B_SysCapG_new(), args);
    env_actor->nr_wthreads = num_wthreads;

    wctx->key_next = ROOT_KEY;
    root_actor = $ROOT();                           // Assumed to return $NEWACTOR(X) for the selected root actor X
    wctx->key_next = key_next;
    time_t now = current_time();
    B_Msg m = B_MsgG_newXX(root_actor, &$InitRoot$cont, now, &$Done$instance);
#ifdef ACTON_DB
    if (db) {
            int ret = 0, minority_status = 0;
            while(!rts_exit) {
                ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)root_actor->$globkey, &minority_status, NULL, db);
                rtsd_printf("   # enqueue bootstrap msg %" PRId64 " to root actor queue %" PRId64 " returns %d, minority_status %d", m->$globkey, root_actor->$globkey, ret, minority_status);
                if(!handle_status_and_schema_mismatch(ret, minority_status, root_actor->$globkey))
                    break;
            }
    }
#endif
    if (ENQ_msg(m, root_actor)) {
        ENQ_ready(root_actor);
    }

}

void save_actor_state($Actor current, B_Msg m) {
#ifdef ACTON_DB
            if (db) {
                int success = 0;
                reverse_outgoing_queue(current);
                while(!success && !rts_exit) {
                    uuid_t * txnid = remote_new_txn(db);
                    if(txnid == NULL)
                        continue;
                    current->$consume_hd++;
                    serialize_actor(current, txnid);
                    FLUSH_outgoing_db(current, txnid);
                    serialize_msg(current->B_Msg, txnid);

                    int64_t key = current->$globkey;
                    snode_t *m_start, *m_end;
                    int entries_read = 0, minority_status = 0;
                    int64_t read_head = -1;

                    int ret0 = remote_read_queue_in_txn(($WORD) db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, &minority_status, NULL, db);
                    rtsd_printf("   # dummy read msg from queue %" PRId64 " returns %d, entries read: %d", key, ret0, entries_read);
                    if(handle_status_and_schema_mismatch(ret0, minority_status, key))
                        continue;

                    int ret1 = remote_consume_queue_in_txn(($WORD) db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key, read_head, &minority_status, txnid, db);
                    rtsd_printf("   # consume msg %" PRId64 " from queue %" PRId64 " returns %d", m->$globkey, key, ret1);
                    if(handle_status_and_schema_mismatch(ret1, minority_status, key))
                        continue;

                    int ret2 = remote_commit_txn(txnid, &minority_status, db);
                    rtsd_printf("############## Commit returned %d, minority_status %d", ret2, minority_status);
                    if(handle_status_and_schema_mismatch(ret2, minority_status, key))
                        continue;
                    if(ret2 == VAL_STATUS_COMMIT)
                        success = 1;
                }
                FLUSH_outgoing_local(current);
            } else {
#endif
                reverse_outgoing_queue(current);
                FLUSH_outgoing_local(current);
#ifdef ACTON_DB
            }
#endif
}

////////////////////////////////////////////////////////////////////////////////////////

void main_stop_cb(uv_async_t *ev) {
    uv_stop(uv_loops[0]);
}


// Timer for the timed message queue, on thread 0's event loop.
//
// The queue's first message is due at a time in microseconds on the
// runtime's clock, current_time(). Thread 0 delivers a message only when its
// due time is at or before current_time() (handle_timeout()), so no message
// is delivered before its due time on that clock. The timer counts on the
// same clock, so it wakes thread 0 when the first message is due.
//
// libuv's timers count whole milliseconds: waiting for a message less than a
// millisecond away would take a busy loop, and rounding would make it up to a
// millisecond late. On Linux and macOS a kernel timer wakes the event loop
// instead; the loop polls the timer's file descriptor, a timerfd or a kqueue
// holding one EVFILT_TIMER. init_clock() creates the timerfd on the runtime's
// clock, and timer_set() sets it to the due time. A kqueue timer set to an
// absolute time in microseconds waits for a wall clock time, so timer_set()
// sets the kqueue timer to the time left until the due time instead, which
// the kernel counts on mach continuous time, the runtime's clock. Elsewhere a
// libuv timer is set to the time left, rounded up to whole milliseconds,
// which libuv counts on the runtime's clock. It fires when the platform's
// wait returns, which on Windows can take a scheduler tick, so timers there
// are late by up to that.
//
// CLOCK_BOOTTIME and mach continuous time keep counting while the system
// sleeps, so a timer that comes due during a sleep fires when the system
// wakes. Where the runtime's clock stops while the system sleeps, as
// CLOCK_MONOTONIC does on Linux, a timer that is pending during a sleep fires
// later by as long as the system slept.
#if defined(__linux__)
#include <sys/timerfd.h>
#define KERNEL_TIMER
#elif defined(__APPLE__)
#include <sys/event.h>
#define KERNEL_TIMER
#endif

void arm_timer_ev();
void check_uv_fatal(int status, char msg[]);

// Deliver every timed message that is due, then set the timer for the next
static void timer_fire(void) {
    time_t now = current_time();
    while (handle_timeout(now))
        ;
    arm_timer_ev();
}

#ifdef KERNEL_TIMER
static int timer_fd = -1;
static uv_poll_t timer_poll;

static void timer_poll_cb(uv_poll_t *handle, int status, int events) {
#if defined(__linux__)
    uint64_t expirations;
    while (read(timer_fd, &expirations, sizeof(expirations)) > 0)
        ;
#else
    struct kevent kev;
    struct timespec zero = {0, 0};
    while (kevent(timer_fd, NULL, 0, &kev, 1, &zero) > 0)
        ;
#endif
    timer_fire();
}

static void timer_init(uv_loop_t *loop) {
    // On Linux, init_clock() has created the timerfd
#if defined(__APPLE__)
    timer_fd = kqueue();
    if (timer_fd < 0) {
        log_fatal("Unable to create timer: %s", strerror(errno));
        exit(1);
    }
#endif
    check_uv_fatal(uv_poll_init(loop, &timer_poll, timer_fd), "Error initializing timer poll: ");
    check_uv_fatal(uv_poll_start(&timer_poll, UV_READABLE, timer_poll_cb), "Error starting timer poll: ");
}

// Set the timer to fire at due (microseconds, at least 1). A due time in
// the past fires at once.
static void timer_set(time_t due) {
#if defined(__linux__)
    struct itimerspec its = {0};
    its.it_value.tv_sec = due / 1000000;
    its.it_value.tv_nsec = (due % 1000000) * 1000;
    if (timerfd_settime(timer_fd, TFD_TIMER_ABSTIME, &its, NULL) != 0) {
        log_fatal("Unable to set timer: %s", strerror(errno));
        exit(1);
    }
#else
    // The time left, or 1 microsecond for a due time in the past
    time_t now = current_time();
    time_t wait_us = due > now ? due - now : 1;
    // The kernel rejects a wait that overflows in nanoseconds
    if (wait_us > INT64_MAX / 1000)
        wait_us = INT64_MAX / 1000;
    struct kevent kev;
    // NOTE_CRITICAL: coalesce as little as possible with other timers.
    // NOTE_MACH_CONTINUOUS_TIME: count the wait on mach continuous time, the
    // runtime's clock, which keeps counting while the system sleeps.
    EV_SET(&kev, 1, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
           NOTE_USECONDS | NOTE_CRITICAL | NOTE_MACH_CONTINUOUS_TIME, wait_us, NULL);
    int r;
    while ((r = kevent(timer_fd, &kev, 1, NULL, 0, NULL)) != 0 && errno == EINTR)
        ;
    if (r != 0) {
        log_fatal("Unable to set timer: %s", strerror(errno));
        exit(1);
    }
#endif
}

static void timer_stop(void) {
#if defined(__linux__)
    // An all-zero value disarms
    struct itimerspec its = {0};
    if (timerfd_settime(timer_fd, 0, &its, NULL) != 0) {
        log_fatal("Unable to stop timer: %s", strerror(errno));
        exit(1);
    }
#else
    struct kevent kev;
    EV_SET(&kev, 1, EVFILT_TIMER, EV_DELETE, 0, 0, NULL);
    // Deleting a timer that has already fired fails with ENOENT, harmlessly
    kevent(timer_fd, &kev, 1, NULL, 0, NULL);
#endif
}
#else
static uv_timer_t timer_ev;

static void timer_cb(uv_timer_t *ev) {
    timer_fire();
}

static void timer_init(uv_loop_t *loop) {
    check_uv_fatal(uv_timer_init(loop, &timer_ev), "Error initializing timer: ");
}

// Set the timer to fire at due (microseconds, at least 1)
static void timer_set(time_t due) {
    // Round up to whole milliseconds, the resolution of libuv timers. libuv
    // counts the timeout from its cached loop time, so update that first.
    uv_update_time(timer_ev.loop);
    long long int offset = (due - current_time() + 999) / 1000;
    if (offset < 0)
        offset = 0;
    check_uv_fatal(uv_timer_start(&timer_ev, timer_cb, offset, 0), "Unable to set timer: ");
}

static void timer_stop(void) {
    uv_timer_stop(&timer_ev);
}
#endif

// Choose the runtime's clock (current_time()). On Linux it must be a clock
// that a timerfd can count on, so this also creates the timerfd. Called at the
// start of main(), before anything reads current_time().
static void init_clock(void) {
#if defined(__linux__)
    // Kernels that do not support CLOCK_BOOTTIME for a timerfd reject it
    // with EINVAL
    timer_fd = timerfd_create(CLOCK_BOOTTIME, TFD_NONBLOCK | TFD_CLOEXEC);
    if (timer_fd < 0 && errno == EINVAL) {
        rts_clock = CLOCK_MONOTONIC;
        timer_fd = timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK | TFD_CLOEXEC);
    }
    if (timer_fd < 0) {
        log_fatal("Unable to create timer: %s", strerror(errno));
        exit(1);
    }
#elif defined(__APPLE__)
    mach_timebase_info(&rts_timebase);
#endif
}

void main_wake_cb(uv_async_t *ev) {
    // Wäjky-päjky
    arm_timer_ev();
}

void arm_timer_ev() {
    time_t due;
    if (!next_timeout(&due))
        timer_stop();
    else
        // A baseline can be before the clock's zero, which the timerfd
        // rejects; any time in the past fires at once
        timer_set(due < 1 ? 1 : due);
}

void wt_stop_cb(uv_async_t *ev) {
    WorkerCtx wctx = GET_WCTX();
    uv_stop((uv_loop_t *)wctx->uv_loop);
}

void wt_wake_cb(uv_async_t *ev) {
    // We just wake up the uv loop here if it is blocked waiting for IO, real
    // work is run later when wt_work_cb is called as part of the "check" phase.
}

// When a worker takes an actor from a ready queue, it keeps running the
// actor's continuations while the actor has work: up to KEEP_CONTS of them,
// or until KEEP_NS has passed, whichever comes first. Then the actor goes to
// the end of the queue if another actor is waiting. If no other actor is
// waiting, the worker keeps running it.
#define KEEP_CONTS 8
#define KEEP_NS (10 * 1000)

// The worker loop reads the time before and after each continuation, so a
// read must be cheap. Where we can, we read the CPU's own counter instead of
// calling uv_clock_gettime(): CNTVCT_EL0 on aarch64, and the TSC on x86_64
// when the CPU says that it ticks at a constant rate. Both are timers, not
// cycle counters. They tick at a fixed rate whatever the frequency of the
// core, and all cores of a chip, performance and efficiency cores alike, read
// the same count, so the time does not jump when a worker moves to another
// core. On a machine with several x86 sockets whose TSCs are not in step, a
// worker that moves to another socket does see the time jump by the
// difference. That only affects the statistics and the decisions to keep
// running an actor around that move. counter_mult converts counter ticks to
// nanoseconds: ns = ticks * counter_mult / 2^32. It is 0 when there is no
// such counter. counter_name is the clock we read, for --rts-verbose.
static uint64_t counter_mult = 0;
static const char *counter_name = "uv_clock_gettime()";

#if defined(__aarch64__) || defined(__x86_64__)
static inline uint64_t read_counter(void) {
#if defined(__aarch64__)
    uint64_t ticks;
    __asm__ volatile("mrs %0, cntvct_el0" : "=r"(ticks) :: "memory");
    return ticks;
#else
    return __builtin_ia32_rdtsc();
#endif
}
#endif

static inline long long int now_ns(void) {
#if defined(__aarch64__) || defined(__x86_64__)
    if (counter_mult)
        return ((unsigned __int128)read_counter() * counter_mult) >> 32;
#endif
    uv_timespec64_t ts;
    uv_clock_gettime(UV_CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000 + ts.tv_nsec;
}

// Sets counter_mult. Called once, before the workers start.
static void init_counter(void) {
#if defined(__aarch64__)
    uint64_t freq;
    __asm__ volatile("mrs %0, cntfrq_el0" : "=r"(freq));
    if (freq > 0) {
        counter_mult = ((uint64_t)1000000000 << 32) / freq;
        counter_name = "CNTVCT_EL0";
    }
#elif defined(__x86_64__)
    // CPUID leaf 0x80000007, EDX bit 8: the TSC is invariant, it ticks at a
    // constant rate through frequency changes and idle states. Without it,
    // the rate follows the core clock and we keep calling uv_clock_gettime().
    // The rate is not known up front, so we measure it against
    // CLOCK_MONOTONIC over 1 ms.
    unsigned int eax, ebx, ecx, edx;
    if (!__get_cpuid(0x80000007, &eax, &ebx, &ecx, &edx) || !(edx & (1 << 8)))
        return;
    long long int ns0 = now_ns(), ns1;
    uint64_t ticks0 = read_counter();
    while ((ns1 = now_ns()) - ns0 < 1000 * 1000)
        ;
    counter_mult = ((unsigned __int128)(ns1 - ns0) << 32) / (read_counter() - ticks0);
    counter_name = "TSC";
#endif
}

// Whether another actor is waiting in this worker's queue or in the shared
// queue. Worker 0 never takes actors from the shared queue, so for worker 0
// only its own queue counts.
static inline bool others_waiting(int wtid) {
    return rqs[wtid].head != NULL || (wtid != 0 && rqs[SHARED_RQ].head != NULL);
}

void wt_work_cb(uv_check_t *ev) {
    WorkerCtx wctx = (WorkerCtx)ev->data;
    assert(wctx->id >= 0 && wctx->id < 256);
    // current: the actor this worker runs. It stays set from one iteration
    // to the next while the worker keeps running the same actor.
    // taken_ns: when the worker took that actor from a queue
    // conts: how many of its continuations the worker has run since
    volatile $Actor current = NULL;
    volatile long long int taken_ns = 0;
    volatile int conts = 0;

    // We read the time before and after each continuation. The time between
    // continuations, and from the start of this call to the first one, is
    // bookkeeping. Like the variables above, these are volatile so that they
    // keep their values when a continuation raises and we longjmp back to
    // $PUSH().
    volatile long long int start_ns = now_ns();
    volatile long long int end_ns = start_ns;
    while (true) {
        maybe_sync_pause();
        if (rts_exit) {
            return;
        }
        bool continued = current != NULL;
        if (!continued)
            current = DEQ_ready(wctx->id);
        if (!current) {
            // Both queues looked empty, so we are about to return to the
            // event loop and sleep until another thread wakes us. A thread
            // that puts an actor on the shared queue then calls
            // wake_wt(SHARED_RQ), which wakes a worker only if it reads that
            // worker's wt_stats[].state as WT_Idle. We stored WT_Idle after
            // our previous continuation, but DEQ_ready() reads the queue
            // heads without taking the queue locks, so without a fence the
            // CPU may read them before our WT_Idle store is visible to other
            // threads. An enqueuer could then read our state as WT_Working
            // and wake no one, while we read its queue as empty and sleep,
            // leaving its actor queued.
            // This fence makes our WT_Idle store visible to all threads
            // before we read the queues again, and wake_wt() has the
            // matching fence between the enqueue and its reads of the
            // worker states. With both fences, at least one side sees the
            // other's store: we find the actor here, or the enqueuer reads
            // WT_Idle and wakes us. A worker that finds an actor on its
            // first look skips the fence.
            atomic_thread_fence(memory_order_seq_cst);
            current = DEQ_ready(wctx->id);
            if (!current)
                return;
        }
        if (!continued) {
            // Putting an actor on the queue wakes one idle worker, but
            // several actors queued in a row can all wake the same worker.
            // So when we take an actor and more are waiting, we wake another
            // worker. We mark ourselves as working first, so that this wake
            // goes to a sleeping worker and not to ourselves.
            wt_stats[wctx->id].state = WT_Working;
            wake_wt(SHARED_RQ);
        }

        SET_SELF(current);
        volatile B_Msg m = current->B_Msg;
        $Cont cont = m->$cont;
        $WORD val = m->value;

        long long int cont_start_ns = now_ns();
        if (!continued) {
            taken_ns = cont_start_ns;
            conts = 0;
        }
        {
            long long int diff = cont_start_ns - end_ns;
            wt_stats[wctx->id].bkeep_count++;
            wt_stats[wctx->id].bkeep_sum += diff;
            if      (diff < 100)              { wt_stats[wctx->id].bkeep_100ns++; }
            else if (diff < 1   * 1000)       { wt_stats[wctx->id].bkeep_1us++; }
            else if (diff < 10  * 1000)       { wt_stats[wctx->id].bkeep_10us++; }
            else if (diff < 100 * 1000)       { wt_stats[wctx->id].bkeep_100us++; }
            else if (diff < 1   * 1000000)    { wt_stats[wctx->id].bkeep_1ms++; }
            else if (diff < 10  * 1000000)    { wt_stats[wctx->id].bkeep_10ms++; }
            else if (diff < 100 * 1000000)    { wt_stats[wctx->id].bkeep_100ms++; }
            else if (diff < 1   * 1000000000) { wt_stats[wctx->id].bkeep_1s++; }
            else if (diff < (long long int)10  * 1000000000) { wt_stats[wctx->id].bkeep_10s++; }
            else if (diff < (long long int)100 * 1000000000) { wt_stats[wctx->id].bkeep_100s++; }
            else                              { wt_stats[wctx->id].bkeep_inf++; }
        }

        $R r;
        if (wctx->jump0 || $PUSH()) {                         // Normal path
            if (!wctx->jump0) {
                wctx->jump0 = wctx->jump_top;
            }
            rtsd_printf("## Running actor %" PRId64 " : %s", current->$globkey, current->$class->$GCINFO);
            r = cont->$class->__call__(cont, val);

            end_ns = now_ns();
            long long int diff = end_ns - cont_start_ns;

            wt_stats[wctx->id].conts_count++;
            wt_stats[wctx->id].conts_sum += diff;

            if      (diff < 100)              { wt_stats[wctx->id].conts_100ns++; }
            else if (diff < 1   * 1000)       { wt_stats[wctx->id].conts_1us++; }
            else if (diff < 10  * 1000)       { wt_stats[wctx->id].conts_10us++; }
            else if (diff < 100 * 1000)       { wt_stats[wctx->id].conts_100us++; }
            else if (diff < 1   * 1000000)    { wt_stats[wctx->id].conts_1ms++; }
            else if (diff < 10  * 1000000)    { wt_stats[wctx->id].conts_10ms++; }
            else if (diff < 100 * 1000000)    { wt_stats[wctx->id].conts_100ms++; }
            else if (diff < 1   * 1000000000) { wt_stats[wctx->id].conts_1s++; }
            else if (diff < (long long int)10  * 1000000000) { wt_stats[wctx->id].conts_10s++; }
            else if (diff < (long long int)100 * 1000000000) { wt_stats[wctx->id].conts_100s++; }
            else                              { wt_stats[wctx->id].conts_inf++; }
        } else {                                        // Exceptional path
            end_ns = now_ns();
            assert(wctx->jump0 != NULL);
            assert(wctx->jump0->xval != NULL);
            B_BaseException ex = wctx->jump0->xval;
            rtsd_printf("## (%d) Actor %" PRId64 " : %s longjmp exception: %s", wctx->id, current->$globkey, current->$class->$GCINFO, ex->$class->$GCINFO);
            r = $R_FAIL(ex);
        }

        bool more = false;             // the actor still has work
        switch (r.tag) {
        case $RDONE: {
            save_actor_state(current, m);
            m->value = r.value;                             // m->value holds the message result,
            $Actor b = FREEZE_waiting(m, MARK_RESULT);      // so mark this and stop further m->waiting additions
            while (b) {
                b->B_Msg->value = r.value;
                b->$waitsfor = NULL;
                $Actor c = b->$next;
                ENQ_ready(b);
                rtsd_printf("## Waking up actor %" PRId64 " : %s", b->$globkey, b->$class->$GCINFO);
                b = c;
            }
            rtsd_printf("## DONE actor %" PRId64 " : %s", current->$globkey, current->$class->$GCINFO);
            if (DEQ_msg(current)) {
                more = true;
            }
            break;
        }
        case $RCONT: {
            m->$cont = r.cont;
            m->value = r.value;
            rtsd_printf("## CONT actor %" PRId64 " : %s", current->$globkey, current->$class->$GCINFO);
            more = true;
            break;
        }
        case $RFAIL: {
            $Catcher c = current->$catcher;
            if (c) {                            // Normal exception handling
                c->xval = (B_BaseException)r.value;
                m->$cont = c->$cont;
                m->value = B_False;             // False signals the exceptional branch
                rtsd_printf("## FAIL/handle actor %" PRId64 " : %s", current->$globkey, current->$class->$GCINFO);
                more = true;
            } else {                            // An unhandled exception
                save_actor_state(current, m);
                B_BaseException ex = (B_BaseException)r.value;
                m->value = r.value;                                 // m->value holds the raised exception,
                $Actor b = FREEZE_waiting(m, MARK_EXCEPTION);       // so mark this and stop further m->waiting additions
                // If any other actor is waiting for our result / exception,
                // then we consider the exception handled and we can avoid
                // printing the exception both in the originating actor and in
                // the waiting actor. Thus we only print Unhandled exception in
                // the originating actor when there is no one waiting for us.
                if (!b)
                    fprintf(stderr, "Unhandled exception in actor: %s[%" PRId64 "]:\n  %s\n", unmangle_name(current->$class->$GCINFO), current->$globkey, fromB_str(ex->$class->__str__(ex)));
                while (b) {
                    b->B_Msg->$cont = &$Fail$instance;
                    b->B_Msg->value = r.value;
                    b->$waitsfor = NULL;
                    $Actor c = b->$next;
                    ENQ_ready(b);
                    rtsd_printf("## Propagating exception to actor %" PRId64 " : %s", b->$globkey, b->$class->$GCINFO);
                    b = c;
                }
                if (DEQ_msg(current)) {
                    more = true;
                }
                rtsd_printf("## Done handling failed actor %" PRId64 " : %s", current->$globkey, current->$class->$GCINFO);
            }
            break;
        }
        case $RWAIT: {
#ifdef ACTON_DB
            if (db) {
                int success = 0, ret = 0, minority_status = 0;
                reverse_outgoing_queue(current);
                while(!success && !rts_exit) {
                    uuid_t * txnid = remote_new_txn(db);
                    if(txnid == NULL)
                        continue;
                    serialize_actor(current, txnid);
                    FLUSH_outgoing_db(current, txnid);
                    serialize_msg(current->B_Msg, txnid);
                    ret = remote_commit_txn(txnid, &minority_status, db);
                    rtsd_printf("############## Commit returned %d, minority_status %d", ret, minority_status);
                    if(handle_status_and_schema_mismatch(ret, minority_status, current->$globkey))
                        continue;
                    if(ret == VAL_STATUS_COMMIT)
                        success = 1;
                }
            } else {
#endif
                reverse_outgoing_queue(current);
#ifdef ACTON_DB
            }
#endif
            m->$cont = r.cont;
            B_Msg x = (B_Msg)r.value;
            assert(x != NULL);

            // Send the actor's messages before it waits: once it is on x's
            // waiting list, x's completion can enqueue it and another worker
            // can run it, while flushing still reads its current message. If
            // x completes first, ADD_waiting sees that and the actor goes on.
            FLUSH_outgoing_local(current);
            bool added_waiting = ADD_waiting(current, x);

            if (added_waiting) {      // x->cont is a proper $Cont: x is still being processed so current was added to x->waiting
                rtsd_printf("## AWAIT actor %" PRId64 " : %s", current->$globkey, current->$class->$GCINFO);
            } else if (EXCEPTIONAL(x)) {        // x->cont == MARK_EXCEPTION: x->value holds the raised exception, current is not in x->waiting
                rtsd_printf("## AWAIT/fail actor %" PRId64 " : %s", current->$globkey, current->$class->$GCINFO);
                m->$cont = &$Fail$instance;
                m->value = x->value;
                more = true;
            } else {                            // x->cont == MARK_RESULT: x->value holds the final response, current is not in x->waiting
                rtsd_printf("## AWAIT/wakeup actor %" PRId64 " : %s", current->$globkey, current->$class->$GCINFO);
                m->value = x->value;
                more = true;
            }
            break;
        }
        }
        // If the actor still has work, run it again here instead of putting
        // it back on the queue. Putting it back would cost a push, a pop and
        // a wake of another worker. It would also usually move the actor to
        // another core, and make it wait behind every actor in the queue.
        // KEEP_CONTS and KEEP_NS limit how long it runs while other actors
        // wait. An actor that was pinned to another worker during this
        // continuation, for example by set_actor_affinity, goes to that
        // worker's queue instead.
        bool keep = false;
        if (more) {
            conts++;
            bool runs_here = current->$affinity == SHARED_RQ
                             || current->$affinity == wctx->id;
            keep = runs_here
                   && (!others_waiting(wctx->id)
                       || (conts < KEEP_CONTS && end_ns - taken_ns < KEEP_NS));
            if (!keep)
                ENQ_ready(current);
        }
        if (!keep)
            current = NULL;
        SET_SELF(NULL);

        // Stay marked as working while we keep running the same actor. We
        // will not take anything from a queue until we are done with it, so
        // wake_wt() should wake some other worker.
        if (!current)
            wt_stats[wctx->id].state = WT_Idle;

        // run for max 20ms before yielding to IO
        // NOTE: since we are not preemptive, a single long continuation can
        // exceed this cap
        if (end_ns - start_ns > 20*1000000) {
            if (current) {
                ENQ_ready(current);
                wt_stats[wctx->id].state = WT_Idle;
            }
            break;
        }
    }

    // if there's more work, wake up ourselves again to process more but
    // interleave with some IO
    uv_async_send(&wake_ev[wctx->id]);
}

void *main_loop(void *idx) {
    WorkerCtx wctx = GC_memalign(_Alignof(struct WorkerCtx), sizeof(struct WorkerCtx));
    wctxs[(long)idx] = wctx;
    wctx->id = (long)idx;
    wctx->uv_loop = uv_loops[wctx->id];
    wctx->jump_top = NULL;
    wctx->jump0 = NULL;
    start_worker_keys(wctx);
#ifdef ACTON_THREADS
    pthread_setspecific(pkey_wctx, (void *)wctx);
#endif

    char tname[11]; // Enough for "Worker XXX\0"
    snprintf(tname, sizeof(tname), "Worker %ld", wctx->id);
#ifdef ACTON_THREADS
#if defined(IS_MACOS)
    pthread_setname_np(tname);
#else
    pthread_setname_np(pthread_self(), tname);
#endif
#endif

    uv_check_init(wctx->uv_loop, &work_ev[wctx->id]);
    work_ev[wctx->id].data = wctx;
    uv_check_start(&work_ev[wctx->id], (uv_check_cb)wt_work_cb);

    wt_stats[wctx->id].state = WT_Idle;
    int r = uv_run(wctx->uv_loop, UV_RUN_DEFAULT);
    wt_stats[wctx->id].state = WT_NoExist;
    rtsd_printf("Exiting...");
    return NULL;
}

////////////////////////////////////////////////////////////////////////////////////////

void $register_rts () {
  $register_force(MSG_ID,&B_MsgG_methods);
  $register_force(ACTOR_ID,&$ActorG_methods);
  $register_force(CATCHER_ID,&$CatcherG_methods);
  $register_force(PROC_ID,&$procG_methods);
  $register_force(ACTION_ID,&$actionG_methods);
  $register_force(MUT_ID,&$mutG_methods);
  $register_force(PURE_ID,&$pureG_methods);
  $register_force(CONT_ID,&$ContG_methods);
  $register_force(DONE_ID,&$DoneG_methods);
  $register_force(CONSTCONT_ID,&$ConstContG_methods);
  $register(&$DoneG_methods);
  $register(&$InitRootG_methods);
  $register(&B_EnvG_methods);
  // $Fail$instance ends up in message $cont slots when an exception propagates
  // to a waiting actor ($RFAIL wake, and the await-on-failed-message path). It
  // must be registered like $Done, or serializing such a message emits no
  // header row for the $cont field and the blob misaligns on recovery.
  $register(&$FailG_methods);
}
 
////////////////////////////////////////////////////////////////////////////////////////

#ifdef ACTON_DB
void dbc_ops_stats_to_json(yyjson_mut_doc *doc, yyjson_mut_val *j_mpoint, struct dbc_ops_stat *ops_stat) {
    yyjson_mut_val *j_ops_stat = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, j_mpoint, ops_stat->name, j_ops_stat);

    yyjson_mut_obj_add_int(doc, j_ops_stat, "called",     ops_stat->called);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "completed",  ops_stat->completed);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "success",    ops_stat->success);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "error",      ops_stat->error);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "no_quorum",  ops_stat->no_quorum);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_sum",   ops_stat->time_sum);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_100ns", ops_stat->time_100ns);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_1us",   ops_stat->time_1us);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_10us",  ops_stat->time_10us);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_100us", ops_stat->time_100us);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_1ms",   ops_stat->time_1ms);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_10ms",  ops_stat->time_10ms);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_100ms", ops_stat->time_100ms);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_1s",    ops_stat->time_1s);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_10s",   ops_stat->time_10s);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_100s",  ops_stat->time_100s);
    yyjson_mut_obj_add_int(doc, j_ops_stat, "time_inf",   ops_stat->time_inf);

}
#endif


const char* stats_to_json () {
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    yyjson_mut_obj_add_str(doc, root, "name", appname);

    yyjson_mut_obj_add_int(doc, root, "pid", pid);

    uv_timespec64_t ts;
    if (uv_clock_gettime(UV_CLOCK_REALTIME, &ts) != 0) {
        log_fatal("Unable to get precise time");
        return NULL;
    }
    struct tm tm;
#ifdef _WIN32
    errno_t result = localtime_s(&tm, &ts.tv_sec);
    if (result != 0) {
        char errmsg[1024] = "Error getting time: ";
        uv_strerror_r(errno, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn("%s", errmsg);
        return NULL;
    }
#else
    time_t seconds = (time_t)ts.tv_sec;
    localtime_r(&seconds, &tm);
#endif
    char dt[32];    // = "YYYY-MM-ddTHH:mm:ss.SSS+0000";
    strftime(dt, 32, "%Y-%m-%dT%H:%M:%S.000%z", &tm);
    sprintf(dt + 20, "%03hu%s", (unsigned short)(ts.tv_nsec / 1000000), dt + 23);

    yyjson_mut_obj_add_str(doc, root, "datetime", dt);

    // Worker threads
    yyjson_mut_val *j_stat = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, root, "wt", j_stat);
    for (unsigned int i = 1; i < NUM_THREADS; i++) {
        yyjson_mut_val *j_wt = yyjson_mut_obj(doc);
        yyjson_mut_obj_add_val(doc, j_stat, wt_stats[i].key, j_wt);
        yyjson_mut_obj_add_str(doc, j_wt, "state",       WT_State_name[wt_stats[i].state]);
        yyjson_mut_obj_add_int(doc, j_wt, "sleeps",      wt_stats[i].sleeps);
        yyjson_mut_obj_add_int(doc, j_wt, "qlen",        rqs[i].count);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_count", wt_stats[i].conts_count);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_sum",   wt_stats[i].conts_sum);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_100ns", wt_stats[i].conts_100ns);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_1us",   wt_stats[i].conts_1us);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_10us",  wt_stats[i].conts_10us);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_100us", wt_stats[i].conts_100us);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_1ms",   wt_stats[i].conts_1ms);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_10ms",  wt_stats[i].conts_10ms);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_100ms", wt_stats[i].conts_100ms);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_1s",    wt_stats[i].conts_1s);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_10s",   wt_stats[i].conts_10s);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_100s",  wt_stats[i].conts_100s);
        yyjson_mut_obj_add_int(doc, j_wt, "conts_inf",   wt_stats[i].conts_inf);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_count", wt_stats[i].bkeep_count);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_sum",   wt_stats[i].bkeep_sum);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_100ns", wt_stats[i].bkeep_100ns);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_1us",   wt_stats[i].bkeep_1us);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_10us",  wt_stats[i].bkeep_10us);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_100us", wt_stats[i].bkeep_100us);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_1ms",   wt_stats[i].bkeep_1ms);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_10ms",  wt_stats[i].bkeep_10ms);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_100ms", wt_stats[i].bkeep_100ms);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_1s",    wt_stats[i].bkeep_1s);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_10s",   wt_stats[i].bkeep_10s);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_100s",  wt_stats[i].bkeep_100s);
        yyjson_mut_obj_add_int(doc, j_wt, "bkeep_inf",   wt_stats[i].bkeep_inf);
    }

    // Database
    yyjson_mut_val *j_dbc = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, root, "db_client", j_dbc);

#ifdef ACTON_DB
#define X(ops_name) \
    dbc_ops_stats_to_json(doc, j_dbc, dbc_stats.ops_name);
LIST_OF_DBC_OPS
#undef X
#endif

    const char *json = yyjson_mut_write(doc, 0, NULL);
    yyjson_mut_doc_free(doc);
    return json;
}

#ifdef ACTON_DB
const char* db_membership_to_json () {
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    yyjson_mut_obj_add_str(doc, root, "name", appname);

    yyjson_mut_obj_add_int(doc, root, "pid", pid);

    uv_timespec64_t ts;
    if (uv_clock_gettime(UV_CLOCK_REALTIME, &ts) != 0) {
        log_fatal("Unable to get precise time");
        return NULL;
    }
    struct tm tm;
    time_t seconds = (time_t)ts.tv_sec;
    localtime_r(&seconds, &tm);
    char dt[32];    // = "YYYY-MM-ddTHH:mm:ss.SSS+0000";
    strftime(dt, 32, "%Y-%m-%dT%H:%M:%S.000%z", &tm);
    sprintf(dt + 20, "%03hu%s", (unsigned short)(ts.tv_nsec / 1000000), dt + 23);

    yyjson_mut_obj_add_str(doc, root, "datetime", dt);

    // Actual topology
    yyjson_mut_val *j_nodes = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, root, "nodes", j_nodes);
    for (snode_t * crt = HEAD(db->servers); crt!=NULL; crt = NEXT(crt)) {
        remote_server * rs = (remote_server *) crt->value;

        yyjson_mut_val *j_node = yyjson_mut_obj(doc);
        yyjson_mut_obj_add_val(doc, j_nodes, rs->id, j_node);
        yyjson_mut_obj_add_str(doc, j_node, "type", "DDB");
        yyjson_mut_obj_add_str(doc, j_node, "hostname", rs->hostname);
        yyjson_mut_obj_add_str(doc, j_node, "status", RS_status_name[rs->status]);
    }
    for (snode_t * crt = HEAD(db->rtses); crt!=NULL; crt = NEXT(crt)) {
        rts_descriptor * nd = (rts_descriptor *) crt->value;

        yyjson_mut_val *j_node = yyjson_mut_obj(doc);
        yyjson_mut_obj_add_val(doc, j_nodes, nd->id, j_node);
        yyjson_mut_obj_add_str(doc, j_node, "type", "RTS");
        yyjson_mut_obj_add_str(doc, j_node, "hostname", nd->hostname);
        yyjson_mut_obj_add_str(doc, j_node, "status", RS_status_name[nd->status]);
        yyjson_mut_obj_add_int(doc, j_node, "local_rts_id", nd->local_rts_id);
        yyjson_mut_obj_add_int(doc, j_node, "dc_id", nd->dc_id);
        yyjson_mut_obj_add_int(doc, j_node, "rack_id", nd->rack_id);
    }

    const char *json = yyjson_mut_write(doc, 0, NULL);
    yyjson_mut_doc_free(doc);
    return json;
}
#endif

const char* actors_to_json () {
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    yyjson_mut_obj_add_str(doc, root, "name", appname);

    yyjson_mut_obj_add_int(doc, root, "pid", pid);

    uv_timespec64_t ts;
    if (uv_clock_gettime(UV_CLOCK_REALTIME, &ts) != 0) {
        log_fatal("Unable to get precise time");
        return NULL;
    }
    struct tm tm;
#ifdef _WIN32
    errno_t result = localtime_s(&tm, &ts.tv_sec);
    if (result != 0) {
        char errmsg[1024] = "Error getting time: ";
        uv_strerror_r(errno, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn("%s", errmsg);
        return NULL;
    }
#else
    time_t seconds = (time_t)ts.tv_sec;
    localtime_r(&seconds, &tm);
#endif
    char dt[32];    // = "YYYY-MM-ddTHH:mm:ss.SSS+0000";
    strftime(dt, 32, "%Y-%m-%dT%H:%M:%S.000%z", &tm);
    sprintf(dt + 20, "%03hu%s", (unsigned short)(ts.tv_nsec / 1000000), dt + 23);

    yyjson_mut_obj_add_str(doc, root, "datetime", dt);

    // Actual topology
    yyjson_mut_val *j_actors = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, root, "actors", j_actors);
#ifdef ACTON_DB
    // TODO: implement similar option but local only
    for(snode_t * crt = HEAD(db->actors); crt!=NULL; crt = NEXT(crt))
    {
        actor_descriptor * a = (actor_descriptor *) crt->value;

        char a_id_str[21]; // up to length of unsigned long long
        snprintf(a_id_str, 21, "%lld", (unsigned long long)a->actor_id);
        yyjson_mut_val *act_id = yyjson_mut_strcpy(doc, a_id_str);

        yyjson_mut_val *j_a = yyjson_mut_obj(doc);
        yyjson_mut_obj_put(j_actors, act_id, j_a);
        yyjson_mut_obj_add_str(doc, j_a, "rts", a->host_rts->id);
        yyjson_mut_obj_add_str(doc, j_a, "local", a->is_local?"yes":"no");
        yyjson_mut_obj_add_str(doc, j_a, "status", Actor_status_name[a->status]);
    }
#endif

    const char *json = yyjson_mut_write(doc, 0, NULL);
    yyjson_mut_doc_free(doc);
    return json;
}

#ifdef ACTON_THREADS
void *$mon_log_loop(void *period) {
    log_info("Starting monitor log, with %ld second(s) period, to: %s", (long)period, mon_log_path);

#if defined(IS_MACOS)
    pthread_setname_np("Monitor Log");
#else
    pthread_setname_np(pthread_self(), "Monitor Log");
#endif

    FILE *f;
    f = fopen(mon_log_path, "w");
    if (!f) {
        fprintf(stderr, "ERROR: Unable to open RTS monitor log file (%s) for writing\n", mon_log_path);
        exit(1);
    }

    while (1) {
        const char *json = stats_to_json();
        fputs(json, f);
        fputs("\n", f);
        if (rts_exit > 0) {
            log_info("Shutting down RTS Monitor log thread.");
            break;
        }

        pthread_mutex_lock(&rts_exit_lock);
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += (long)period;
        pthread_cond_timedwait(&rts_exit_signal, &rts_exit_lock, &ts);
        pthread_mutex_unlock(&rts_exit_lock);
    }
    fclose(f);
    return NULL;
}


void *$mon_socket_loop() {
    log_info("Starting monitor socket listen on %s", mon_socket_path);

#if defined(IS_MACOS)
    pthread_setname_np("Monitor Socket");
#else
    pthread_setname_np(pthread_self(), "Monitor Socket");
#endif

#ifdef _WIN32
    // TODO: implement on windows!?
#else
    int s, client_sock, len;
    struct sockaddr_un local, remote;
    char q[100];

    if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        fprintf(stderr, "ERROR: Unable to create Monitor Socket\n");
        exit(1);
    }

    local.sun_family = AF_UNIX;
    strcpy(local.sun_path, mon_socket_path);
    unlink(local.sun_path);
    len = sizeof(local.sun_path) + sizeof(local.sun_family);
    if (bind(s, (struct sockaddr *)&local, len) == -1) {
        fprintf(stderr, "ERROR: Unable to bind to Monitor Socket\n");
        exit(1);
    }

    if (listen(s, 5) == -1) {
        fprintf(stderr, "ERROR: Unable to listen on Monitor Socket\n");
        exit(1);
    }

    while (1) {
        socklen_t t = sizeof(remote);
        if ((client_sock = accept(s, (struct sockaddr *)&remote, &t)) == -1) {
            perror("accept");
            exit(1);
        }

        int n;
        char rbuf[64], *buf_base, *str;
        ssize_t bytes_read;
        size_t buf_used = 0, len;
        while (1) {
            bytes_read = recv(client_sock, &rbuf[buf_used], sizeof(rbuf) - buf_used, 0);
            if (bytes_read <= 0)
                break;
            buf_used += bytes_read;

            buf_base = rbuf;
            while (1) {
                if (buf_used == 0)
                    break;
                int r = netstring_read(&buf_base, &buf_used, &str, &len);
                if (r != 0) {
                    log_info("Mon socket: Error reading netstring: %d", r);
                    break;
                }

                if (memcmp(str, "actors", len) == 0) {
                    const char *json = actors_to_json();
                    char *send_buf = GC_malloc(strlen(json)+14); // 14 = maximum digits for length is 9 (999999999) + : + ; + \0
                    sprintf(send_buf, "%lu:%s,", strlen(json), json);
                    int send_res = send(client_sock, send_buf, strlen(send_buf), 0);
                    //free((void *)json);
                    //free((void *)send_buf);
                    if (send_res < 0) {
                        log_info("Mon socket: Error sending");
                        break;
                    }
                }

#ifdef ACTON_DB
                if (memcmp(str, "membership", len) == 0) {
                    const char *json = db_membership_to_json();
                    char *send_buf = GC_malloc(strlen(json)+14); // 14 = maximum digits for length is 9 (999999999) + : + ; + \0
                    sprintf(send_buf, "%lu:%s,", strlen(json), json);
                    int send_res = send(client_sock, send_buf, strlen(send_buf), 0);
                    //free((void *)json);
                    //free((void *)send_buf);
                    if (send_res < 0) {
                        log_info("Mon socket: Error sending");
                        break;
                    }
                }
#endif

                if (memcmp(str, "WTS", len) == 0) {
                    const char *json = stats_to_json();
                    char *send_buf = GC_malloc(strlen(json)+14); // 14 = maximum digits for length is 9 (999999999) + : + ; + \0
                    sprintf(send_buf, "%lu:%s,", strlen(json), json);
                    int send_res = send(client_sock, send_buf, strlen(send_buf), 0);
                    //free((void *)json);
                    //free((void *)send_buf);
                    if (send_res < 0) {
                        log_info("Mon socket: Error sending");
                        break;
                    }
                }
            }

            if (buf_base > rbuf && buf_used > 0)
                memmove(rbuf, buf_base, buf_used);
        }

        close(client_sock);
    }
    return NULL;
#endif
}
#endif

void rts_shutdown() {
#if defined(_WIN32) || defined(_WIN64)
#else
    tcsetattr(STDIN_FILENO, TCSANOW, &old_stdin_attr);
#endif

    rts_exit = 1;
    // 0 = main thread, rest is wthreads, thus +1
    for (int i = 0; i < NUM_THREADS; i++) {
        uv_async_send(&stop_ev[i]);
    }
}


#ifndef _WIN32
// Keep path resolution out of crash signal handlers.
static char crash_exe_path[512] = {0};
static void capture_exe_path() {
    size_t name_len = sizeof(crash_exe_path) - 1;
    if (uv_exepath(crash_exe_path, &name_len) != 0)
        name_len = 0;
    crash_exe_path[name_len] = 0;
}
static void get_exe_path(char *name_buf, size_t buf_size) {
    strncpy(name_buf, crash_exe_path, buf_size - 1);
    name_buf[buf_size - 1] = 0;
}

#ifdef __APPLE__
// In incremental and generational mode, the collector's mprotect dirty
// tracking makes other threads take write faults on protected heap pages
// while lldb is attached. debugserver takes over the task's EXC_BAD_ACCESS
// exception port when it attaches, so these faults reach lldb instead of
// the collector's handler. lldb then reports a crash, --one-line-on-crash
// exits before "thread backtrace all" runs, and on detach the fault is
// delivered as SIGBUS. This setting makes debugserver leave EXC_BAD_ACCESS
// to the task's own handler.
#define LLDB_IGNORE_BAD_ACCESS "settings set platform.plugin.darwin.ignored-exceptions EXC_BAD_ACCESS"

// Older lldb versions lack the setting, and lldb --batch stops at the first
// failing command, before it attaches. Try the command on its own first.
static bool lldb_can_ignore_bad_access() {
    int probe_pid = fork();
    if (probe_pid < 0)
        return false;
    if (!probe_pid) {
        int devnull = open("/dev/null", O_WRONLY);
        if (devnull >= 0) {
            dup2(devnull, 1);
            dup2(devnull, 2);
        }
        execlp("lldb", "lldb", "--batch", "-O", LLDB_IGNORE_BAD_ACCESS, NULL);
        _exit(127);
    }
    int status;
    if (waitpid(probe_pid, &status, 0) != probe_pid)
        return false;
    return WIFEXITED(status) && WEXITSTATUS(status) == 0;
}
#endif

void print_trace() {
    char pid_buf[30];
    sprintf(pid_buf, "%d", getpid());
    char name_buf[512];
    get_exe_path(name_buf, sizeof(name_buf));
#ifdef __linux__
    prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0);
#endif
    int child_pid = fork();
    if (!child_pid) {
        dup2(2, 1); // redirect output to stderr
#ifdef __APPLE__
        if (lldb_can_ignore_bad_access())
            execlp("lldb", "lldb", "-O", LLDB_IGNORE_BAD_ACCESS, "-p", pid_buf, "--batch", "-o", "thread backtrace all", "-o", "exit", "--one-line-on-crash", "exit", name_buf, NULL);
#endif
        execlp("lldb", "lldb", "-p", pid_buf, "--batch", "-o", "thread backtrace all", "-o", "exit", "--one-line-on-crash", "exit", name_buf, NULL);
        execlp("gdb", "gdb", "--quiet", "--batch", "-n", "-ex", "set confirm off", "-ex", "set pagination off", "-ex", "set debuginfod enabled off", "-ex", "thread", "-ex", "thread apply all backtrace full", name_buf, pid_buf, NULL);
        fprintf(stderr, "Unable to get detailed backtrace using lldb or gdb\n");
        _exit(127); /* If lldb/gdb failed to start */
    } else {
        waitpid(child_pid, NULL, 0);
    }
}

void launch_debugger(int signum) {
    fprintf(stderr, "\nERROR: This is the automatic debug launcher for %s\n", appname);
    if (signum == SIGILL)
        fprintf(stderr, "\nERROR: illegal instruction\n");
    if (signum == SIGSEGV)
        fprintf(stderr, "\nERROR: segmentation fault\n");
    fprintf(stderr, "Starting interactive debugger...\n");
    char pid_buf[30];
    sprintf(pid_buf, "%d", getpid());
    char name_buf[512];
    get_exe_path(name_buf, sizeof(name_buf));
#ifdef __linux__
    prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0);
#endif
    int child_pid = fork();
    if (!child_pid) {
        char findthread[40] = "thread find ";
        sprintf(findthread + strlen(findthread), "%p", (void *)pthread_self());
        execlp("gdb", "gdb", "--quiet", "-n", "-ex", "set confirm off", "-ex", findthread, name_buf, pid_buf, NULL);
        fprintf(stderr, "Unable to get detailed backtrace using lldb or gdb");
        _exit(127); /* If lldb/gdb failed to start */
    } else {
        waitpid(child_pid, NULL, 0);
        exit(0);
    }
}

void crash_handler(int signum) {
    fprintf(stderr, "\nERROR: This is the automatic crash handler for %s\n", appname);
    if (signum == SIGILL)
        fprintf(stderr, "ERROR: illegal instruction\n");
    if (signum == SIGSEGV)
        fprintf(stderr, "ERROR: segmentation fault\n");
    fprintf(stderr, "NOTE: this is likely a bug in acton, please report this at:\n");
    fprintf(stderr, "NOTE: https://github.com/actonlang/acton/issues/new\n");
    fprintf(stderr, "NOTE: include the backtrace printed below between -- 8< -- lines\n");

    fprintf(stderr, "\n-- 8< --------- BACKTRACE --------------------\n");
    print_trace();
    fprintf(stderr, "\n-- 8< --------- END BACKTRACE ----------------\n");

    if (signum == SIGILL)
        fprintf(stderr, "\nERROR: illegal instruction\n");
    if (signum == SIGSEGV)
        fprintf(stderr, "\nERROR: segmentation fault\n");
    fprintf(stderr, "NOTE: this is likely a bug in acton, please report this at:\n");
    fprintf(stderr, "NOTE: https://github.com/actonlang/acton/issues/new\n");
    fprintf(stderr, "NOTE: include the backtrace printed above between -- 8< -- lines\n");

    // Restore every crash handler before re-raising the original signal.
    sa_abrt.sa_handler = SIG_DFL;
    if (sigaction(SIGABRT, &sa_abrt, NULL) == -1) {
        log_fatal("Failed to restore signal handler for SIGABRT: %s", strerror(errno));
        exit(1);
    }
    sa_ill.sa_handler = SIG_DFL;
    if (sigaction(SIGILL, &sa_ill, NULL) == -1) {
        log_fatal("Failed to restore signal handler for SIGILL: %s", strerror(errno));
        exit(1);
    }
    sa_segv.sa_handler = SIG_DFL;
    if (sigaction(SIGSEGV, &sa_segv, NULL) == -1) {
        log_fatal("Failed to restore signal handler for SIGSEGV: %s", strerror(errno));
        exit(1);
    }
    // Kill ourselves with original signal sent to us
    kill(getpid(), signum);
}

void sigint_handler(int signum) {
    if (rts_exit == 0) {
        log_info("Received SIGINT, shutting down gracefully...");
        rts_shutdown();
    } else {
        log_info("Received SIGINT during graceful shutdown, exiting immediately");
        exit(return_val);
    }
}

void sigterm_handler(int signum) {
    if (rts_exit == 0) {
        log_info("Received SIGTERM, shutting down gracefully...");
        rts_shutdown();
    } else {
        log_info("Received SIGTERM during graceful shutdown, exiting immediately");
        exit(return_val);
    }
}
#endif

void check_uv_fatal(int status, char msg[]) {
    if (status == 0)
        return;

    char errmsg[1024];
    snprintf(errmsg, sizeof(errmsg), "%s", msg);
    uv_strerror_r(status, errmsg+strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
    log_fatal(errmsg);
    exit(1);
}


struct option {
    const char *name;
    const char *arg_name;
    int         val;
    const char *desc;
};


void print_help(struct option *opt) {
    printf("The Acton RTS reads and consumes the following options and arguments. All\n" \
           "other parameters are passed verbatim to the Acton application. Option\n" \
           "arguments can be passed either with --rts-option=ARG or --rts-option ARG\n\n");
    while (opt->name) {
        char optarg[64];

        sprintf(optarg, "%s%s%s", opt->name, opt->arg_name?"=":"", opt->arg_name?opt->arg_name:"");
        printf("  --%-30s  %s\n", optarg, opt->desc);
        opt++;
    }
    printf("\n");
    exit(0);
}

#ifdef ACTON_GC_DISABLE_THP
static void GC_CALLBACK gc_disable_thp(void *space, size_t size) {
    // Called with the GC allocator lock held, including failed allocations.
    if (space && madvise(space, size, MADV_NOHUGEPAGE) != 0) {
        static const char message[] = "Acton RTS: gc_disable_thp: MADV_NOHUGEPAGE failed\n";
        (void)write(STDERR_FILENO, message, sizeof(message) - 1);
        _exit(1);
    }
}
#endif

void DaveNull () {}

int main(int argc, char **argv) {
    rts_perf_init();
    init_counter();
    init_clock();
    // Init garbage collector and suppress warnings
#ifdef ACTON_GC_DISABLE_THP
    GC_set_on_os_get_mem(gc_disable_thp);
#endif
    GC_INIT();
#if ACTON_GC_REQUIRED_VDB != 0
    if (getenv("GC_ENABLE_INCREMENTAL") != NULL
            && GC_get_actual_vdb() != ACTON_GC_REQUIRED_VDB) {
        fprintf(stderr, "Acton RTS: requested GC dirty tracking backend %s is unavailable\n",
                ACTON_GC_DIRTY_TRACKING_BACKEND);
        exit(1);
    }
#endif
    GC_set_warn_proc(DaveNull);
    acton_init_alloc();
    acton_replace_allocator(GC_malloc, GC_malloc_atomic, GC_realloc, GC_calloc, acton_noop_free, GC_strdup, GC_strndup);
    int ddb_no_host = 0;
    char **ddb_host = NULL;
    char *rts_host = "localhost";
    int ddb_port = 32000;
    int ddb_replication = 3;
    int rts_node_id = -1;
    int rts_rack_id = -1;
    int rts_dc_id = -1;
    int new_argc = argc;
    int cpu_pin;
    uv_cpu_info_t* cpu_infos;
    int num_cores;
    if (uv_cpu_info(&cpu_infos, &num_cores) != 0) {
        log_fatal("Unable to get CPU info");
        exit(1);
    }
    uv_free_cpu_info(cpu_infos, num_cores);
    bool mon_on_exit = false;
    bool auto_backtrace = true;
    bool interactive_backtrace = false;
    char *log_path = NULL;
    FILE *logf = NULL;
    bool log_stderr = false;

    appname = argv[0];
    pid = getpid();

#ifndef _WIN32
    // Do line buffered output
    setlinebuf(stdout);

    // Signal handling
    sigfillset(&sa_abrt.sa_mask);
    sigfillset(&sa_ill.sa_mask);
    sigfillset(&sa_int.sa_mask);
    sigfillset(&sa_pipe.sa_mask);
    sigfillset(&sa_segv.sa_mask);
    sigfillset(&sa_term.sa_mask);
    sa_abrt.sa_flags = SA_RESTART;
    sa_ill.sa_flags = SA_RESTART;
    sa_int.sa_flags = SA_RESTART;
    sa_pipe.sa_flags = SA_RESTART;
    sa_segv.sa_flags = SA_RESTART;
    sa_term.sa_flags = SA_RESTART;

    // Ignore SIGPIPE, like we get if the other end talking to us on the Monitor
    // socket (which is a Unix domain socket) goes away.
    sa_pipe.sa_handler = SIG_IGN;
    // Handle signals
    sa_int.sa_handler = &sigint_handler;
    sa_term.sa_handler = &sigterm_handler;

    if (sigaction(SIGPIPE, &sa_pipe, NULL) == -1) {
        log_fatal("Failed to install signal handler for SIGPIPE: %s", strerror(errno));
        exit(1);
    }
    if (sigaction(SIGINT, &sa_int, NULL) == -1) {
        log_fatal("Failed to install signal handler for SIGINT: %s", strerror(errno));
        exit(1);
    }
    if (sigaction(SIGTERM, &sa_term, NULL) == -1) {
        log_fatal("Failed to install signal handler for SIGTERM: %s", strerror(errno));
        exit(1);
    }
#endif

#ifdef ACTON_THREADS
    pthread_key_create(&self_key, NULL);
    pthread_setspecific(self_key, NULL);
    pthread_key_create(&pkey_wctx, NULL);
#else
    self_actor = NULL;
#endif

    log_set_quiet(true);
    /*
     * A note on argument parsing: The RTS has its own command line arguments,
     * all prefixed with --rts-, which we need to parse out. The remainder of
     * the arguments should be passed on to the Acton program, thus we need to
     * fiddle with argv. To avoid modifying argv in place, we create a new argc
     * and argv which we bootstrap the Acton program with. The special -- means
     * to stop scanning for options, and any argument following it will be
     * passed verbatim.
     * For example (note the duplicate --rts-verbose)
     *   Command line    : ./app foo --rts-verbose --bar --rts-verbose
     *   Application sees: [./app, foo, --bar]
     * Using -- to pass verbatim arguments:
     *   Command line    : ./app foo --rts-verbose --bar -- --rts-verbose
     *   Application sees: [./app, foo, --bar, --, --rts-verbose]
     *
     * We support both styles of providing an option argument, e.g.:
     *    ./app --rts-wthreads 8
     *    ./app --rts-wthreads=8
     * Optional arguments aren't supported, an option either takes a required
     * argument or it does not.
     */
    static struct option long_options[] = {
        {"rts-bt-dbg", NULL, 'x', "Interactively debug on SIGILL / SIGSEGV"},
        {"rts-debug", NULL, 'd', "RTS debug, requires program to be compiled with --optimize Debug"},
        {"rts-ddb-host", "HOST", 'h', "DDB hostname"},
        {"rts-ddb-port", "PORT", 'p', "DDB port [32000]"},
        {"rts-ddb-replication", "FACTOR", 'r', "DDB replication factor [3]"},
        {"rts-node-id", "ID", 'i', "RTS node ID"},
        {"rts-rack-id", "RACK", 'R', "RTS rack ID"},
        {"rts-dc-id", "DATACENTER", 'D', "RTS datacenter ID"},
        {"rts-host", "RTSHOST", 'N', "RTS hostname"},
        {"rts-help", NULL, 'H', "Show this help"},
        {"rts-mon-log-path", "PATH", 'l', "Path to RTS mon stats log"},
        {"rts-mon-log-period", "PERIOD", 'k', "Periodicity of writing RTS mon stats log entry"},
        {"rts-mon-on-exit", NULL, 'E', "Print RTS mon stats to stdout on exit"},
        {"rts-mon-socket-path", "PATH", 'm', "Path to unix socket to expose RTS mon stats"},
        {"rts-no-bt", NULL, 'B', "Disable automatic backtrace"},
        {"rts-log-path", "PATH", 'L', "Path to RTS log"},
        {"rts-log-stderr", NULL, 's', "Log to stderr in addition to log file"},
        {"rts-verbose", NULL, 'v', "Enable verbose RTS output"},
        {"rts-wthreads", "COUNT", 'w', "Number of worker threads [#CPU cores]"},
        {NULL, 0, 0}
    };
    // length of long_options array
    #define OPTLEN (sizeof(long_options) / sizeof(long_options[0]) - 1)

    int ch = 0;
    // where we map current (i) argc position into new_argc
    int new_argc_dst = 0;
    // stop scanning once we've seen '--', passing the rest verbatim
    int opt_scan = 1;
    char **new_argv = acton_malloc((argc+1) * sizeof *new_argv);
    char *optarg = NULL;
    for (int i = 0; i < argc; i++) {
        ch = 0;
        optarg = NULL;
        if (strcmp(argv[i], "--") == 0) opt_scan = 0;
        if (opt_scan) {
            for (int j=0; j<OPTLEN; j++) {
                if (strlen(argv[i]) > 2
                    && strncmp(argv[i]+2, long_options[j].name, strlen(long_options[j].name)) == 0) {
                    // argv[i] matches one of our options!
                    ch = long_options[j].val;
                    new_argc--;
                    if (long_options[j].arg_name) {
                        if (strlen(argv[i]) > 2+strlen(long_options[j].name)
                            && argv[i][2+strlen(long_options[j].name)] == '=') {
                            // option argument is in --opt=arg style, so dig out
                            optarg = (char *)argv[i]+(2+strlen(long_options[j].name)+1);
                        } else {
                            // argument has to be next in argv
                            if (i+1 == argc) { // check we are not at end
                                fprintf(stderr, "ERROR: --%s requires an argument.\n", long_options[j].name);
                                exit(1);
                            }
                            i++;
                            optarg = argv[i];
                            new_argc--;
                        }
                    }
                    break;
                }
            }
        }
        if (!ch) { // Didn't identify one of our options, so pass through
            new_argv[new_argc_dst++] = argv[i];
            continue;
        }

        switch (ch) {
            case 'B':
                auto_backtrace = false;
                break;
            case 'd':
                #ifndef DEV
                fprintf(stderr, "ERROR: RTS debug not supported.\n");
                fprintf(stderr, "HINT: Recompile this program using: acton --optimize Debug ...\n");
                exit(1);
                #endif
                log_set_quiet(false);
                if (log_get_level() > LOG_DEBUG)
                    log_set_level(LOG_DEBUG);
                rts_debug = 1;
                // Enabling rts debug implies verbose RTS output too
                rts_verbose = 10;
                break;
            case 'E':
                mon_on_exit = true;
                break;
            case 'H':
                print_help(long_options);
                break;
            case 'h':
                ddb_host = acton_realloc(ddb_host, ++ddb_no_host * sizeof *ddb_host);
                ddb_host[ddb_no_host-1] = optarg;
                break;
            case 'k':
                mon_log_period = atoi(optarg);
                break;
            case 'L':
                log_path = optarg;
                break;
            case 'l':
                mon_log_path = optarg;
                break;
            case 'm':
                mon_socket_path = optarg;
                break;
            case 'p':
                ddb_port = atoi(optarg);
                break;
            case 'r':
                ddb_replication = atoi(optarg);
                break;
            case 'i':
                rts_node_id = atoi(optarg);
                break;
            case 'R':
                rts_rack_id = atoi(optarg);
                break;
            case 'D':
                rts_dc_id = atoi(optarg);
                break;
            case 'N':
                rts_host = acton_strdup(optarg);
                break;
            case 's':
                log_stderr = true;
                break;
            case 'v':
                if (log_get_level() > LOG_INFO)
                    log_set_level(LOG_INFO);
                rts_verbose = 1;
                break;
            case 'w':
                num_wthreads = atoi(optarg);
                break;
            case 'x':
                interactive_backtrace = true;
                break;
        }
    }
    new_argv[new_argc] = NULL;

#ifndef _WIN32
    capture_exe_path();
    if (interactive_backtrace) {
        sa_abrt.sa_handler = &launch_debugger;
        sa_ill.sa_handler = &launch_debugger;
        sa_segv.sa_handler = &launch_debugger;
    } else {
        sa_abrt.sa_handler = &crash_handler;
        sa_ill.sa_handler = &crash_handler;
        sa_segv.sa_handler = &crash_handler;
    }
#endif

    if (auto_backtrace) {
#ifndef _WIN32
        if (sigaction(SIGABRT, &sa_abrt, NULL) == -1) {
            log_fatal("Failed to install signal handler for SIGABRT: %s", strerror(errno));
            exit(1);
        }
        if (sigaction(SIGILL, &sa_ill, NULL) == -1) {
            log_fatal("Failed to install signal handler for SIGILL: %s", strerror(errno));
            exit(1);
        }
        if (sigaction(SIGSEGV, &sa_segv, NULL) == -1) {
            log_fatal("Failed to install signal handler for SIGSEGV: %s", strerror(errno));
            exit(1);
        }
#endif
    }

    if (log_path)
        log_set_quiet(true);
    if (rts_verbose || log_stderr)
        log_set_quiet(false);

    if (log_path) {
        logf = fopen(log_path, "w");
        if (!logf) {
            fprintf(stderr, "ERROR: Unable to open RTS log file (%s) for writing\n", log_path);
            exit(1);
        }
        log_add_fp(logf, LOG_TRACE);
    }

#ifdef ACTON_THREADS
    if (num_wthreads >= MAX_WTHREADS) {
        fprintf(stderr, "ERROR: Maximum of %d worker threads supported.\n", MAX_WTHREADS - 1);
        fprintf(stderr, "HINT: Run this program with fewer worker threads: %s --rts-wthreads %d\n", argv[0], MAX_WTHREADS - 1);
        exit(1);
    }
    // Determine number of worker threads, normally 1:1 per CPU thread / core
    // For low core count systems we do a minimum of 4 worker threads
    if (num_wthreads == -1 && num_cores < 4) { // auto, few CPU cores, so use 4 worker threads
        num_wthreads = 4;
        cpu_pin = 0;
        log_info("Detected %ld CPUs: Using %ld worker threads, due to low CPU count. No CPU affinity used.", num_cores, num_wthreads);
    } else if (num_wthreads == -1) { // auto, many CPU cores, use 1 worker thread per CPU core
        num_wthreads = num_cores;
        cpu_pin = 1;
        log_info("Detected %ld CPUs: Using %ld worker threads for 1:1 mapping with CPU affinity set.", num_cores, num_wthreads);
    } else {
        cpu_pin = 0;
        log_info("Detected %ld CPUs: Using %ld worker threads (manually set). No CPU affinity used.", num_cores, num_wthreads);
    }
#else
    log_info("Running without threads, main thread will perform all work");
    if (num_wthreads != -1) {
        fprintf(stderr, "ERROR: Threads disabled, provided --rts-wthreads argument has no effect.\n");
        fprintf(stderr, "HINT: You cannot compile with --no-threads and use --rts-wthreads at run time.\n");
        exit(1);
    }
    num_wthreads = 0;
#endif
    if (counter_mult)
        log_info("Worker clock: %s, %.1f MHz", counter_name, 4294967296e3 / counter_mult);
    else
        log_info("Worker clock: %s", counter_name);
    // Zeroize statistics
    for (int i=0; i < MAX_WTHREADS; i++) {
        wt_stats[i].idx = i;
        sprintf(wt_stats[i].key, "%d", i);
        wt_stats[i].state = 0;
        wt_stats[i].sleeps = 0;

        wt_stats[i].conts_count = 0;
        wt_stats[i].conts_sum = 0;
        wt_stats[i].conts_100ns = 0;
        wt_stats[i].conts_1us = 0;
        wt_stats[i].conts_10us = 0;
        wt_stats[i].conts_100us = 0;
        wt_stats[i].conts_1ms = 0;
        wt_stats[i].conts_10ms = 0;
        wt_stats[i].conts_100ms = 0;
        wt_stats[i].conts_1s = 0;
        wt_stats[i].conts_10s = 0;
        wt_stats[i].conts_100s = 0;
        wt_stats[i].conts_inf = 0;

        wt_stats[i].bkeep_count = 0;
        wt_stats[i].bkeep_sum = 0;
        wt_stats[i].bkeep_100ns = 0;
        wt_stats[i].bkeep_1us = 0;
        wt_stats[i].bkeep_10us = 0;
        wt_stats[i].bkeep_100us = 0;
        wt_stats[i].bkeep_1ms = 0;
        wt_stats[i].bkeep_10ms = 0;
        wt_stats[i].bkeep_100ms = 0;
        wt_stats[i].bkeep_1s = 0;
        wt_stats[i].bkeep_10s = 0;
        wt_stats[i].bkeep_100s = 0;
        wt_stats[i].bkeep_inf = 0;
    }
#ifdef ACTON_DB
    init_dbc_stats();
#endif
    wctxs[0] = NULL;

    for (int i=0; i <= num_wthreads; i++) {
        uv_loop_t *loop = GC_malloc(sizeof(uv_loop_t));
        check_uv_fatal(uv_loop_init(loop), "Error initializing libuv loop: ");
        uv_loops[i] = loop;

        if (i == 0) {
            check_uv_fatal(uv_async_init(uv_loops[i], &stop_ev[i], main_stop_cb), "Error initializing libuv stop event: ");
            check_uv_fatal(uv_async_init(uv_loops[i], &wake_ev[i], main_wake_cb), "Error initializing libuv wake event: ");
            check_uv_fatal(uv_async_send(&wake_ev[i]), "Error sending initial work event: ");
        } else {
            check_uv_fatal(uv_async_init(uv_loops[i], &stop_ev[i], wt_stop_cb), "Error initializing libuv stop event: ");
            check_uv_fatal(uv_async_init(uv_loops[i], &wake_ev[i], wt_wake_cb), "Error initializing libuv wake event: ");
            check_uv_fatal(uv_async_send(&wake_ev[i]), "Error sending initial work event: ");
        }
    }
    aux_uv_loop = uv_loops[0];

    for (int i=0; i < NUM_RQS; i++) {
        rqs[i].head = NULL;
        rqs[i].tail = NULL;
        rqs[i].count = 0;
    }

#if defined(_WIN32) || defined(_WIN64)
#else
    tcgetattr(STDIN_FILENO, &old_stdin_attr);
#endif

    // RTS startup and module is static stuff, in particular module constants
    // which are created during module init are static and do not need to be
    // scanned. We therefore use the real_malloc (not GC_malloc) so that it is
    // not traced by the GC, thus saving loads of work scanning this memory
    // over and over.
    acton_replace_allocator(malloc, malloc, realloc, calloc, acton_noop_free, strdup, strndup);
    $register_builtin();
    B___init__();
    $register_rts();

    WorkerCtx wctx = GC_memalign(_Alignof(struct WorkerCtx), sizeof(struct WorkerCtx));
    wctxs[0] = wctx;
    wctx->id = 0;
    wctx->uv_loop = uv_loops[wctx->id];
    wctx->jump_top = NULL;
    wctx->jump0 = NULL;
#ifdef ACTON_THREADS
    pthread_setspecific(pkey_wctx, (void *)wctx);
#endif
    start_keys(FIRST_KEY);

    $ROOTINIT();
    acton_replace_allocator(GC_malloc, GC_malloc_atomic, GC_realloc, GC_calloc, acton_noop_free, GC_strdup, GC_strndup);

    unsigned int seed;
    if (ddb_host) {
#ifdef ACTON_DB
        GET_RANDSEED(&seed, 0);
        log_info("Starting distributed RTS node, host=%s, node_id=%d, rack_id=%d, datacenter_id=%d", rts_host, rts_node_id, rts_rack_id, rts_dc_id);
        log_info("Using distributed database backend replication factor of %d", ddb_replication);
        char ** seed_hosts = (char **) malloc(ddb_no_host * sizeof(char *));
        int * seed_ports = (int *) malloc(ddb_no_host * sizeof(int));

        for (int i=0; i<ddb_no_host; i++) {
            seed_hosts[i] = acton_strdup(ddb_host[i]);
            seed_ports[i] = ddb_port;
            char *colon = strchr(seed_hosts[i], ':');
            if (colon) {
                *colon = '\0';
                seed_ports[i] = atoi(colon + 1);
            }
            log_info("Using distributed database backend (DDB): %s:%d", seed_hosts[i], seed_ports[i]);
        }
        db = get_remote_db(ddb_replication, rts_rack_id, rts_dc_id, rts_host, rts_node_id, ddb_no_host, seed_hosts, seed_ports, &seed);
        free(seed_hosts);
        free(seed_ports);
#else
        fprintf(stderr, "ERROR: DB support disabled, unable to use provided DB backend host.\n");
        fprintf(stderr, "HINT: Enable DB backend: acton --db\n");
        exit(1);
#endif
    }

#ifdef ACTON_DB
    if (db) {
        snode_t* start_row = NULL, * end_row = NULL;
        log_info("Checking for existing actor state in DDB.");
        int ret = 0,  minority_status = 0, no_items = 0;
        while(!rts_exit) {
            ret = remote_read_full_table_in_txn(&start_row, &end_row, ACTORS_TABLE, &no_items, &minority_status, NULL, db);
            if(!handle_status_and_schema_mismatch(ret, minority_status, 0))
                break;
        }
        if (no_items > 0) {
            log_info("Found %d existing actors; Restoring actor state from DDB.", no_items);
            deserialize_system(start_row);
            log_info("Actor state restored from DDB.");
        } else {
            log_info("No previous state in DDB; Initializing database...\n");
            queue_callback * gqc = get_queue_callback(queue_group_message_callback);
            rtsd_printf("### initializing remote_subscribe_group(consumer_id = %d, group_id = %d)\n", (int) db->local_rts_id, (int) db->local_rts_id);
            while(!rts_exit) {
                ret = remote_subscribe_group((WORD) db->local_rts_id, NULL, NULL, (WORD) db->local_rts_id, gqc, &minority_status, db);
                if(!handle_status_and_schema_mismatch(ret, minority_status, 0)) {
                    break;
                }
            }
            int indices[] = {0};
            db_schema_t* db_schema = db_create_schema(NULL, 1, indices, 1, indices, 0, indices, 0);
            create_db_queue(TIMER_QUEUE);
            timer_consume_hd = 0;
            BOOTSTRAP(new_argc, new_argv);
            log_info("Database intialization complete.");
        }
    } else {
#endif
        BOOTSTRAP(new_argc, new_argv);
#ifdef ACTON_DB
    }
#endif

#ifdef ACTON_THREADS
    cpu_set_t cpu_set;

    size_t primary_thread_stack_size;
    size_t target_thread_stack_size;
#if defined(IS_MACOS)
    primary_thread_stack_size = pthread_get_stacksize_np(pthread_self());
#else
    pthread_attr_t attr;
    pthread_getattr_np(pthread_self(), &attr);
    pthread_attr_getstacksize(&attr, &primary_thread_stack_size);
    pthread_attr_destroy(&attr);
#endif
    target_thread_stack_size = REQUIRED_STACK_SIZE > primary_thread_stack_size ? (size_t)REQUIRED_STACK_SIZE : primary_thread_stack_size;

    if (primary_thread_stack_size < target_thread_stack_size)
        log_warn("Current primary thread stack size: %u, required thread stack size: %u", primary_thread_stack_size, target_thread_stack_size);

    pthread_attr_t ss_attr;
    size_t secondary_thread_stack_size = 0;
    pthread_attr_init(&ss_attr);
    pthread_attr_getstacksize(&ss_attr, &secondary_thread_stack_size);
    if (secondary_thread_stack_size < target_thread_stack_size)
    {
        log_debug("Secondary thread stack size: %d, required thread stack size: %d", secondary_thread_stack_size, target_thread_stack_size);
        int err = pthread_attr_setstacksize(&ss_attr, target_thread_stack_size);
        if (err)
            log_error("pthread_attr_setstacksize failed: %s", strerror(err));
    }

    // RTS Monitor Log
    pthread_t mon_log_thread;
    if (mon_log_path) {
        pthread_create(&mon_log_thread, &ss_attr, $mon_log_loop, (void *)(intptr_t)mon_log_period);
        if (cpu_pin) {
            CPU_ZERO(&cpu_set);
            CPU_SET(0, &cpu_set);
            pthread_setaffinity_np(mon_log_thread, sizeof(cpu_set), &cpu_set);
        }
    }

    // RTS Monitor Socket
    pthread_t mon_socket_thread;
    if (mon_socket_path) {
        pthread_create(&mon_socket_thread, &ss_attr, $mon_socket_loop, NULL);
        if (cpu_pin) {
            CPU_ZERO(&cpu_set);
            CPU_SET(0, &cpu_set);
            pthread_setaffinity_np(mon_socket_thread, sizeof(cpu_set), &cpu_set);
        }
    }

    // Start worker threads
    pthread_t threads[MAX_WTHREADS];
    // Worker threads run through 0..num_wthreads where 0 is the main thread,
    // thus we need to start 1..num_wthreads. Only need to keep track of
    // branches we start.
    for (int idx = 1; idx <= num_wthreads; idx++) {
        pthread_create(&threads[idx-1], &ss_attr, main_loop, (void*)(intptr_t)idx);
        // Index start at 1 and we pin wthreads to CPU 1...n
        // We use CPU 0 for misc threads, like IO / mon etc
        if (cpu_pin) {
            CPU_ZERO(&cpu_set);
            CPU_SET(idx, &cpu_set);
            //pthread_setaffinity_np(threads[idx-1], sizeof(cpu_set), &cpu_set);
        }
    }
    sync_pause_workers_started();

    pthread_attr_destroy(&ss_attr);
#endif


    uv_check_init(aux_uv_loop, &work_ev[wctx->id]);
    work_ev[wctx->id].data = wctx;
    uv_check_start(&work_ev[wctx->id], (uv_check_cb)wt_work_cb);

    // Run the timer queue and keep track of other periodic tasks
    timer_init(aux_uv_loop);
    timer_fire();

    // Run the uv loop for the main thread
    wt_stats[0].state = WT_Idle;
    int r = uv_run(aux_uv_loop, UV_RUN_DEFAULT);
    wt_stats[0].state = WT_NoExist;

    // -- SHUTDOWN --

#ifdef ACTON_THREADS
    // Join threads
    for (int idx = 1; idx <= num_wthreads; idx++) {
        pthread_join(threads[idx-1], NULL);
    }

    pthread_mutex_lock(&rts_exit_lock);
    pthread_cond_broadcast(&rts_exit_signal);
    pthread_mutex_unlock(&rts_exit_lock);

    if (mon_log_path) {
        pthread_join(mon_log_thread, NULL);
    }

    if (mon_on_exit) {
        const char *stats_json = stats_to_json();
        printf("%s\n", stats_json);
    }
#endif

    if (logf) {
        fclose(logf);
    }

    return return_val;
}
