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
#ifdef __linux__
#include <sys/prctl.h>
#endif

#include "common.h"
#include "common.c"

#include "yyjson.h"
#include "rts.h"

#include <uv.h>

#include "q.c"

#include "io.c"

#include "log.c"
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
uv_timer_t *timer_ev;

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

B_Msg timerQ = NULL;
$Lock timerQ_lock;

int64_t next_key = -10;
$Lock next_key_lock;

int64_t timer_consume_hd = 0;       // Lacks protection, although spinlocks wouldn't help concurrent increments. Must fix in db!

time_t current_time() {
    uv_timespec64_t now;
    if (uv_clock_gettime(UV_CLOCK_REALTIME, &now) != 0) {
        log_error("uv_clock_gettime() failed");
        return 0;
    }
    return now.tv_sec * 1000000 + now.tv_nsec / 1000;
}

#ifdef ACTON_THREADS
pthread_key_t self_key;
pthread_key_t pkey_wctx;

pthread_mutex_t rts_exit_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t rts_exit_signal = PTHREAD_COND_INITIALIZER;

#define NUM_THREADS num_wthreads+1

void pin_actor_affinity() {
    $Actor a = ($Actor)pthread_getspecific(self_key);
    WorkerCtx wctx = (WorkerCtx)pthread_getspecific(pkey_wctx);
    long i = wctx->id;
    log_debug("Pinning affinity for %s actor %ld to current WT %d", a->$class->$GCINFO, a->$globkey, i);
    a->$affinity = i;
}

void set_actor_affinity(int wthread_id) {
    $Actor a = ($Actor)pthread_getspecific(self_key);
    log_debug("Setting affinity for %s actor %ld to WT %d", a->$class->$GCINFO, a->$globkey, wthread_id);
    a->$affinity = wthread_id;
}
#else // ACTON_THREADS
$Actor self_actor;

#define NUM_THREADS 1

void pin_actor_affinity() { }
void set_actor_affinity(int wthread_id) { }
#endif // ACTON_THREADS

void wake_wt(int wtid) {
    // We are sometimes optimistically called, i.e. the caller sometimes does
    // not really know whether there is new work or not. We check and if there
    // is not, then there is no need to wake anyone up.
    if (!rqs[wtid].head)
        return;

#ifdef ACTON_THREADS
    // wake up corresponding worker threads....
    if (wtid == SHARED_RQ) {
        for (int i = 1; i <= num_wthreads; i++) {
            if (wt_stats[i].state == WT_Idle) {
                uv_async_send(&wake_ev[i]);
                return;
            }
        }
    } else {
        // thread specific queue
        uv_async_send(&wake_ev[wtid]);
    }
#else
    uv_async_send(&wake_ev[wtid]);
#endif

}

void reset_timeout() {
    // Wake up timerQ thread
    uv_async_send(&wake_ev[0]);
}
int64_t get_next_key() {
    spinlock_lock(&next_key_lock);
    int64_t res = --next_key;
    spinlock_unlock(&next_key_lock);
    return res;
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
    atomic_flag_clear(&m->$wait_lock);
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
    atomic_flag_clear(&m->$wait_lock);
    m->$globkey = get_next_key();
    return m;
}


////////////////////////////////////////////////////////////////////////

B_bool B_MsgD___bool__(B_Msg self) {
  return B_True;
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
    $val_serialize(ITEM_ID,&self->$baseline,state);
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
    res->$baseline = (time_t)$val_deserialize(state);
    res->value = $step_deserialize(state);
    atomic_flag_clear(&res->$wait_lock);
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
    atomic_flag_clear(&a->B_Msg_lock);
    a->$globkey = get_next_key();
    a->$affinity = SHARED_RQ;
    rtsd_printf("# New Actor %ld at %p of class %s", a->$globkey, a, a->$class->$GCINFO);
}

B_bool $ActorD___bool__($Actor self) {
  return B_True;
}

B_str $ActorD___str__($Actor self) {
  return $FORMAT("<$Actor %ld %s at %p>", self->$globkey, self->$class->$GCINFO, self);
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
    atomic_flag_clear(&res->B_Msg_lock);
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

B_bool $CatcherD___bool__($Catcher self) {
  return B_True;
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

B_bool $ConstContD___bool__($ConstCont self) {
  return B_True;
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

// Atomically enqueue timed message "m" onto the global timer-queue, at position
// given by "m->baseline".
bool ENQ_timed(B_Msg m) {
    time_t m_baseline = m->$baseline;
    bool new_head = false;
    spinlock_lock(&timerQ_lock);
    B_Msg x = timerQ;
    if (x && x->$baseline <= m_baseline) {
        B_Msg next = x->$next;
        while (next && next->$baseline <= m_baseline) {
            x = next;
            next = x->$next;
        }
        x->$next = m;
        m->$next = next;
    } else {
        timerQ = m;
        m->$next = x;
        new_head = true;
    }
    spinlock_unlock(&timerQ_lock);
    return new_head;
}

// Atomically dequeue and return the first message from the global timer-queue if 
// its baseline is less or equal to "now", else return NULL.
B_Msg DEQ_timed(time_t now) {
    spinlock_lock(&timerQ_lock);
    B_Msg res = timerQ;
    if (res) {
        if (res->$baseline <= now) {
            timerQ = res->$next;
            res->$next = NULL;
        } else {
            res = NULL;
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

B_bool $DoneD___bool__($Cont self) {
  return B_True;
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

B_bool $FailD___bool__($Cont self) {
  return B_True;
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

void create_db_queue(long key) {
    int minority_status = 0;
    while(!rts_exit) {
        int ret = remote_create_queue_in_txn(MSG_QUEUE, ($WORD)key, &minority_status, NULL, db);
        rtsd_printf("#### Create queue %ld returns %d", key, ret);
        if(ret == NO_QUORUM_ERR) {
            sleep(3);
            continue;
        }
        if(ret == 0 || ret == CLIENT_ERR_SUBSCRIPTION_EXISTS)
            break;
    }
}

void init_db_queue(long key) {
    if (db)
        create_db_queue(key);
}

void register_actor(long key) {
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
    rtsd_printf("# AFTER by %ld", self->$globkey);
    time_t baseline = self->B_Msg->$baseline + sec->val * 1000000;
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

int handle_status_and_schema_mismatch(int ret, int minority_status, long key)
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
    rtsd_printf("#### FLUSH_outgoing messages from %ld to DB queues", self->$globkey);
    B_Msg m = self->$outgoing;
    while (m) {
        long dest = (m->$baseline == self->B_Msg->$baseline)? m->$to->$globkey : 0;
        int ret = 0, minority_status = 0;
        while(!rts_exit) {
            ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)dest, &minority_status, txnid, db);
            if (dest) {
                    rtsd_printf("   # enqueue msg %ld to queue %ld returns %d, minority_status=%d", m->$globkey, dest, ret, minority_status);
            } else {
                    rtsd_printf("   # enqueue msg %ld to TIMER_QUEUE returns %d, minority_status=%d", m->$globkey, ret, minority_status);
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
    rtsd_printf("#### FLUSH_outgoing messages from %ld to RTS-internal queues", self->$globkey);
    B_Msg m = self->$outgoing;
    self->$outgoing = NULL;
    while (m) {
        B_Msg next = m->$next;
        m->$next = NULL;
        long dest;
        if (m->$baseline == self->B_Msg->$baseline) {
            $Actor to = m->$to;
            if (ENQ_msg(m, to)) {
                fprintf(stderr, ">>>> ASYNC msg %p from actor %ld wakes up target %ld\n", m, self->$globkey, to->$globkey);
                ENQ_ready(to);
            } else {
                fprintf(stderr, ">>>> ASYNC msg %p from actor %ld queued on target %ld\n", m, self->$globkey, to->$globkey);
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

time_t next_timeout() {
    return timerQ ? timerQ->$baseline : 0;
}

void handle_timeout() {
    time_t now = current_time();
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

                long key = TIMER_QUEUE;
                snode_t *m_start, *m_end;
                int entries_read = 0, minority_status = 0;
                int64_t read_head = -1;

                int ret0 = remote_read_queue_in_txn(($WORD)db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, &minority_status, NULL, db);
                rtsd_printf("   # dummy read msg from TIMER_QUEUE returns %d, entries read: %d", ret0, entries_read);
                if(handle_status_and_schema_mismatch(ret0, minority_status, key))
                    continue;

                int ret1 = remote_consume_queue_in_txn(($WORD)db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key, read_head, &minority_status, txnid, db);
                rtsd_printf("   # consume msg %ld from TIMER_QUEUE returns %d", m->$globkey, ret1);
                if(handle_status_and_schema_mismatch(ret1, minority_status, key))
                    continue;

                int ret2 = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)m->$to->$globkey, &minority_status, txnid, db);
                rtsd_printf("   # (timed) enqueue msg %ld to queue %ld returns %d", m->$globkey, m->$to->$globkey, ret2);
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
}

////////////////////////////////////////////////////////////////////////////////////////

B_dict globdict = NULL;

$WORD try_globdict($WORD w) {
    int key = (int)(long)w;
    $WORD obj = B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), NULL);
    return obj;
}

#ifdef ACTON_DB
long read_queued_msg(long key, int64_t *read_head) {
    snode_t *m_start, *m_end;
    int entries_read = 0, minority_status = 0, ret = 0;
    
    while(!rts_exit) {
        ret = remote_read_queue_in_txn(($WORD)db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key,
                                           1, &entries_read, read_head, &m_start, &m_end, &minority_status, NULL, db);
        rtsd_printf("   # read msg from queue %ld returns %d, entries read: %d, minority_status: %d", key, ret, entries_read, minority_status);
        if(!handle_status_and_schema_mismatch(ret, minority_status, key))
            break;
    }

    if (!entries_read)
        return 0;
    db_row_t *r = (db_row_t*)m_start->value;
    rtsd_printf("# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
    return (long)r->column_array[0];
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
    rtsd_printf("     globkey: %ld", m->$globkey);
}

void print_actor($Actor a) {
    rtsd_printf("==== Actor %p", a);
    rtsd_printf("     next: %p", a->$next);
    rtsd_printf("     msg: %p", a->B_Msg);
    rtsd_printf("     outgoing: %p", a->$outgoing);
    rtsd_printf("     waitsfor: %p", a->$waitsfor);
    rtsd_printf("     consume_hd: %ld", (long)a->$consume_hd);
    rtsd_printf("     catcher: %p", a->$catcher);
    rtsd_printf("     globkey: %ld", a->$globkey);
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

    long min_key = 0;

    rtsd_printf("#### Msg allocation:");
    for(snode_t * node = msgs_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        rtsd_printf("# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
        long key = (long)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            rtsd_printf("# r2 %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d", r2, (long)r2->key, r2->cells, r2->column_array, r2->no_columns, r2->last_blob_size);
            BlobHd *head = (BlobHd*)r2->column_array[0];
            B_Msg msg = (B_Msg)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            msg->$globkey = key;
            B_dictD_setitem(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), msg);
            rtsd_printf("# Allocated Msg %p = %ld of class %s = %d", msg, msg->$globkey, msg->$class->$GCINFO, msg->$class->$class_id);
            if (key < min_key)
                min_key = key;
        }
    }
    rtsd_printf("#### Actor allocation:");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        rtsd_printf("# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
        long key = (long)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            rtsd_printf("# r2 %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d", r2, (long)r2->key, r2->cells, r2->column_array, r2->no_columns, r2->last_blob_size);
            BlobHd *head = (BlobHd*)r2->column_array[0];
            $Actor act = ($Actor)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            act->$globkey = key;
            B_dictD_setitem(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), act);
            rtsd_printf("# Allocated Actor %p = %ld of class %s = %d", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            if (key < min_key)
                min_key = key;
        }
        register_actor(key);
    }
    next_key = min_key;

    rtsd_printf("#### Msg contents:");
    for(snode_t * node = msgs_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        long key = (long)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            $WORD *blob = ($WORD*)r2->column_array[0];
            int blob_size = r2->last_blob_size;
            $ROW row = extract_row(blob, blob_size);
            B_Msg msg = (B_Msg)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), NULL);
            rtsd_printf("####### Deserializing msg %p = %ld of class %s = %d", msg, msg->$globkey, msg->$class->$GCINFO, msg->$class->$class_id);
            print_rows(row);
            $glob_deserialize(($Serializable)msg, row, try_globdict);
            print_msg(msg);
        }
    }

    rtsd_printf("#### Actor contents:");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        long key = (long)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            $WORD *blob = ($WORD*)r2->column_array[0];
            int blob_size = r2->last_blob_size;
            $ROW row = extract_row(blob, blob_size);
            $Actor act = ($Actor)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), NULL);
            rtsd_printf("####### Deserializing actor %p = %ld of class %s = %d", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            print_rows(row);
            $glob_deserialize(($Serializable)act, row, try_globdict);

            B_Msg m = act->$waitsfor;
            if (m && !FROZEN(m)) {
                ADD_waiting(act, m);
                rtsd_printf("# Adding Actor %ld to wait for Msg %ld", act->$globkey, m->$globkey);
            }
            else {
                act->$waitsfor = NULL;
            }

            rtsd_printf("#### Reading msgs queue %ld contents:", key);
            int64_t prev_read_head = -1; //, prev_consume_head = -1;
            int ret = 0, minority_status = 0;
            while (!rts_exit) {
                    long msg_key = read_queued_msg(key, &prev_read_head);
                if (!msg_key)
                    break;
                m = B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(msg_key), NULL);
                rtsd_printf("# Adding Msg %ld to Actor %ld", m->$globkey, act->$globkey);
                ENQ_msg(m, act);
            }
            if (act->B_Msg && !act->$waitsfor) {
                ENQ_ready(act);
                rtsd_printf("# Adding Actor %ld to the readyQ", act->$globkey);
            }
            print_actor(act);
        }
    }

    rtsd_printf("#### Actor resume:");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        long key = (long)r->key;
        $Actor act = ($Actor)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(key), NULL);
        rtsd_printf("####### Resuming actor %p = %ld of class %s = %d", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
        act->$class->__resume__(act);
    }

    rtsd_printf("#### Reading timer queue contents:");
    time_t now = current_time();
    int64_t prev_read_head = -1;
    while(!rts_exit) {
        long msg_key = read_queued_msg(TIMER_QUEUE, &prev_read_head);
        if (!msg_key)
            break;
        B_Msg m = B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(msg_key), NULL);
        if (m->$baseline < now)
            m->$baseline = now;
        rtsd_printf("# Adding Msg %ld to the timerQ", m->$globkey);
        ENQ_timed(m);
    }

    /*
     * Actor IDs (-11 & -12) here chosen by fair dice roll... Haha, kidding.
     * These values are aligned with the IDs allocated by get_next_key() when
     * called in the BOOTSTRAP() function. The ID allocator next_key starts at
     * -10, so -11 is the first key handed out and with the env actor is created
     * first, it will get -11. Similarly for the root actor, which is assigned
     * ID -12 (via the $NEWROOT call embedded in the external function $ROOT).
     * These values must be kept in sync with next_key and the structure in
     * the BOOTSTRAP() function!
     */
    env_actor  = (B_Env)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(-11), NULL);
    root_actor = ($Actor)B_dictD_get(globdict, (B_Hashable)B_HashableD_intG_witness, to$int(-12), NULL);
    globdict = NULL;
    rtsd_printf("System deserialized");
}
#endif

$WORD try_globkey($WORD obj) {
    $SerializableG_class c = (($Serializable)obj)->$class;
    if (c->$class_id == MSG_ID) {
        long key = ((B_Msg)obj)->$globkey;
        return ($WORD)key;
    } else if (c->$class_id == ACTOR_ID || c->$superclass && c->$superclass->$class_id == ACTOR_ID) {
        long key = (($Actor)obj)->$globkey;
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
void insert_row(long key, size_t total, $ROW row, $WORD table, uuid_t *txnid) {
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
        rtsd_printf("   # insert to table %ld, row %ld, returns %d", (long)table, key, ret);
        if(!handle_status_and_schema_mismatch(ret, minority_status, 0))
            break;
    }
}

void serialize_msg(B_Msg m, uuid_t *txnid) {
    rtsd_printf("#### Serializing Msg %ld", m->$globkey);
    $ROW row = $glob_serialize(($Serializable)m, try_globkey);
    print_rows(row);
    insert_row(m->$globkey, $total_rowsize(row), row, MSGS_TABLE, txnid);
}

void serialize_actor($Actor a, uuid_t *txnid) {
    rtsd_printf("#### Serializing Actor %ld", a->$globkey);
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
        wit->$class->append(wit,args,to$str(argv[i]));

    env_actor = B_EnvG_newactor(B_WorldCapG_new(), B_SysCapG_new(), args);
    env_actor->nr_wthreads = to$int(num_wthreads);

    root_actor = $ROOT();                           // Assumed to return $NEWACTOR(X) for the selected root actor X
    time_t now = current_time();
    B_Msg m = B_MsgG_newXX(root_actor, &$InitRoot$cont, now, &$Done$instance);
#ifdef ACTON_DB
    if (db) {
            int ret = 0, minority_status = 0;
            while(!rts_exit) {
                ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)root_actor->$globkey, &minority_status, NULL, db);
                rtsd_printf("   # enqueue bootstrap msg %ld to root actor queue %ld returns %d, minority_status %d", m->$globkey, root_actor->$globkey, ret, minority_status);
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

                    long key = current->$globkey;
                    snode_t *m_start, *m_end;
                    int entries_read = 0, minority_status = 0;
                    int64_t read_head = -1;

                    int ret0 = remote_read_queue_in_txn(($WORD) db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, &minority_status, NULL, db);
                    rtsd_printf("   # dummy read msg from queue %ld returns %d, entries read: %d", key, ret0, entries_read);
                    if(handle_status_and_schema_mismatch(ret0, minority_status, key))
                        continue;

                    int ret1 = remote_consume_queue_in_txn(($WORD) db->local_rts_id, 0, 0, MSG_QUEUE, ($WORD)key, read_head, &minority_status, txnid, db);
                    rtsd_printf("   # consume msg %ld from queue %ld returns %d", m->$globkey, key, ret1);
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


void arm_timer_ev();
void main_wake_cb(uv_async_t *ev) {
    // Wäjky-päjky
    arm_timer_ev();
}

void main_timer_cb(uv_timer_t *ev) {
    handle_timeout();
    arm_timer_ev();
}

void arm_timer_ev() {
    time_t next_time = next_timeout();
    if (next_time) {
        time_t now = current_time();
        long long int offset = (next_time - now) / 1000; // offset in milliseconds
        // Negative offset means we missed to trigger in time. Set timeout to 0
        // to directly run on next uv cycle.
        if (offset < 0)
            offset = 0;
        int r = uv_timer_start(timer_ev, main_timer_cb, offset, 0);
        if (r != 0) {
            char errmsg[1024] = "Unable to set timer: ";
            uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
            log_fatal(errmsg);
        }
    } else {
        int r = uv_timer_stop(timer_ev);
        if (r != 0) {
            char errmsg[1024] = "Unable to stop timer: ";
            uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
            log_fatal(errmsg);
        }
    }
}

void wt_stop_cb(uv_async_t *ev) {
    WorkerCtx wctx = GET_WCTX();
    uv_stop((uv_loop_t *)wctx->uv_loop);
}

void wt_wake_cb(uv_async_t *ev) {
    // We just wake up the uv loop here if it is blocked waiting for IO, real
    // work is run later when wt_work_cb is called as part of the "check" phase.
}

void wt_work_cb(uv_check_t *ev) {
    WorkerCtx wctx = (WorkerCtx)ev->data;
    assert(wctx->id >= 0 && wctx->id < 256);

    uv_timespec64_t ts_start, ts1, ts2, ts3;
    long long int runtime = 0;

    uv_clock_gettime(UV_CLOCK_MONOTONIC, &ts_start);
    while (true) {
        if (rts_exit) {
            return;
        }
        volatile $Actor current = DEQ_ready(wctx->id);
        if (!current)
            return;

        wake_wt(SHARED_RQ);

        SET_SELF(current);
        volatile B_Msg m = current->B_Msg;
        $Cont cont = m->$cont;
        $WORD val = m->value;

        uv_clock_gettime(UV_CLOCK_MONOTONIC, &ts1);
        wt_stats[wctx->id].state = WT_Working;

        $R r;
        if (wctx->jump0 || $PUSH()) {                         // Normal path
            if (!wctx->jump0) {
                wctx->jump0 = wctx->jump_top;
            }
            rtsd_printf("## Running actor %ld : %s", current->$globkey, current->$class->$GCINFO);
            fprintf(stderr, "RUNNING actor %ld : %s\n", current->$globkey, current->$class->$GCINFO);
            r = cont->$class->__call__(cont, val);

            uv_clock_gettime(UV_CLOCK_MONOTONIC, &ts2);
            long long int diff = (ts2.tv_sec * 1000000000 + ts2.tv_nsec) - (ts1.tv_sec * 1000000000 + ts1.tv_nsec);

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
            assert(wctx->jump0 != NULL);
            assert(wctx->jump0->xval != NULL);
            B_BaseException ex = wctx->jump0->xval;
            rtsd_printf("## (%d) Actor %ld : %s longjmp exception: %s", wctx->id, current->$globkey, current->$class->$GCINFO, ex->$class->$GCINFO);
            r = $R_FAIL(ex);
        }

        switch (r.tag) {
        case $RDONE: {
            save_actor_state(current, m);
            m->value = r.value;                             // m->value holds the message result,
            $Actor b = FREEZE_waiting(m, MARK_RESULT);      // so mark this and stop further m->waiting additions
            if (b) {
                while (b) {
                    b->B_Msg->value = r.value;
                    b->$waitsfor = NULL;
                    $Actor c = b->$next;
                    ENQ_ready(b);
                    rtsd_printf("## Waking up actor %ld : %s", b->$globkey, b->$class->$GCINFO);
                    fprintf(stderr, "==== Result by actor %ld for msg %p, waking up client %ld\n", current->$globkey, m, b->$globkey);
                    b = c;
                }
            } else {
                fprintf(stderr, "==== Result by actor %ld for msg %p, no waiting clients\n", current->$globkey, m);
            }
            rtsd_printf("## DONE actor %ld : %s", current->$globkey, current->$class->$GCINFO);
            if (DEQ_msg(current)) {
                ENQ_ready(current);
            }
            break;
        }
        case $RCONT: {
            m->$cont = r.cont;
            m->value = r.value;
            rtsd_printf("## CONT actor %ld : %s", current->$globkey, current->$class->$GCINFO);
            ENQ_ready(current);
            break;
        }
        case $RFAIL: {
            $Catcher c = current->$catcher;
            if (c) {                            // Normal exception handling
                c->xval = (B_BaseException)r.value;
                m->$cont = c->$cont;
                m->value = B_False;             // False signals the exceptional branch
                rtsd_printf("## FAIL/handle actor %ld : %s", current->$globkey, current->$class->$GCINFO);
                ENQ_ready(current);
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
                    fprintf(stderr, "Unhandled exception in actor: %s[%ld]:\n  %s\n", unmangle_name(current->$class->$GCINFO), current->$globkey, fromB_str(ex->$class->__str__(ex)));
                while (b) {
                    b->B_Msg->$cont = &$Fail$instance;
                    b->B_Msg->value = r.value;
                    b->$waitsfor = NULL;
                    $Actor c = b->$next;
                    ENQ_ready(b);
                    rtsd_printf("## Propagating exception to actor %ld : %s", b->$globkey, b->$class->$GCINFO);
                    fprintf(stderr, "==== Exception by actor %ld for msg %p, waking up client %ld\n", current->$globkey, m, b->$globkey);
                    b = c;
                }
                if (DEQ_msg(current)) {
                    ENQ_ready(current);
                }
                rtsd_printf("## Done handling failed actor %ld : %s", current->$globkey, current->$class->$GCINFO);
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
                FLUSH_outgoing_local(current);
            } else {
#endif
                reverse_outgoing_queue(current);
                FLUSH_outgoing_local(current);
#ifdef ACTON_DB
            }
#endif
            m->$cont = r.cont;
            B_Msg x = (B_Msg)r.value;
            assert(x != NULL);
            if (ADD_waiting(current, x)) {      // x->cont is a proper $Cont: x is still being processed so current was added to x->waiting
                rtsd_printf("## AWAIT actor %ld : %s", current->$globkey, current->$class->$GCINFO);
                fprintf(stderr, "---- AWAIT by client %ld on msg %p to actor %ld\n", current->$globkey, x, x->$to->$globkey);
                current->$waitsfor = x;
            } else if (EXCEPTIONAL(x)) {        // x->cont == MARK_EXCEPTION: x->value holds the raised exception, current is not in x->waiting
                rtsd_printf("## AWAIT/fail actor %ld : %s", current->$globkey, current->$class->$GCINFO);
                fprintf(stderr, "---- Immediate exception found by client %ld in msg %p to actor %ld\n", current->$globkey, x, x->$to->$globkey);
                m->$cont = &$Fail$instance;
                m->value = x->value;
                ENQ_ready(current);
            } else {                            // x->cont == MARK_RESULT: x->value holds the final response, current is not in x->waiting
                rtsd_printf("## AWAIT/wakeup actor %ld : %s", current->$globkey, current->$class->$GCINFO);
                fprintf(stderr, "---- Immediate result found by client %ld in msg %p to actor %ld\n", current->$globkey, x, x->$to->$globkey);
                m->value = x->value;
                ENQ_ready(current);
            }
            break;
        }
        }
        SET_SELF(NULL);

        uv_clock_gettime(UV_CLOCK_MONOTONIC, &ts3);
        long long int diff = (ts3.tv_sec * 1000000000 + ts3.tv_nsec) - (ts2.tv_sec * 1000000000 + ts2.tv_nsec);
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

        wt_stats[wctx->id].state = WT_Idle;

        runtime = (ts3.tv_sec * 1000000000 + ts3.tv_nsec) - (ts_start.tv_sec * 1000000000 + ts_start.tv_nsec);
        // run for max 20ms before yielding to IO
        // NOTE: since we are not preemptive, a single long continuation can
        // exceed this cap
        if (runtime > 20*1000000)
            break;
    }

    // if there's more work, wake up ourselves again to process more but
    // interleave with some IO
    uv_async_send(&wake_ev[wctx->id]);
}

void *main_loop(void *idx) {
    WorkerCtx wctx = (WorkerCtx)GC_malloc(sizeof(struct WorkerCtx));
    wctxs[(long)idx] = wctx;
    wctx->id = (long)idx;
    wctx->uv_loop = uv_loops[wctx->id];
    wctx->jump_top = NULL;
    wctx->jump0 = NULL;
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
    localtime_r(&ts.tv_sec, &tm);
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
    localtime_r(&ts.tv_sec, &tm);
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
    localtime_r(&ts.tv_sec, &tm);
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
void print_trace() {
    char pid_buf[30];
    sprintf(pid_buf, "%d", getpid());
    char name_buf[512];
    name_buf[readlink("/proc/self/exe", name_buf, 511)]=0;
#ifdef __linux__
    prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0);
#endif
#ifdef __linux__
    int child_pid = fork();
    if (!child_pid) {
        dup2(2, 1); // redirect output to stderr
        execlp("lldb", "lldb", "-p", pid_buf, "--batch", "-o", "thread backtrace all", "-o", "exit", "--one-line-on-crash", "exit", name_buf, NULL);
        execlp("gdb", "gdb", "--quiet", "--batch", "-n", "-ex", "set confirm off", "-ex", "set pagination off", "-ex", "set debuginfod enabled off", "-ex", "thread", "-ex", "thread apply all backtrace full", name_buf, pid_buf, NULL);
        fprintf(stderr, "Unable to get detailed backtrace using lldb or gdb\n");
        exit(0); /* If lldb/gdb failed to start */
    } else {
        waitpid(child_pid, NULL, 0);
    }
#else
    fprintf(stderr, "Unable to get detailed backtrace on this platform\n");
    exit(0); /* If lldb/gdb failed to start */
#endif
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
    name_buf[readlink("/proc/self/exe", name_buf, 511)]=0;
#ifdef __linux__
    prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0);
#endif
    int child_pid = fork();
    if (!child_pid) {
        char findthread[40] = "thread find ";
        sprintf(findthread + strlen(findthread), "%p", (void *)pthread_self());
        execlp("gdb", "gdb", "--quiet", "-n", "-ex", "set confirm off", "-ex", findthread, name_buf, pid_buf, NULL);
        fprintf(stderr, "Unable to get detailed backtrace using lldb or gdb");
        exit(0); /* If lldb/gdb failed to start */
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

    // Restore / uninstall signal handlers for SIGILL & SIGSEGV
    sa_ill.sa_handler = NULL;
    if (sigaction(SIGILL, &sa_ill, NULL) == -1) {
        log_fatal("Failed to install signal handler for SIGILL: %s", strerror(errno));
        exit(1);
    }
    sa_segv.sa_handler = NULL;
    if (sigaction(SIGSEGV, &sa_segv, NULL) == -1) {
        log_fatal("Failed to install signal handler for SIGSEGV: %s", strerror(errno));
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

void DaveNull () {}

int main(int argc, char **argv) {
    // Init garbage collector and suppress warnings
    GC_INIT();
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
    bool mon_on_exit = false;
    bool auto_backtrace = true;
    bool interactive_backtrace = false;
    char *log_path = NULL;
    FILE *logf = NULL;
    bool log_stderr = false;

    appname = argv[0];
    pid = getpid();

    fprintf(stderr, "RTS-DEBUG\n");

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
        {"rts-debug", NULL, 'd', "RTS debug, requires program to be compiled with --dev"},
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
                fprintf(stderr, "HINT: Recompile this program using: actonc --dev ...\n");
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
    if (num_wthreads > MAX_WTHREADS) {
        fprintf(stderr, "ERROR: Maximum of %d worker threads supported.\n", MAX_WTHREADS);
        fprintf(stderr, "HINT: Run this program with fewer worker threads: %s --rts-wthreads %d\n", argv[0], MAX_WTHREADS);
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

    WorkerCtx wctx = (WorkerCtx)GC_malloc(sizeof(struct WorkerCtx));
    wctxs[0] = wctx;
    wctx->id = 0;
    wctx->uv_loop = uv_loops[wctx->id];
    wctx->jump_top = NULL;
    wctx->jump0 = NULL;
#ifdef ACTON_THREADS
    pthread_setspecific(pkey_wctx, (void *)wctx);
#endif

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
        fprintf(stderr, "HINT: Enable DB backend: actonc --db\n");
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

    pthread_attr_destroy(&ss_attr);
#endif


    uv_check_init(aux_uv_loop, &work_ev[wctx->id]);
    work_ev[wctx->id].data = wctx;
    uv_check_start(&work_ev[wctx->id], (uv_check_cb)wt_work_cb);

    // Run the timer queue and keep track of other periodic tasks
    timer_ev = GC_malloc(sizeof(uv_timer_t));
    uv_timer_init(aux_uv_loop, timer_ev);
    uv_timer_start(timer_ev, main_timer_cb, 0, 0);

#ifdef ACTON_THREADS
    // Set affinity for main thread
    if (cpu_pin) {
        CPU_ZERO(&cpu_set);
        CPU_SET(0, &cpu_set);
        pthread_setaffinity_np(pthread_self(), sizeof(cpu_set), &cpu_set);
    }
#endif

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
