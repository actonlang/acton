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

#include <unistd.h>
#include <pthread.h>
#include <stdio.h>
#include <stdarg.h>
#include <uuid/uuid.h>
#include <signal.h>

#include <time.h>
#include <stdlib.h>

#include <uv.h>

#include "yyjson.h"
#include "rts.h"

#include "io.h"

#include "log.h"
#include "netstring.h"
#include "../builtin/env.h"

#include "../backend/client_api.h"
#include "../backend/fastrand.h"

extern struct dbc_stat dbc_stats;

char rts_verbose = 0;
char rts_debug = 0;
long num_wthreads;

char rts_exit = 0;
int return_val = 0;

char *appname = NULL;
pid_t pid;

uv_loop_t *uv_loops[MAX_WTHREADS];
uv_async_t stop_ev[MAX_WTHREADS];
uv_async_t wake_ev[MAX_WTHREADS];
uv_check_t work_ev[MAX_WTHREADS];

char *mon_log_path = NULL;
int mon_log_period = 30;
char *mon_socket_path = NULL;


struct wt_stat {
    unsigned int idx;          // worker thread index
    char key[10];              // thread index as string for convenience
    unsigned int state;        // current thread state
    unsigned long long sleeps; // number of times thread slept
    // Executing actor continuations is the primary work of the RTS, we measure
    // the execution time of each and count to buckets to get a rough idea of
    // how long it takes
    unsigned long long conts_count; // number of executed continuations
    unsigned long long conts_sum;   // nanoseconds spent running continuations
    unsigned long long conts_100ns; // bucket for <100ns
    unsigned long long conts_1us;   // bucket for <1us
    unsigned long long conts_10us;  // bucket for <10us
    unsigned long long conts_100us; // bucket for <100us
    unsigned long long conts_1ms;   // bucket for <1ms
    unsigned long long conts_10ms;  // bucket for <10ms
    unsigned long long conts_100ms; // bucket for <100ms
    unsigned long long conts_1s;     // bucket for <1s
    unsigned long long conts_10s;    // bucket for <10s
    unsigned long long conts_100s;   // bucket for <100s
    unsigned long long conts_inf;   // bucket for <+Inf
    // Bookkeeping is all the other work we do not directly related to running
    // actor continuations, like taking locks, committing information, talking
    // to the database etc
    unsigned long long bkeep_count; // number of bookkeeping rounds
    unsigned long long bkeep_sum;   // nanoseconds spent bookkeeping
    unsigned long long bkeep_100ns; // bucket for <100ns
    unsigned long long bkeep_1us;   // bucket for <1us
    unsigned long long bkeep_10us;  // bucket for <10us
    unsigned long long bkeep_100us; // bucket for <100us
    unsigned long long bkeep_1ms;   // bucket for <1ms
    unsigned long long bkeep_10ms;  // bucket for <10ms
    unsigned long long bkeep_100ms; // bucket for <100ms
    unsigned long long bkeep_1s;     // bucket for <1s
    unsigned long long bkeep_10s;    // bucket for <10s
    unsigned long long bkeep_100s;   // bucket for <100s
    unsigned long long bkeep_inf;   // bucket for <+Inf
};
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
///////////////////////////////////////////////////////////////////////////////////////////////
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

$Actor root_actor = NULL;
$Env env_actor = NULL;

struct readyqs {
    $Actor head;
    $Actor tail;
    unsigned long long count;
    $Lock lock;
};
struct readyqs rqs[MAX_WTHREADS+1];


$Msg timerQ = NULL;
$Lock timerQ_lock;

int64_t next_key = -10;
$Lock next_key_lock;

int64_t timer_consume_hd = 0;       // Lacks protection, although spinlocks wouldn't help concurrent increments. Must fix in db!

time_t current_time() {
    struct timeval now;
    gettimeofday(&now, NULL);
    return now.tv_sec * 1000000 + now.tv_usec;
}

pthread_key_t self_key;
pthread_key_t pkey_wtid;
pthread_key_t pkey_uv_loop;

pthread_mutex_t rts_exit_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t rts_exit_signal = PTHREAD_COND_INITIALIZER;

void pin_actor_affinity() {
    $Actor a = ($Actor)pthread_getspecific(self_key);
    int i = (int)pthread_getspecific(pkey_wtid);
    log_debug("Pinning affinity for %s actor %ld to current WT %d", a->$class->$GCINFO, a->$globkey, i);
    a->$affinity = i;
}

void wake_wt(int wtid) {
    // We are sometimes optimistically called, i.e. the caller sometimes does
    // not really know whether there is new work or not. We check and if there
    // is not, then there is no need to wake anyone up.
    if (!rqs[wtid].head)
        return;

    // wake up corresponding worker threads....
    if (wtid == 0) {
        // global queue
        for (int j = 0; j < num_wthreads; j++) {
            if (wt_stats[j].state == WT_Idle) {
                uv_async_send(&wake_ev[j]);
                return;
            }
        }
    } else {
        // thread specific queue
        uv_async_send(&wake_ev[wtid-1]);
    }

}

static inline void spinlock_lock($Lock *f) {
    while (atomic_flag_test_and_set(f) == true) {
        // spin until we could set the flag
    }
}
static inline void spinlock_unlock($Lock *f) {
    atomic_flag_clear(f);
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

remote_db_t * db = NULL;

////////////////////////////////////////////////////////////////////////////////////////

void $Msg$__init__($Msg m, $Actor to, $Cont cont, time_t baseline, $WORD value) {
    m->$next = NULL;
    m->$to = to;
    m->$cont = cont;
    m->$waiting = NULL;
    m->$baseline = baseline;
    m->$value = value;
    atomic_flag_clear(&m->$wait_lock);
    m->$globkey = get_next_key();
}

$bool $Msg$__bool__($Msg self) {
  return $True;
}

$str $Msg$__str__($Msg self) {
  char *s;
  asprintf(&s,"<$Msg object at %p>",self);
  return to$str(s);
}

void $Msg$__serialize__($Msg self, $Serial$state state) {
    $step_serialize(self->$to,state);
    $step_serialize(self->$cont,state);
    $val_serialize(ITEM_ID,&self->$baseline,state);
    $step_serialize(self->$value,state);
}


$Msg $Msg$__deserialize__($Msg res, $Serial$state state) {
    if (!res) {
        if (!state) {
            res = malloc(sizeof (struct $Msg));
            res->$class = &$Msg$methods;
            return res;
        }
        res = $DNEW($Msg,state);
    }
    res->$next = NULL;
    res->$to = $step_deserialize(state);
    res->$cont = $step_deserialize(state);
    res->$waiting = NULL;
    res->$baseline = (time_t)$val_deserialize(state);
    res->$value = $step_deserialize(state);
    atomic_flag_clear(&res->$wait_lock);
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $Actor$__init__($Actor a) {
    a->$next = NULL;
    a->$msg = NULL;
    a->$outgoing = NULL;
    a->$waitsfor = NULL;
    a->$consume_hd = 0;
    a->$catcher = NULL;
    atomic_flag_clear(&a->$msg_lock);
    a->$globkey = get_next_key();
    a->$affinity = 0;
    rtsd_printf("# New Actor %ld at %p of class %s", a->$globkey, a, a->$class->$GCINFO);
}

$bool $Actor$__bool__($Actor self) {
  return $True;
}

$str $Actor$__str__($Actor self) {
  char *s;
  asprintf(&s,"<$Actor %ld %s at %p>", self->$globkey, self->$class->$GCINFO, self);
  return to$str(s);
}

$NoneType $Actor$__resume__($Actor self) {
  return $None;
}

void $Actor$__serialize__($Actor self, $Serial$state state) {
    $step_serialize(self->$waitsfor,state);
    $val_serialize(ITEM_ID,&self->$consume_hd,state);
    $step_serialize(self->$catcher,state);
}

$Actor $Actor$__deserialize__($Actor res, $Serial$state state) {
    if (!res) {
        if (!state) {
            res = malloc(sizeof(struct $Actor));
            res->$class = &$Actor$methods;
            return res;
        }
        res = $DNEW($Actor, state);
    }
    res->$next = NULL;
    res->$msg = NULL;
    res->$outgoing = NULL;
    res->$waitsfor = $step_deserialize(state);
    res->$consume_hd = (long)$val_deserialize(state);
    res->$catcher = $step_deserialize(state);
    atomic_flag_clear(&res->$msg_lock);
    res->$affinity = 0;
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $Catcher$__init__($Catcher c, $Cont cont) {
    c->$next = NULL;
    c->$cont = cont;
}

$bool $Catcher$__bool__($Catcher self) {
  return $True;
}

$str $Catcher$__str__($Catcher self) {
  char *s;
  asprintf(&s,"<$Catcher object at %p>",self);
  return to$str(s);
}

void $Catcher$__serialize__($Catcher self, $Serial$state state) {
    $step_serialize(self->$next,state);
    $step_serialize(self->$cont,state);
}

$Catcher $Catcher$__deserialize__($Catcher self, $Serial$state state) {
    $Catcher res = $DNEW($Catcher,state);
    res->$next = $step_deserialize(state);
    res->$cont = $step_deserialize(state);
    return res;
}
///////////////////////////////////////////////////////////////////////////////////////

void $Cont$__init__($Cont $this) { }

$bool $Cont$__bool__($Cont self) {
  return $True;
}

$str $Cont$__str__($Cont self) {
  char *s;
  asprintf(&s,"<$Cont object at %p>",self);
  return to$str(s);
}

void $Cont$__serialize__($Cont self, $Serial$state state) {
    // Empty
}

$Cont $Cont$__deserialize__($Cont self, $Serial$state state) {
    return $DNEW($Cont,state);
}

////////////////////////////////////////////////////////////////////////////////////////

void $ConstCont$__init__($ConstCont $this, $WORD val, $Cont cont) {
    $this->val = val;
    $this->cont = cont;
}

$bool $ConstCont$__bool__($ConstCont self) {
  return $True;
}

$str $ConstCont$__str__($ConstCont self) {
  char *s;
  asprintf(&s,"<$ConstCont object at %p>",self);
  return to$str(s);
}

void $ConstCont$__serialize__($ConstCont self, $Serial$state state) {
    $step_serialize(self->val,state);
    $step_serialize(self->cont,state);
}

$ConstCont $ConstCont$__deserialize__($ConstCont self, $Serial$state state) {
    $ConstCont res = $DNEW($ConstCont,state);
    res->val = $step_deserialize(state);
    res->cont = $step_deserialize(state);
    return res;
}

$R $ConstCont$__call__($ConstCont $this, $WORD _ignore) {
    $Cont cont = $this->cont;
    return cont->$class->__call__(cont, $this->val);
}

$Cont $CONSTCONT($WORD val, $Cont cont){
    $ConstCont obj = malloc(sizeof(struct $ConstCont));
    obj->$class = &$ConstCont$methods;
    $ConstCont$methods.__init__(obj, val, cont);
    return ($Cont)obj;
}

////////////////////////////////////////////////////////////////////////////////////////

struct $Msg$class $Msg$methods = {
    MSG_HEADER,
    UNASSIGNED,
    NULL,
    $Msg$__init__,
    $Msg$__serialize__,
    $Msg$__deserialize__,
    $Msg$__bool__,
    $Msg$__str__,
    $Msg$__str__
};

struct $Actor$class $Actor$methods = {
    ACTOR_HEADER,
    UNASSIGNED,
    NULL,
    $Actor$__init__,
    $Actor$__serialize__,
    $Actor$__deserialize__,
    $Actor$__bool__,
    $Actor$__str__,
    $Actor$__str__,
    $Actor$__resume__
};

struct $Catcher$class $Catcher$methods = {
    CATCHER_HEADER,
    UNASSIGNED,
    NULL,
    $Catcher$__init__,
    $Catcher$__serialize__,
    $Catcher$__deserialize__,
    $Catcher$__bool__,
    $Catcher$__str__,
    $Catcher$__str__
};

struct $Cont$class $Cont$methods = {
    CONT_HEADER,
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    $Cont$__serialize__,
    $Cont$__deserialize__,
    $Cont$__bool__,
    $Cont$__str__,
    $Cont$__str__,
    NULL
};

struct $ConstCont$class $ConstCont$methods = {
    "$ConstCont",
    UNASSIGNED,
    NULL,
    $ConstCont$__init__,
    $ConstCont$__serialize__,
    $ConstCont$__deserialize__,
    $ConstCont$__bool__,
    $ConstCont$__str__,
    $ConstCont$__str__,
    $ConstCont$__call__
};

////////////////////////////////////////////////////////////////////////////////////////

// Atomically enqueue actor "a" onto the right ready-queue, either a thread
// local one or the "default" of 0 which is a shared queue among all worker
// threads. The index is offset by 1 so worker thread 0 is at index 1.
int ENQ_ready($Actor a) {
    int i = a->$affinity;
    spinlock_lock(&rqs[i].lock);
    if (rqs[i].head) {
        $Actor x = rqs[i].head;
        while (x->$next)
            x = x->$next;
        x->$next = a;
    } else {
        rqs[i].head = a;
    }
    a->$next = NULL;
    spinlock_unlock(&rqs[i].lock);
    // If we enqueue to someone who is not us, immediately wake them up...
    int our_wtid = (int)pthread_getspecific(pkey_wtid);
    if (our_wtid != i)
        wake_wt(i);
    return i;
}

// Atomically dequeue and return the first actor from a ready-queue, first
// dequeueing from the thread specific queue and second from the global shared
// readyQ or return NULL if no work is found.
$Actor _DEQ_ready(int idx) {
    $Actor res = NULL;
    if (rqs[idx].head == NULL)
        return res;

    spinlock_lock(&rqs[idx].lock);
    res = rqs[idx].head;
    if (res) {
        rqs[idx].head = res->$next;
        res->$next = NULL;
    }
    spinlock_unlock(&rqs[idx].lock);
    return res;
}
$Actor DEQ_ready(int idx) {
    $Actor res = _DEQ_ready(idx);
    if (res)
        return res;

    res = _DEQ_ready(0);
    return res;
}

// Atomically enqueue message "m" onto the queue of actor "a", 
// return true if the queue was previously empty.
bool ENQ_msg($Msg m, $Actor a) {
    bool did_enq = true;
    spinlock_lock(&a->$msg_lock);
    m->$next = NULL;
    if (a->$msg) {
        $Msg x = a->$msg;
        while (x->$next)
            x = x->$next;
        x->$next = m;
        did_enq = false;
    } else {
        a->$msg = m;
    }
    spinlock_unlock(&a->$msg_lock);
    return did_enq;
}

// Atomically dequeue the first message from the queue of actor "a",
// return true if the queue still holds messages.
bool DEQ_msg($Actor a) {
    bool has_more = false;
    spinlock_lock(&a->$msg_lock);
    if (a->$msg) {
        $Msg x = a->$msg;
        a->$msg = x->$next;
        x->$next = NULL;
        has_more = a->$msg != NULL;
    }
    spinlock_unlock(&a->$msg_lock);
    return has_more;
}

// Atomically add actor "a" to the waiting list of messasge "m" if it is not frozen (and return true),
// else immediately return false.
bool ADD_waiting($Actor a, $Msg m) {
    bool did_add = false;
    spinlock_lock(&m->$wait_lock);
    if (m->$cont) {
        a->$next = m->$waiting;
        m->$waiting = a;
        did_add = true;
    }
    spinlock_unlock(&m->$wait_lock);
    return did_add;
}

// Atomically freeze message "m" and return its list of waiting actors. 
$Actor FREEZE_waiting($Msg m) {
    spinlock_lock(&m->$wait_lock);
    m->$cont = NULL;
    spinlock_unlock(&m->$wait_lock);
    $Actor res = m->$waiting;
    m->$waiting = NULL;
    return res;
}

// Atomically enqueue timed message "m" onto the global timer-queue, at position
// given by "m->baseline".
bool ENQ_timed($Msg m) {
    time_t m_baseline = m->$baseline;
    bool new_head = false;
    spinlock_lock(&timerQ_lock);
    $Msg x = timerQ;
    if (x && x->$baseline <= m_baseline) {
        $Msg next = x->$next;
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
$Msg DEQ_timed(time_t now) {
    spinlock_lock(&timerQ_lock);
    $Msg res = timerQ;
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
$R $DONE$__call__($Cont $this, $WORD val) {
    return $R_DONE(val);
}

$bool $Done$__bool__($Cont self) {
  return $True;
}

$str $Done$__str__($Cont self) {
  char *s;
  asprintf(&s,"<$Done object at %p>",self);
  return to$str(s);
}

void $Done__serialize__($Cont self, $Serial$state state) {
  return;
}

$Cont $Done__deserialize__($Cont self, $Serial$state state) {
  $Cont res = $DNEW($Cont,state);
  res->$class = &$Done$methods;
  return res;
}

struct $Cont$class $Done$methods = {
    "$Done",
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    $Done__serialize__,
    $Done__deserialize__,
    $Done$__bool__,
    $Done$__str__,
    $Done$__str__,
    ($R (*)($Cont, ...))$DONE$__call__
};
struct $Cont $Done$instance = {
    &$Done$methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $InitRoot$__call__ ($Cont $this, $WORD val) {
    typedef $R(*ROOT__init__t)($Actor, $Env, $Cont);    // Assumed type of the ROOT actor's __init__ method
    return ((ROOT__init__t)root_actor->$class->__init__)(root_actor, env_actor, ($Cont)val);
}

struct $Cont$class $InitRoot$methods = {
    "$InitRoot",
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    $Cont$__serialize__,
    $Cont$__deserialize__,
    $Cont$__bool__,
    $Cont$__str__,
    $Cont$__str__,
    ($R (*)($Cont, ...))$InitRoot$__call__
};
struct $Cont $InitRoot$cont = {
    &$InitRoot$methods
};
////////////////////////////////////////////////////////////////////////////////////////

void dummy_callback(queue_callback_args * qca) { }

void create_db_queue(long key) {
    int ret = remote_create_queue_in_txn(MSG_QUEUE, ($WORD)key, NULL, db);
    rtsd_printf("#### Create queue %ld returns %d", key, ret);
    queue_callback * qc = get_queue_callback(dummy_callback);
	int64_t prev_read_head = -1, prev_consume_head = -1;
	ret = remote_subscribe_queue(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, qc, &prev_read_head, &prev_consume_head, db);
    rtsd_printf("   # Subscribe queue %ld returns %d", key, ret);
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

void PUSH_outgoing($Actor self, $Msg m) {
    m->$next = self->$outgoing;
    self->$outgoing = m;
}

void PUSH_catcher($Actor a, $Catcher c) {
    c->$next = a->$catcher;
    a->$catcher = c;
}

$Catcher POP_catcher($Actor a) {
    $Catcher c = a->$catcher;
    a->$catcher = c->$next;
    c->$next = NULL;
    return c;
}

$Msg $ASYNC($Actor to, $Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    time_t baseline = 0;
    $Msg m = $NEW($Msg, to, cont, baseline, &$Done$instance);
    if (self) {                                         // $ASYNC called by actor code
        m->$baseline = self->$msg->$baseline;
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

$Msg $AFTER($int sec, $Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    rtsd_printf("# AFTER by %ld", self->$globkey);
    time_t baseline = self->$msg->$baseline + sec->val * 1000000;
    $Msg m = $NEW($Msg, self, cont, baseline, &$Done$instance);
    PUSH_outgoing(self, m);
    return m;
}

$R $AWAIT($Msg m, $Cont cont) {
    return $R_WAIT(cont, m);
}

void $PUSH($Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    $Catcher c = $NEW($Catcher, cont);
    PUSH_catcher(self, c);
}

void $POP() {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    POP_catcher(self);
}

// Actually send all buffered messages of the sender
void FLUSH_outgoing($Actor self, uuid_t *txnid) {
    rtsd_printf("#### FLUSH_outgoing messages from %ld", self->$globkey);
    $Msg prev = NULL;
    $Msg m = self->$outgoing;
    self->$outgoing = NULL;
    while (m) {
        $Msg next = m->$next;
        m->$next = prev;
        prev = m;
        m = next;
    }
    m = prev;
    while (m) {
        $Msg next = m->$next;
        m->$next = NULL;
        long dest;
        if (m->$baseline == self->$msg->$baseline) {
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
        if (db) {
            int ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)dest, txnid, db);
            if (dest) {
                rtsd_printf("   # enqueue msg %ld to queue %ld returns %d", m->$globkey, dest, ret);
            } else {
                rtsd_printf("   # enqueue msg %ld to TIMER_QUEUE returns %d", m->$globkey, ret);
            }
        }
        m = next;
    }
}

time_t next_timeout() {
    return timerQ ? timerQ->$baseline : 0;
}

void handle_timeout() {
    time_t now = current_time();
    $Msg m = DEQ_timed(now);
    if (m) {
        rtsd_printf("## Dequeued timed msg with baseline %ld (now is %ld)", m->$baseline, now);
        if (ENQ_msg(m, m->$to)) {
            int wtid = ENQ_ready(m->$to);
            wake_wt(wtid);
        }
        if (db) {
            uuid_t *txnid = remote_new_txn(db);
            timer_consume_hd++;

            long key = TIMER_QUEUE;
            snode_t *m_start, *m_end;
            int entries_read = 0;
            int64_t read_head = -1;
            int ret0 = remote_read_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, NULL, db);
            rtsd_printf("   # dummy read msg from TIMER_QUEUE returns %d, entries read: %d", ret0, entries_read);

            int ret = remote_consume_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, read_head, txnid, db);
            rtsd_printf("   # consume msg %ld from TIMER_QUEUE returns %d", m->$globkey, ret);
            int ret2 = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)m->$to->$globkey, txnid, db);
            rtsd_printf("   # (timed) enqueue msg %ld to queue %ld returns %d", m->$globkey, m->$to->$globkey, ret2);
            remote_commit_txn(txnid, db);
            rtsd_printf("############## Commit");
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////

$dict globdict = NULL;

$WORD try_globdict($WORD w) {
    int key = (int)(long)w;
    $WORD obj = $dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
    return obj;
}

long read_queued_msg(long key, int64_t *read_head) {
    snode_t *m_start, *m_end;
    int entries_read = 0;
    
    int ret = remote_read_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, 
                                       1, &entries_read, read_head, &m_start, &m_end, NULL, db);
    rtsd_printf("   # read msg from queue %ld returns %d, entries read: %d", key, ret, entries_read);

    if (!entries_read)
        return 0;
    db_row_t *r = (db_row_t*)m_start->value;
    rtsd_printf("# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
    return (long)r->column_array[0];
}

typedef struct BlobHd {           // C.f. $ROW
    int class_id;
    int blob_size;
} BlobHd;

$ROW extract_row($WORD *blob, size_t blob_size) {
    int words_left = blob_size / sizeof($WORD);
    if (words_left == 0)
        return NULL;
    BlobHd* head = (BlobHd*)blob;
    $ROW fst = malloc(sizeof(struct $ROW) + head->blob_size*sizeof($WORD));
    $ROW row = fst;
    while (1) {
        long size = 1 + head->blob_size;
        memcpy(&row->class_id, blob, size*sizeof($WORD));
        blob += size;
        words_left -= size;
        if (words_left == 0)
            break;
        head = (BlobHd*)blob;
        row->next = malloc(sizeof(struct $ROW) + head->blob_size*sizeof($WORD));
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

void print_msg($Msg m) {
    rtsd_printf("==== Message %p", m);
    rtsd_printf("     next: %p", m->$next);
    rtsd_printf("     to: %p", m->$to);
    rtsd_printf("     cont: %p", m->$cont);
    rtsd_printf("     waiting: %p", m->$waiting);
    rtsd_printf("     baseline: %ld", m->$baseline);
    rtsd_printf("     value: %p", m->$value);
    rtsd_printf("     globkey: %ld", m->$globkey);
}

void print_actor($Actor a) {
    rtsd_printf("==== Actor %p", a);
    rtsd_printf("     next: %p", a->$next);
    rtsd_printf("     msg: %p", a->$msg);
    rtsd_printf("     outgoing: %p", a->$outgoing);
    rtsd_printf("     waitsfor: %p", a->$waitsfor);
    rtsd_printf("     consume_hd: %ld", (long)a->$consume_hd);
    rtsd_printf("     catcher: %p", a->$catcher);
    rtsd_printf("     globkey: %ld", a->$globkey);
}

void deserialize_system(snode_t *actors_start) {
    rtsd_printf("Deserializing system");
    snode_t *msgs_start, *msgs_end;
    remote_read_full_table_in_txn(&msgs_start, &msgs_end, MSGS_TABLE, NULL, db);
    
    globdict = $NEW($dict,($Hashable)$Hashable$int$witness,NULL,NULL);

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
            $Msg msg = ($Msg)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            msg->$globkey = key;
            $dict_setitem(globdict, ($Hashable)$Hashable$int$witness, to$int(key), msg);
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
            $dict_setitem(globdict, ($Hashable)$Hashable$int$witness, to$int(key), act);
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
            $Msg msg = ($Msg)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
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
            $Actor act = ($Actor)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
            rtsd_printf("####### Deserializing actor %p = %ld of class %s = %d", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            print_rows(row);
            $glob_deserialize(($Serializable)act, row, try_globdict);

            $Msg m = act->$waitsfor;
            if (m && m->$cont) {
                ADD_waiting(act, m);
                rtsd_printf("# Adding Actor %ld to wait for Msg %ld", act->$globkey, m->$globkey);
            }
            else {
                act->$waitsfor = NULL;
            }

            rtsd_printf("#### Reading msgs queue %ld contents:", key);
            queue_callback * qc = get_queue_callback(dummy_callback);
            int64_t prev_read_head = -1, prev_consume_head = -1;
            int ret = remote_subscribe_queue(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, qc, &prev_read_head, &prev_consume_head, db);
            rtsd_printf("   # Subscribe queue %ld returns %d", key, ret);
            while (1) {
                long msg_key = read_queued_msg(key, &prev_read_head);
                if (!msg_key)
                    break;
                m = $dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(msg_key), NULL);
                rtsd_printf("# Adding Msg %ld to Actor %ld", m->$globkey, act->$globkey);
                ENQ_msg(m, act);
            }
            if (act->$msg && !act->$waitsfor) {
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
        $Actor act = ($Actor)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
        rtsd_printf("####### Resuming actor %p = %ld of class %s = %d", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
        act->$class->__resume__(act);
    }

    rtsd_printf("#### Reading timer queue contents:");
    time_t now = current_time();
    queue_callback * qc = get_queue_callback(dummy_callback);
	int64_t prev_read_head = -1, prev_consume_head = -1;
	int ret = remote_subscribe_queue(TIMER_QUEUE, 0, 0, MSG_QUEUE, TIMER_QUEUE, qc, &prev_read_head, &prev_consume_head, db);
    rtsd_printf("   # Subscribe queue 0 returns %d", ret);
    while (1) {
        long msg_key = read_queued_msg(TIMER_QUEUE, &prev_read_head);
        if (!msg_key)
            break;
        $Msg m = $dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(msg_key), NULL);
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
    env_actor  = ($Env)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(-11), NULL);
    root_actor = ($Actor)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(-12), NULL);
    globdict = NULL;
    rtsd_printf("System deserialized");
}

$WORD try_globkey($WORD obj) {
    $Serializable$class c = (($Serializable)obj)->$class;
    if (c->$class_id == MSG_ID) {
        long key = (($Msg)obj)->$globkey;
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

    int ret = remote_insert_in_txn(column, 2, 1, 1, blob, total*sizeof($WORD), table, txnid, db);
    rtsd_printf("   # insert to table %ld, row %ld, returns %d", (long)table, key, ret);
}

void serialize_msg($Msg m, uuid_t *txnid) {
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

    $Msg out = a->$outgoing;
    while (out) {
        serialize_msg(out, txnid);
        out = out->$next;
    }
}

void serialize_state_shortcut($Actor a) {
    if (db) {
        uuid_t * txnid = remote_new_txn(db);
        serialize_actor(a, txnid);
        remote_commit_txn(txnid, db);
    }
}

void BOOTSTRAP(int argc, char *argv[]) {
    $list args = $list$new(NULL,NULL);
    for (int i=0; i< argc; i++)
      $list_append(args,to$str(argv[i]));

    env_actor = $Env$newact($WorldAuth$new(), args);

    root_actor = $ROOT();                           // Assumed to return $NEWACTOR(X) for the selected root actor X
    time_t now = current_time();
    $Msg m = $NEW($Msg, root_actor, &$InitRoot$cont, now, &$Done$instance);
    if (db) {
        int ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)root_actor->$globkey, NULL, db);
        rtsd_printf("   # enqueue bootstrap msg %ld to root actor queue %ld returns %d", m->$globkey, root_actor->$globkey, ret);
    }
    if (ENQ_msg(m, root_actor)) {
        ENQ_ready(root_actor);
    }

}


////////////////////////////////////////////////////////////////////////////////////////

void wt_stop_cb(uv_async_t *ev) {
    int wtid = (int)pthread_getspecific(pkey_wtid);
    uv_check_stop(&work_ev[wtid-1]);
    uv_stop(get_uv_loop());
}

void wt_wake_cb(uv_async_t *ev) {
    // We just wake up the uv loop here if it is blocked waiting for IO, real
    // work is run later when wt_work_cb is called as part of the "check" phase.
}

void wt_work_cb(uv_check_t *ev) {
    int wtid = (int)pthread_getspecific(pkey_wtid);

    struct timespec ts_start, ts1, ts2, ts3;
    long long int runtime = 0;

    clock_gettime(CLOCK_MONOTONIC, &ts_start);
    while (true) {
        if (rts_exit) {
            return;
        }
        $Actor current = DEQ_ready(wtid);
        if (!current)
            return;

        wake_wt(0);

        pthread_setspecific(self_key, current);
        $Msg m = current->$msg;
        $Cont cont = m->$cont;
        $WORD val = m->$value;

        clock_gettime(CLOCK_MONOTONIC, &ts1);
        wt_stats[wtid].state = WT_Working;

        rtsd_printf("## Running actor %ld : %s", current->$globkey, current->$class->$GCINFO);
        $R r = cont->$class->__call__(cont, val);

        clock_gettime(CLOCK_MONOTONIC, &ts2);
        long long int diff = (ts2.tv_sec * 1000000000 + ts2.tv_nsec) - (ts1.tv_sec * 1000000000 + ts1.tv_nsec);

        wt_stats[wtid].conts_count++;
        wt_stats[wtid].conts_sum += diff;

        if      (diff < 100)              { wt_stats[wtid].conts_100ns++; }
        else if (diff < 1   * 1000)       { wt_stats[wtid].conts_1us++; }
        else if (diff < 10  * 1000)       { wt_stats[wtid].conts_10us++; }
        else if (diff < 100 * 1000)       { wt_stats[wtid].conts_100us++; }
        else if (diff < 1   * 1000000)    { wt_stats[wtid].conts_1ms++; }
        else if (diff < 10  * 1000000)    { wt_stats[wtid].conts_10ms++; }
        else if (diff < 100 * 1000000)    { wt_stats[wtid].conts_100ms++; }
        else if (diff < 1   * 1000000000) { wt_stats[wtid].conts_1s++; }
        else if (diff < (long long int)10  * 1000000000) { wt_stats[wtid].conts_10s++; }
        else if (diff < (long long int)100 * 1000000000) { wt_stats[wtid].conts_100s++; }
        else                              { wt_stats[wtid].conts_inf++; }

        switch (r.tag) {
        case $RDONE: {
            if (db) {
                uuid_t * txnid = remote_new_txn(db);
                current->$consume_hd++;
                serialize_actor(current, txnid);
                FLUSH_outgoing(current, txnid);
                serialize_msg(current->$msg, txnid);

                long key = current->$globkey;
                snode_t *m_start, *m_end;
                int entries_read = 0;
                int64_t read_head = -1;
                int ret0 = remote_read_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, NULL, db);
                rtsd_printf("   # dummy read msg from queue %ld returns %d, entries read: %d", key, ret0, entries_read);

                int ret = remote_consume_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, read_head, txnid, db);
                rtsd_printf("   # consume msg %ld from queue %ld returns %d", m->$globkey, key, ret);
                remote_commit_txn(txnid, db);
                rtsd_printf("############## Commit");
            } else {
                FLUSH_outgoing(current, NULL);
            }

            m->$value = r.value;                 // m->value holds the response,
            $Actor b = FREEZE_waiting(m);        // so set m->cont = NULL and stop further m->waiting additions
            while (b) {
                b->$msg->$value = r.value;
                b->$waitsfor = NULL;
                $Actor c = b->$next;
                ENQ_ready(b);
                rtsd_printf("## Waking up actor %ld : %s", b->$globkey, b->$class->$GCINFO);
                b = c;
            }
            rtsd_printf("## DONE actor %ld : %s", current->$globkey, current->$class->$GCINFO);
            if (DEQ_msg(current)) {
                ENQ_ready(current);
            }
            break;
        }
        case $RCONT: {
            m->$cont = r.cont;
            m->$value = r.value;
            rtsd_printf("## CONT actor %ld : %s", current->$globkey, current->$class->$GCINFO);
            ENQ_ready(current);
            break;
        }
        case $RFAIL: {
            $Catcher c = POP_catcher(current);
            m->$cont = c->$cont;
            m->$value = r.value;
            ENQ_ready(current);
            break;
        }
        case $RWAIT: {
            if (db) {
                uuid_t * txnid = remote_new_txn(db);
                serialize_actor(current, txnid);
                FLUSH_outgoing(current, txnid);
                serialize_msg(current->$msg, txnid);
                remote_commit_txn(txnid, db);
                rtsd_printf("############## Commit");
            } else {
                FLUSH_outgoing(current, NULL);
            }
            m->$cont = r.cont;
            $Msg x = ($Msg)r.value;
            if (ADD_waiting(current, x)) {      // x->cont != NULL: x is still being processed so current was added to x->waiting
                rtsd_printf("## AWAIT actor %ld : %s", current->$globkey, current->$class->$GCINFO);
                current->$waitsfor = x;
            } else {                            // x->cont == NULL: x->value holds the final response, current is not in x->waiting
                rtsd_printf("## AWAIT/wakeup actor %ld : %s", current->$globkey, current->$class->$GCINFO);
                m->$value = x->$value;
                ENQ_ready(current);
            }
            break;
        }
        }
        pthread_setspecific(self_key, NULL);

        clock_gettime(CLOCK_MONOTONIC, &ts3);
        diff = (ts3.tv_sec * 1000000000 + ts3.tv_nsec) - (ts2.tv_sec * 1000000000 + ts2.tv_nsec);
        wt_stats[wtid].bkeep_count++;
        wt_stats[wtid].bkeep_sum += diff;

        if      (diff < 100)              { wt_stats[wtid].bkeep_100ns++; }
        else if (diff < 1   * 1000)       { wt_stats[wtid].bkeep_1us++; }
        else if (diff < 10  * 1000)       { wt_stats[wtid].bkeep_10us++; }
        else if (diff < 100 * 1000)       { wt_stats[wtid].bkeep_100us++; }
        else if (diff < 1   * 1000000)    { wt_stats[wtid].bkeep_1ms++; }
        else if (diff < 10  * 1000000)    { wt_stats[wtid].bkeep_10ms++; }
        else if (diff < 100 * 1000000)    { wt_stats[wtid].bkeep_100ms++; }
        else if (diff < 1   * 1000000000) { wt_stats[wtid].bkeep_1s++; }
        else if (diff < (long long int)10  * 1000000000) { wt_stats[wtid].bkeep_10s++; }
        else if (diff < (long long int)100 * 1000000000) { wt_stats[wtid].bkeep_100s++; }
        else                              { wt_stats[wtid].bkeep_inf++; }

        wt_stats[wtid].state = WT_Idle;

        runtime = (ts3.tv_sec * 1000000000 + ts3.tv_nsec) - (ts_start.tv_sec * 1000000000 + ts_start.tv_nsec);
        // run for max 20ms before yielding to IO
        // NOTE: since we are not preemptive, a single long continuation can
        // exceed this cap
        if (runtime > 20*1000000)
            break;
    }

    // if there's more work, wake up ourselves again to process more but
    // interleave with some IO
    uv_async_send(&wake_ev[wtid-1]);
}

void *main_loop(void *idx) {
    char tname[11]; // Enough for "Worker XXX\0"
    int wtid = (int)idx;
    snprintf(tname, sizeof(tname), "Worker %d", wtid);
#if defined(IS_MACOS)
    pthread_setname_np(tname);
#else
    pthread_setname_np(pthread_self(), tname);
#endif
    uv_loop_t *uv_loop = uv_loops[wtid-1];
    pthread_setspecific(pkey_wtid, (void *)wtid);
    pthread_setspecific(pkey_uv_loop, (void *)uv_loop);

    uv_check_init(uv_loop, &work_ev[wtid-1]);
    uv_check_start(&work_ev[wtid-1], (uv_check_cb)wt_work_cb);

    int r = uv_run(uv_loop, UV_RUN_DEFAULT);
    wt_stats[wtid].state = WT_NoExist;
    rtsd_printf("Exiting...");
}

////////////////////////////////////////////////////////////////////////////////////////

void $register_rts () {
  $register_force(MSG_ID,&$Msg$methods);
  $register_force(ACTOR_ID,&$Actor$methods);
  $register_force(CATCHER_ID,&$Catcher$methods);
  $register_force(CLOS_ID,&$function$methods);
  $register_force(CONT_ID,&$Cont$methods);
  $register_force(DONE_ID,&$Done$methods);
  $register_force(CONSTCONT_ID,&$ConstCont$methods);
  $register(&$Done$methods);
  $register(&$InitRoot$methods);
  $register(&$Env$methods);
  $register(&$Connection$methods);
}
 
////////////////////////////////////////////////////////////////////////////////////////

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


const char* stats_to_json () {
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    yyjson_mut_obj_add_str(doc, root, "name", appname);

    yyjson_mut_obj_add_int(doc, root, "pid", pid);

    struct timeval tv;
    struct tm tm;
    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &tm);
    char dt[32];    // = "YYYY-MM-ddTHH:mm:ss.SSS+0000";
    strftime(dt, 32, "%Y-%m-%dT%H:%M:%S.000%z", &tm);
    sprintf(dt + 20, "%03hu%s", (unsigned short)(tv.tv_usec / 1000), dt + 23);

    yyjson_mut_obj_add_str(doc, root, "datetime", dt);

    // Worker threads
    yyjson_mut_val *j_stat = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, root, "wt", j_stat);
    for (unsigned int i = 0; i < num_wthreads; i++) {
        yyjson_mut_val *j_wt = yyjson_mut_obj(doc);
        yyjson_mut_obj_add_val(doc, j_stat, wt_stats[i].key, j_wt);
        yyjson_mut_obj_add_str(doc, j_wt, "state",       WT_State_name[wt_stats[i].state]);
        yyjson_mut_obj_add_int(doc, j_wt, "sleeps",      wt_stats[i].sleeps);
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

#define X(ops_name) \
    dbc_ops_stats_to_json(doc, j_dbc, dbc_stats.ops_name);
LIST_OF_DBC_OPS
#undef X

    const char *json = yyjson_mut_write(doc, 0, NULL);
    yyjson_mut_doc_free(doc);
    return json;
}

const char* db_membership_to_json () {
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    yyjson_mut_obj_add_str(doc, root, "name", appname);

    yyjson_mut_obj_add_int(doc, root, "pid", pid);

    struct timeval tv;
    struct tm tm;
    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &tm);
    char dt[32];    // = "YYYY-MM-ddTHH:mm:ss.SSS+0000";
    strftime(dt, 32, "%Y-%m-%dT%H:%M:%S.000%z", &tm);
    sprintf(dt + 20, "%03hu%s", (unsigned short)(tv.tv_usec / 1000), dt + 23);

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

const char* actors_to_json () {
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    yyjson_mut_obj_add_str(doc, root, "name", appname);

    yyjson_mut_obj_add_int(doc, root, "pid", pid);

    struct timeval tv;
    struct tm tm;
    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &tm);
    char dt[32];    // = "YYYY-MM-ddTHH:mm:ss.SSS+0000";
    strftime(dt, 32, "%Y-%m-%dT%H:%M:%S.000%z", &tm);
    sprintf(dt + 20, "%03hu%s", (unsigned short)(tv.tv_usec / 1000), dt + 23);

    yyjson_mut_obj_add_str(doc, root, "datetime", dt);

    // Actual topology
    yyjson_mut_val *j_actors = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, root, "actors", j_actors);
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

    const char *json = yyjson_mut_write(doc, 0, NULL);
    yyjson_mut_doc_free(doc);
    return json;
}


void *$mon_log_loop(void *period) {
    log_info("Starting monitor log, with %d second(s) period, to: %s\n", (uint)period, mon_log_path);

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
            log_info("Shutting down RTS Monitor log thread.\n");
            break;
        }

        pthread_mutex_lock(&rts_exit_lock);
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += (uint)period;
        pthread_cond_timedwait(&rts_exit_signal, &rts_exit_lock, &ts);
        pthread_mutex_unlock(&rts_exit_lock);
    }
    fclose(f);
    return NULL;
}


void *$mon_socket_loop() {
    log_info("Starting monitor socket listen on %s\n", mon_socket_path);

#if defined(IS_MACOS)
    pthread_setname_np("Monitor Socket");
#else
    pthread_setname_np(pthread_self(), "Monitor Socket");
#endif

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
                    log_info("Mon socket: Error reading netstring: %d\n", r);
                    break;
                }

                if (memcmp(str, "actors", len) == 0) {
                    const char *json = actors_to_json();
                    char *send_buf = malloc(strlen(json)+14); // 14 = maximum digits for length is 9 (999999999) + : + ; + \0
                    sprintf(send_buf, "%lu:%s,", strlen(json), json);
                    int send_res = send(client_sock, send_buf, strlen(send_buf), 0);
                    free((void *)json);
                    free((void *)send_buf);
                    if (send_res < 0) {
                        log_info("Mon socket: Error sending\n");
                        break;
                    }
                }

                if (memcmp(str, "membership", len) == 0) {
                    const char *json = db_membership_to_json();
                    char *send_buf = malloc(strlen(json)+14); // 14 = maximum digits for length is 9 (999999999) + : + ; + \0
                    sprintf(send_buf, "%lu:%s,", strlen(json), json);
                    int send_res = send(client_sock, send_buf, strlen(send_buf), 0);
                    free((void *)json);
                    free((void *)send_buf);
                    if (send_res < 0) {
                        log_info("Mon socket: Error sending\n");
                        break;
                    }
                }

                if (memcmp(str, "WTS", len) == 0) {
                    const char *json = stats_to_json();
                    char *send_buf = malloc(strlen(json)+14); // 14 = maximum digits for length is 9 (999999999) + : + ; + \0
                    sprintf(send_buf, "%lu:%s,", strlen(json), json);
                    int send_res = send(client_sock, send_buf, strlen(send_buf), 0);
                    free((void *)json);
                    free((void *)send_buf);
                    if (send_res < 0) {
                        log_info("Mon socket: Error sending\n");
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
}

void rts_shutdown() {
    rts_exit = 1;
    for (int i = 0; i < num_wthreads; i++) {
        uv_async_send(&stop_ev[i]);
    }
}


void sigint_handler(int signum) {
    if (rts_exit == 0) {
        log_info("Received SIGINT, shutting down gracefully...\n");
        rts_shutdown();
    } else {
        log_info("Received SIGINT during graceful shutdown, exiting immediately\n");
        exit(return_val);
    }
}

void sigterm_handler(int signum) {
    if (rts_exit == 0) {
        log_info("Received SIGTERM, shutting down gracefully...\n");
        rts_shutdown();
    } else {
        log_info("Received SIGTERM during graceful shutdown, exiting immediately\n");
        exit(return_val);
    }
}

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


int main(int argc, char **argv) {
    uint ddb_no_host = 0;
    char **ddb_host = NULL;
    char *rts_host = "localhost";
    int ddb_port = 32000;
    int ddb_replication = 3;
    int rts_node_id = -1;
    int rts_rack_id = -1;
    int rts_dc_id = -1;
    int new_argc = argc;
    int cpu_pin;
    long num_cores = sysconf(_SC_NPROCESSORS_ONLN);
    num_wthreads = num_cores;
    bool mon_on_exit = false;
    char *log_path = NULL;
    FILE *logf = NULL;
    bool log_stderr = false;

    appname = argv[0];
    pid_t pid = getpid();

    // Do line buffered output
    setlinebuf(stdout);

    // Signal handling
    struct sigaction sa_int, sa_pipe, sa_term;
    sigfillset(&sa_int.sa_mask);
    sigfillset(&sa_pipe.sa_mask);
    sigfillset(&sa_term.sa_mask);
    sa_int.sa_flags = SA_RESTART;
    sa_pipe.sa_flags = SA_RESTART;
    sa_term.sa_flags = SA_RESTART;

    // Ignore SIGPIPE, like we get if the other end talking to us on the Monitor
    // socket (which is a Unix domain socket) goes away.
    sa_pipe.sa_handler = SIG_IGN;
    // Handle SIGINT & SIGTERM
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


    pthread_key_create(&self_key, NULL);
    pthread_setspecific(self_key, NULL);
    pthread_key_create(&pkey_wtid, NULL);
    pthread_key_create(&pkey_uv_loop, NULL);

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
    char **new_argv = malloc((argc+1) * sizeof *new_argv);
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
                ddb_host = realloc(ddb_host, ++ddb_no_host * sizeof *ddb_host);
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
                rts_host = strdup(optarg);
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
        }
    }
    new_argv[new_argc] = NULL;

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

    if (num_wthreads > MAX_WTHREADS) {
        fprintf(stderr, "ERROR: Maximum of %d worker threads supported.\n", MAX_WTHREADS);
        fprintf(stderr, "HINT: Run this program with fewer worker threads: %s --rts-wthreads %d\n", argv[0], MAX_WTHREADS);
        exit(1);
    }
    // Determine number of worker threads, normally 1:1 per CPU thread / core
    // For low core count systems we do a minimum of 4 worker threads
    if (num_wthreads < 4) {
        num_wthreads = 4;
        cpu_pin = 0;
        log_info("Detected %ld CPUs: Using %ld worker threads, due to low CPU count. No CPU affinity used.", num_cores, num_wthreads);
    } else if (num_wthreads == num_cores) {
        cpu_pin = 1;
        log_info("Detected %ld CPUs: Using %ld worker threads for 1:1 mapping with CPU affinity set.", num_cores, num_wthreads);
    } else {
        cpu_pin = 0;
        log_info("Detected %ld CPUs: Using %ld worker threads. No CPU affinity used.", num_cores, num_wthreads);
    }

    // Zeroize statistics
    for (uint i=0; i < MAX_WTHREADS; i++) {
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
    init_dbc_stats();

    for (uint i=0; i < num_wthreads; i++) {
        uv_loop_t *loop = malloc(sizeof(uv_loop_t));
        check_uv_fatal(uv_loop_init(loop), "Error initializing libuv loop: ");
        uv_loops[i] = loop;

        check_uv_fatal(uv_async_init(uv_loops[i], &stop_ev[i], wt_stop_cb), "Error initializing libuv stop event: ");
        check_uv_fatal(uv_async_init(uv_loops[i], &wake_ev[i], wt_wake_cb), "Error initializing libuv wake event: ");
        check_uv_fatal(uv_async_send(&wake_ev[i]), "Error sending initial work event: ");
    }

    for (uint i=0; i <= MAX_WTHREADS; i++) {
        rqs[i].head = NULL;
        rqs[i].tail = NULL;
        rqs[i].count = 0;
    }

    $register_builtin();
    $__init__();
    $register_rts();
    $ROOTINIT();

    unsigned int seed;
    if (ddb_host) {
        GET_RANDSEED(&seed, 0);
        log_info("Starting distributed RTS node, host=%s, node_id=%d, rack_id=%d, datacenter_id=%d\n", rts_host, rts_node_id, rts_rack_id, rts_dc_id);
        log_info("Using distributed database backend replication factor of %d\n", ddb_replication);
        char ** seed_hosts = (char **) malloc(ddb_no_host * sizeof(char *));
        int * seed_ports = (int *) malloc(ddb_no_host * sizeof(int));

        for (int i=0; i<ddb_no_host; i++) {
            seed_hosts[i] = strdup(ddb_host[i]);
            seed_ports[i] = ddb_port;
            char *colon = strchr(seed_hosts[i], ':');
            if (colon) {
                *colon = '\0';
                seed_ports[i] = atoi(colon + 1);
            }
            log_info("Using distributed database backend (DDB): %s:%d\n", seed_hosts[i], seed_ports[i]);
        }
        db = get_remote_db(ddb_replication, rts_rack_id, rts_dc_id, rts_host, rts_node_id, ddb_no_host, seed_hosts, seed_ports, &seed);
        free(seed_hosts);
        free(seed_ports);
    }

    if (db) {
        snode_t* start_row = NULL, * end_row = NULL;
        log_info("Checking for existing actor state in DDB.\n");
        int no_items = remote_read_full_table_in_txn(&start_row, &end_row, ACTORS_TABLE, NULL, db);
        if (no_items > 0) {
            log_info("Found %d existing actors; Restoring actor state from DDB.\n", no_items);
            deserialize_system(start_row);
            log_info("Actor state restored from DDB.\n");
        } else {
            log_info("No previous state in DDB; Initializing database...\n");
            int indices[] = {0};
            db_schema_t* db_schema = db_create_schema(NULL, 1, indices, 1, indices, 0, indices, 0);
            create_db_queue(TIMER_QUEUE);
            timer_consume_hd = 0;
            BOOTSTRAP(new_argc, new_argv);
            log_info("Database intialization complete.\n");
        }
    } else {
        BOOTSTRAP(new_argc, new_argv);
    }

    cpu_set_t cpu_set;

    // RTS Monitor Log
    pthread_t mon_log_thread;
    if (mon_log_path) {
        pthread_create(&mon_log_thread, NULL, $mon_log_loop, (void *)mon_log_period);
        if (cpu_pin) {
            CPU_ZERO(&cpu_set);
            CPU_SET(0, &cpu_set);
            pthread_setaffinity_np(mon_log_thread, sizeof(cpu_set), &cpu_set);
        }
    }

    // RTS Monitor Socket
    pthread_t mon_socket_thread;
    if (mon_socket_path) {
        pthread_create(&mon_socket_thread, NULL, $mon_socket_loop, NULL);
        if (cpu_pin) {
            CPU_ZERO(&cpu_set);
            CPU_SET(0, &cpu_set);
            pthread_setaffinity_np(mon_socket_thread, sizeof(cpu_set), &cpu_set);
        }
    }

    // Start IO + worker threads
    pthread_t threads[1 + num_wthreads];

    // eventloop (our "old" IO thread)
    pthread_create(&threads[0], NULL, $eventloop, NULL);
    if (cpu_pin) {
        CPU_ZERO(&cpu_set);
        CPU_SET(0, &cpu_set);
        pthread_setaffinity_np(threads[0], sizeof(cpu_set), &cpu_set);
    }

    for(int idx = 1; idx < num_wthreads+1; idx++) {
        pthread_create(&threads[idx], NULL, main_loop, (void*)idx);
        // Index start at 1 and we pin wthreads to CPU 1...n
        // We use CPU 0 for misc threads, like IO / mon etc
        if (cpu_pin) {
            CPU_ZERO(&cpu_set);
            CPU_SET(idx, &cpu_set);
            pthread_setaffinity_np(threads[idx], sizeof(cpu_set), &cpu_set);
        }
    }

    // -- SHUTDOWN --

    // Join threads
    for(int idx = 1; idx < num_wthreads+1; idx++) {
        pthread_join(threads[idx], NULL);
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

    if (logf) {
        fclose(logf);
    }

    return return_val;
}
