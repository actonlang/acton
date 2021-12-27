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

#define MAX_WTHREADS 256

#include <unistd.h>
#include <pthread.h>
#include <stdio.h>
#include <stdarg.h>
#include <uuid/uuid.h>
#include <getopt.h>

#include "yyjson.h"
#include "rts.h"
#include "../builtin/env.h"

// We want to pass through unknown arguments, so tell getopt to ignore invalid
// (unknown to it) options
extern int opterr;
int opterr = 0;

#include "../backend/client_api.h"
#include "../backend/fastrand.h"

char rts_verbose = 0;
char rts_debug = 0;

long num_wthreads;

char *rts_mon_path = NULL;

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
 * RTS Verbose Printf = rtsv_printf
 * RTS Debug Printf   = rtsd_printf
 */
#define LOGPFX "**RTS** "
#define rtsv_printf(...) if (rts_verbose) printf(__VA_ARGS__)

#ifdef DEV
#define rtsd_printf(...) if (rts_debug) printf(__VA_ARGS__)
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
extern $R $ROOT($Env, $Cont);

$Actor root_actor = NULL;
$Env env_actor = NULL;

$Actor readyQ = NULL;
$Lock readyQ_lock;

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
pthread_mutex_t sleep_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t work_to_do = PTHREAD_COND_INITIALIZER;

void new_work() {
    // We are sometimes optimistically called, i.e. the caller sometimes does
    // not really know whether there is new work or not. We check and if there
    // is not, then there is no need to wake anyone up.
    if (!readyQ)
        return;

    pthread_mutex_lock(&sleep_lock);
    pthread_cond_signal(&work_to_do);
    pthread_mutex_unlock(&sleep_lock);
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
    rtsd_printf(LOGPFX "# New Actor %ld at %p of class %s\n", a->$globkey, a, a->$class->$GCINFO);
}

$bool $Actor$__bool__($Actor self) {
  return $True;
}

$str $Actor$__str__($Actor self) {
  char *s;
  asprintf(&s,"<$Actor object at %p>",self);
  return to$str(s);
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
    $Actor$__str__
};

struct $Catcher$class $Catcher$methods = {
    CATCHER_HEADER,
    UNASSIGNED,
    NULL,
    $Catcher$__init__,
    $Catcher$__serialize__,
    $Catcher$__deserialize__,
    $Catcher$__bool__,
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
    $ConstCont$__call__
};

////////////////////////////////////////////////////////////////////////////////////////

// Atomically enqueue actor "a" onto the global ready-queue.
void ENQ_ready($Actor a) {
    spinlock_lock(&readyQ_lock);
    if (readyQ) {
        $Actor x = readyQ;
        while (x->$next)
            x = x->$next;
        x->$next = a;
    } else {
        readyQ = a;
    }
    a->$next = NULL;
    spinlock_unlock(&readyQ_lock);
}

// Atomically dequeue and return the first actor from the global ready-queue, 
// or return NULL.
$Actor DEQ_ready() {
    spinlock_lock(&readyQ_lock);
    $Actor res = readyQ;
    if (res) {
        readyQ = res->$next;
        res->$next = NULL;
    }
    spinlock_unlock(&readyQ_lock);
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
    ($R (*)($Cont, ...))$DONE$__call__
};
struct $Cont $Done$instance = {
    &$Done$methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $NewRoot$__call__ ($Cont $this, $WORD val) {
    $Cont then = ($Cont)val;
    return $ROOT(env_actor, then);
}

struct $Cont$class $NewRoot$methods = {
    "$NewRoot",
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    $Cont$__serialize__,
    $Cont$__deserialize__,
    $Cont$__bool__,
    $Cont$__str__,
    ($R (*)($Cont, ...))$NewRoot$__call__
};
struct $Cont $NewRoot$cont = {
    &$NewRoot$methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $WriteRoot$__call__($Cont $this, $WORD val) {
    root_actor = ($Actor)val;
    return $R_DONE(val);
}

struct $Cont$class $WriteRoot$methods = {
    "$WriteRoot",
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    $Cont$__serialize__,
    $Cont$__deserialize__,
    $Cont$__bool__,
    $Cont$__str__,
    ($R (*)($Cont, ...))$WriteRoot$__call__
};
struct $Cont $WriteRoot$cont = {
    &$WriteRoot$methods
};
////////////////////////////////////////////////////////////////////////////////////////

void dummy_callback(queue_callback_args * qca) { }

void create_db_queue(long key) {
    int ret = remote_create_queue_in_txn(MSG_QUEUE, ($WORD)key, NULL, db);
    rtsd_printf(LOGPFX "#### Create queue %ld returns %d\n", key, ret);
    queue_callback * qc = get_queue_callback(dummy_callback);
	int64_t prev_read_head = -1, prev_consume_head = -1;
	ret = remote_subscribe_queue(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, qc, &prev_read_head, &prev_consume_head, db);
    rtsd_printf(LOGPFX "   # Subscribe queue %ld returns %d\n", key, ret);
}

void init_db_queue(long key) {
    if (db)
        create_db_queue(key);
}

void BOOTSTRAP(int argc, char *argv[]) {
    $list args = $list$new(NULL,NULL);
    for (int i=0; i< argc; i++)
      $list_append(args,to$str(argv[i]));

    env_actor = $NEW($Env, args);
    $Actor ancestor0 = $NEW($Actor);
    time_t now = current_time();
    $Msg m = $NEW($Msg, ancestor0, &$NewRoot$cont, now, &$WriteRoot$cont);

    if (db) {
        create_db_queue(env_actor->$globkey);
        create_db_queue(ancestor0->$globkey);
        int ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)ancestor0->$globkey, NULL, db);
        rtsd_printf(LOGPFX "   # enqueue bootstrap msg %ld to ancestor0 queue %ld returns %d\n", m->$globkey, ancestor0->$globkey, ret);
    }

    if (ENQ_msg(m, ancestor0)) {
        ENQ_ready(ancestor0);
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
           ENQ_ready(to);
           new_work();
        }
    }
    return m;
}

$Msg $AFTER($int sec, $Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    rtsd_printf(LOGPFX "# AFTER by %ld\n", self->$globkey);
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
    rtsd_printf(LOGPFX "#### FLUSH_outgoing messages from %ld\n", self->$globkey);
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
                rtsd_printf(LOGPFX "   # enqueue msg %ld to queue %ld returns %d\n", m->$globkey, dest, ret);
            } else {
                rtsd_printf(LOGPFX "   # enqueue msg %ld to TIMER_QUEUE returns %d\n", m->$globkey, ret);
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
        rtsd_printf(LOGPFX "## Dequeued timed msg with baseline %ld (now is %ld)\n", m->$baseline, now);
        if (ENQ_msg(m, m->$to)) {
            ENQ_ready(m->$to);
            new_work();
        }
        if (db) {
            uuid_t *txnid = remote_new_txn(db);
            timer_consume_hd++;

            long key = TIMER_QUEUE;
            snode_t *m_start, *m_end;
            int entries_read = 0;
            int64_t read_head = -1;
            int ret0 = remote_read_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, NULL, db);
            rtsd_printf(LOGPFX "   # dummy read msg from TIMER_QUEUE returns %d, entries read: %d\n", ret0, entries_read);

            int ret = remote_consume_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, read_head, txnid, db);
            rtsd_printf(LOGPFX "   # consume msg %ld from TIMER_QUEUE returns %d\n", m->$globkey, ret);
            int ret2 = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)m->$to->$globkey, txnid, db);
            rtsd_printf(LOGPFX "   # (timed) enqueue msg %ld to queue %ld returns %d\n", m->$globkey, m->$to->$globkey, ret2);
            remote_commit_txn(txnid, db);
            rtsd_printf(LOGPFX "############## Commit\n\n");
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
    rtsd_printf(LOGPFX "   # read msg from queue %ld returns %d, entries read: %d\n", key, ret, entries_read);

    if (!entries_read)
        return 0;
    db_row_t *r = (db_row_t*)m_start->value;
    rtsd_printf(LOGPFX "# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
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
        rtsd_printf(LOGPFX "--- %d: class_id %d, blob_size: %d, blob: ", n, row->class_id, row->blob_size);
        for (int i = 0; i < row->blob_size; i++)
            rtsd_printf(LOGPFX "%ld ", (long)row->blob[i]);
        rtsd_printf(LOGPFX ".\n");
        n++;
        row = row->next;
    }
}

void print_msg($Msg m) {
    rtsd_printf(LOGPFX "==== Message %p\n", m);
    rtsd_printf(LOGPFX "     next: %p\n", m->$next);
    rtsd_printf(LOGPFX "     to: %p\n", m->$to);
    rtsd_printf(LOGPFX "     cont: %p\n", m->$cont);
    rtsd_printf(LOGPFX "     waiting: %p\n", m->$waiting);
    rtsd_printf(LOGPFX "     baseline: %ld\n", m->$baseline);
    rtsd_printf(LOGPFX "     value: %p\n", m->$value);
    rtsd_printf(LOGPFX "     globkey: %ld\n", m->$globkey);
}

void print_actor($Actor a) {
    rtsd_printf(LOGPFX "==== Actor %p\n", a);
    rtsd_printf(LOGPFX "     next: %p\n", a->$next);
    rtsd_printf(LOGPFX "     msg: %p\n", a->$msg);
    rtsd_printf(LOGPFX "     outgoing: %p\n", a->$outgoing);
    rtsd_printf(LOGPFX "     waitsfor: %p\n", a->$waitsfor);
    rtsd_printf(LOGPFX "     consume_hd: %ld\n", (long)a->$consume_hd);
    rtsd_printf(LOGPFX "     catcher: %p\n", a->$catcher);
    rtsd_printf(LOGPFX "     globkey: %ld\n", a->$globkey);
}

void deserialize_system(snode_t *actors_start) {
    snode_t *msgs_start, *msgs_end;
    remote_read_full_table_in_txn(&msgs_start, &msgs_end, MSGS_TABLE, NULL, db);
    
    globdict = $NEW($dict,($Hashable)$Hashable$int$witness,NULL,NULL);

    long min_key = 0;

    rtsd_printf(LOGPFX "\n#### Msg allocation:\n");
    for(snode_t * node = msgs_start; node!=NULL; node=NEXT(node)) {
		db_row_t* r = (db_row_t*) node->value;
        rtsd_printf(LOGPFX "# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
        long key = (long)r->key;
		if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            rtsd_printf(LOGPFX "# r2 %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r2, (long)r2->key, r2->cells, r2->column_array, r2->no_columns, r2->last_blob_size);
            BlobHd *head = (BlobHd*)r2->column_array[0];
            $Msg msg = ($Msg)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            msg->$globkey = key;
            $dict_setitem(globdict, ($Hashable)$Hashable$int$witness, to$int(key), msg);
            rtsd_printf(LOGPFX "# Allocated Msg %p = %ld of class %s = %d\n", msg, msg->$globkey, msg->$class->$GCINFO, msg->$class->$class_id);
            if (key < min_key)
                min_key = key;
        }
    }
    rtsd_printf(LOGPFX "\n#### Actor allocation:\n");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        rtsd_printf(LOGPFX "# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
        long key = (long)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            rtsd_printf(LOGPFX "# r2 %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r2, (long)r2->key, r2->cells, r2->column_array, r2->no_columns, r2->last_blob_size);
            BlobHd *head = (BlobHd*)r2->column_array[0];
            $Actor act = ($Actor)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            act->$globkey = key;
            $dict_setitem(globdict, ($Hashable)$Hashable$int$witness, to$int(key), act);
            rtsd_printf(LOGPFX "# Allocated Actor %p = %ld of class %s = %d\n", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            if (key < min_key)
                min_key = key;
        }
    }
    next_key = min_key;

    rtsd_printf(LOGPFX "\n#### Msg contents:\n");
    for(snode_t * node = msgs_start; node!=NULL; node=NEXT(node)) {
		db_row_t* r = (db_row_t*) node->value;
        long key = (long)r->key;
		if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            $WORD *blob = ($WORD*)r2->column_array[0];
            int blob_size = r2->last_blob_size;
            $ROW row = extract_row(blob, blob_size);
            $Msg msg = ($Msg)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
            rtsd_printf(LOGPFX "####### Deserializing msg %p = %ld of class %s = %d\n", msg, msg->$globkey, msg->$class->$GCINFO, msg->$class->$class_id);
            print_rows(row);
            $glob_deserialize(($Serializable)msg, row, try_globdict);
            print_msg(msg);
        }
    }

    rtsd_printf(LOGPFX "\n#### Actor contents:\n");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
		db_row_t* r = (db_row_t*) node->value;
        long key = (long)r->key;
		if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            $WORD *blob = ($WORD*)r2->column_array[0];
            int blob_size = r2->last_blob_size;
            $ROW row = extract_row(blob, blob_size);
            $Actor act = ($Actor)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
            rtsd_printf(LOGPFX "####### Deserializing actor %p = %ld of class %s = %d\n", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            print_rows(row);
            $glob_deserialize(($Serializable)act, row, try_globdict);

            $Msg m = act->$waitsfor;
            if (m && m->$cont) {
                ADD_waiting(act, m);
                rtsd_printf(LOGPFX "# Adding Actor %ld to wait for Msg %ld\n", act->$globkey, m->$globkey);
            }
            else {
                act->$waitsfor = NULL;
            }

            rtsd_printf(LOGPFX "\n#### Reading msgs queue %ld contents:\n", key);
            queue_callback * qc = get_queue_callback(dummy_callback);
            int64_t prev_read_head = -1, prev_consume_head = -1;
            int ret = remote_subscribe_queue(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, qc, &prev_read_head, &prev_consume_head, db);
            rtsd_printf(LOGPFX "   # Subscribe queue %ld returns %d\n", key, ret);
            while (1) {
                long msg_key = read_queued_msg(key, &prev_read_head);
                if (!msg_key)
                    break;
                m = $dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(msg_key), NULL);
                rtsd_printf(LOGPFX "# Adding Msg %ld to Actor %ld\n", m->$globkey, act->$globkey);
                ENQ_msg(m, act);
            }
            if (act->$msg && !act->$waitsfor) {
                ENQ_ready(act);
                rtsd_printf(LOGPFX "# Adding Actor %ld to the readyQ\n", act->$globkey);
            }
            print_actor(act);
        }
    }

    rtsd_printf(LOGPFX "\n#### Reading timer queue contents:\n");
    time_t now = current_time();
    queue_callback * qc = get_queue_callback(dummy_callback);
	int64_t prev_read_head = -1, prev_consume_head = -1;
	int ret = remote_subscribe_queue(TIMER_QUEUE, 0, 0, MSG_QUEUE, TIMER_QUEUE, qc, &prev_read_head, &prev_consume_head, db);
    rtsd_printf(LOGPFX "   # Subscribe queue 0 returns %d\n", ret);
    while (1) {
        long msg_key = read_queued_msg(TIMER_QUEUE, &prev_read_head);
        if (!msg_key)
            break;
        $Msg m = $dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(msg_key), NULL);
        if (m->$baseline < now)
            m->$baseline = now;
        rtsd_printf(LOGPFX "# Adding Msg %ld to the timerQ\n", m->$globkey);
        ENQ_timed(m);
    }

    env_actor  = ($Env)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(-11), NULL);
    root_actor = ($Actor)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(-14), NULL);
    globdict = NULL;
    rtsd_printf(LOGPFX "\n\n");
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

    rtsd_printf(LOGPFX "## Built blob of size %ld\n", total);
    for (int i = 0; i < total; i++) {
        rtsd_printf(LOGPFX "%lu ", (unsigned long)blob[i]);
    }

    //printf("\n## Sanity check extract row:\n");
    //$ROW row1 = extract_row(blob, total*sizeof($WORD));
    //print_rows(row1);

    int ret = remote_insert_in_txn(column, 2, 1, 1, blob, total*sizeof($WORD), table, txnid, db);
    rtsd_printf(LOGPFX "   # insert to table %ld, row %ld, returns %d\n", (long)table, key, ret);
}

void serialize_msg($Msg m, uuid_t *txnid) {
    rtsd_printf(LOGPFX "#### Serializing Msg %ld\n", m->$globkey);
    $ROW row = $glob_serialize(($Serializable)m, try_globkey);
    print_rows(row);
    insert_row(m->$globkey, $total_rowsize(row), row, MSGS_TABLE, txnid);
}

void serialize_actor($Actor a, uuid_t *txnid) {
    rtsd_printf(LOGPFX "#### Serializing Actor %ld\n", a->$globkey);
    $ROW row = $glob_serialize(($Serializable)a, try_globkey);
    print_rows(row);
    insert_row(a->$globkey, $total_rowsize(row), row, ACTORS_TABLE, txnid);

    $Msg out = a->$outgoing;
    while (out) {
        serialize_msg(out, txnid);
        out = out->$next;
    }
}

////////////////////////////////////////////////////////////////////////////////////////

void *main_loop(void *idx) {
    struct timespec ts1, ts2, ts3;
    while (1) {
        $Actor current = DEQ_ready();
        if (current) {
            new_work();
            pthread_setspecific(self_key, current);
            $Msg m = current->$msg;
            $Cont cont = m->$cont;
            $WORD val = m->$value;
            
            clock_gettime(CLOCK_MONOTONIC, &ts1);
            wt_stats[(int)idx].state = WT_Working;

            rtsd_printf(LOGPFX "## Running actor %ld : %s\n", current->$globkey, current->$class->$GCINFO);
            $R r = cont->$class->__call__(cont, val);

            clock_gettime(CLOCK_MONOTONIC, &ts2);
            long long int diff = (ts2.tv_sec * 1000000000 + ts2.tv_nsec) - (ts1.tv_sec * 1000000000 + ts1.tv_nsec);

            wt_stats[(int)idx].conts_count++;
            wt_stats[(int)idx].conts_sum += diff;

            if      (diff < 100)              { wt_stats[(int)idx].conts_100ns++; }
            else if (diff < 1   * 1000)       { wt_stats[(int)idx].conts_1us++; }
            else if (diff < 10  * 1000)       { wt_stats[(int)idx].conts_10us++; }
            else if (diff < 100 * 1000)       { wt_stats[(int)idx].conts_100us++; }
            else if (diff < 1   * 1000000)    { wt_stats[(int)idx].conts_1ms++; }
            else if (diff < 10  * 1000000)    { wt_stats[(int)idx].conts_10ms++; }
            else if (diff < 100 * 1000000)    { wt_stats[(int)idx].conts_100ms++; }
            else if (diff < 1   * 1000000000) { wt_stats[(int)idx].conts_1s++; }
            else if (diff < (long long int)10  * 1000000000) { wt_stats[(int)idx].conts_10s++; }
            else if (diff < (long long int)100 * 1000000000) { wt_stats[(int)idx].conts_100s++; }
            else                              { wt_stats[(int)idx].conts_inf++; }

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
                        rtsd_printf(LOGPFX "   # dummy read msg from queue %ld returns %d, entries read: %d\n", key, ret0, entries_read);

                        int ret = remote_consume_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, read_head, txnid, db);
                        rtsd_printf(LOGPFX "   # consume msg %ld from queue %ld returns %d\n", m->$globkey, key, ret);
                        remote_commit_txn(txnid, db);
                        rtsd_printf(LOGPFX "############## Commit\n\n");
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
                        rtsd_printf(LOGPFX "## Waking up actor %ld : %s\n", b->$globkey, b->$class->$GCINFO);
                        b = c;
                    }
                    rtsd_printf(LOGPFX "## DONE actor %ld : %s\n", current->$globkey, current->$class->$GCINFO);
                    if (DEQ_msg(current)) {
                        ENQ_ready(current);
                    }
                    break;
                }
                case $RCONT: {
                    m->$cont = r.cont;
                    m->$value = r.value;
                    rtsd_printf(LOGPFX "## CONT actor %ld : %s\n", current->$globkey, current->$class->$GCINFO);
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
                        rtsd_printf(LOGPFX "############## Commit\n\n");
                    } else {
                        FLUSH_outgoing(current, NULL);
                    }
                    m->$cont = r.cont;
                    $Msg x = ($Msg)r.value;
                    if (ADD_waiting(current, x)) {      // x->cont != NULL: x is still being processed so current was added to x->waiting
                        rtsd_printf(LOGPFX "## AWAIT actor %ld : %s\n", current->$globkey, current->$class->$GCINFO);
                        current->$waitsfor = x;
                    } else {                            // x->cont == NULL: x->value holds the final response, current is not in x->waiting
                        rtsd_printf(LOGPFX "## AWAIT/wakeup actor %ld : %s\n", current->$globkey, current->$class->$GCINFO);
                        m->$value = x->$value;
                        ENQ_ready(current);
                    }
                    break;
                }
            }
            clock_gettime(CLOCK_MONOTONIC, &ts3);
            diff = (ts3.tv_sec * 1000000000 + ts3.tv_nsec) - (ts2.tv_sec * 1000000000 + ts2.tv_nsec);
            wt_stats[(int)idx].bkeep_count++;
            wt_stats[(int)idx].bkeep_sum += diff;

            if      (diff < 100)              { wt_stats[(int)idx].bkeep_100ns++; }
            else if (diff < 1   * 1000)       { wt_stats[(int)idx].bkeep_1us++; }
            else if (diff < 10  * 1000)       { wt_stats[(int)idx].bkeep_10us++; }
            else if (diff < 100 * 1000)       { wt_stats[(int)idx].bkeep_100us++; }
            else if (diff < 1   * 1000000)    { wt_stats[(int)idx].bkeep_1ms++; }
            else if (diff < 10  * 1000000)    { wt_stats[(int)idx].bkeep_10ms++; }
            else if (diff < 100 * 1000000)    { wt_stats[(int)idx].bkeep_100ms++; }
            else if (diff < 1   * 1000000000) { wt_stats[(int)idx].bkeep_1s++; }
            else if (diff < (long long int)10  * 1000000000) { wt_stats[(int)idx].bkeep_10s++; }
            else if (diff < (long long int)100 * 1000000000) { wt_stats[(int)idx].bkeep_100s++; }
            else                              { wt_stats[(int)idx].bkeep_inf++; }

            wt_stats[(int)idx].state = WT_Idle;
        } else {
            pthread_mutex_lock(&sleep_lock);
            wt_stats[(int)idx].state = WT_Sleeping;
            wt_stats[(int)idx].sleeps++;
            pthread_cond_wait(&work_to_do, &sleep_lock);
            pthread_mutex_unlock(&sleep_lock);
        }
    }
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
  $register(&$NewRoot$methods);
  $register(&$WriteRoot$methods);
  $register(&$Env$methods);
  $register(&$Connection$methods);
}
 
////////////////////////////////////////////////////////////////////////////////////////

void *$mon_loop(void *appname) {
    rtsv_printf("Starting monitor listen on %s\n", rts_mon_path);

    pid_t pid = getpid();

    int s, s2, t, len;
    struct sockaddr_un local, remote;
    char q[100];

    if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("socket");
        exit(1);
    }

    local.sun_family = AF_UNIX;
    strcpy(local.sun_path, rts_mon_path);
    unlink(local.sun_path);
    len = sizeof(local.sun_path) + sizeof(local.sun_family);
    if (bind(s, (struct sockaddr *)&local, len) == -1) {
        perror("bind");
        exit(1);
    }

    if (listen(s, 5) == -1) {
        perror("listen");
        exit(1);
    }

    for(;;) {
        t = sizeof(remote);
        if ((s2 = accept(s, (struct sockaddr *)&remote, &t)) == -1) {
            perror("accept");
            exit(1);
        }

        int n;
        while (1) {
            n = recv(s2, q, 100, 0);
            if (n <= 0) {
                if (n < 0) perror("recv");
                break;
            }

            if (strncmp(q, "WTS", 3) == 0) {
                yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
                yyjson_mut_val *root = yyjson_mut_obj(doc);
                yyjson_mut_doc_set_root(doc, root);

                yyjson_mut_obj_add_str(doc, root, "name", appname);
                yyjson_mut_obj_add_int(doc, root, "pid", pid);

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

                const char *json = yyjson_mut_write(doc, 0, NULL);
                int send_res = send(s2, json, strlen(json), 0);
                free((void *)json);
                yyjson_mut_doc_free(doc);
                if (send_res < 0) {
                    perror("send");
                    break;
                }
            }
        }

        close(s2);
    }
}


/*
 * A note on argument parsing: The RTS has its own command line arguments, all
 * prefixed with --rts-, which we need to parse out. The remainder of the
 * arguments should be passed on to the Acton program, thus we need to fiddle
 * with argv. To avoid modifying argv in place, we create a new argc and argv
 * which we bootstrap the Acton program with. The special -- means to stop
 * scanning for options, and any argument following it will be passed verbatim.
 * For example (note the duplicate --rts-verbose)
 *   Command line    : ./app foo --rts-verbose --bar --rts-verbose
 *   Application sees: [./app, foo, --bar]
 * Using -- to pass verbatim arguments:
 *   Command line    : ./app foo --rts-verbose --bar -- --rts-verbose
 *   Application sees: [./app, foo, --bar, --, --rts-verbose]
 */
int main(int argc, char **argv) {
    int ch = 0;
    uint ddb_no_host = 0;
    char **ddb_host = NULL;
    int ddb_port = 32000;
    int ddb_replication = 3;
    int new_argc = argc;
    int cpu_pin = 0;
    long num_cores = sysconf(_SC_NPROCESSORS_ONLN);
    num_wthreads = num_cores;

    static struct option long_options[] = {
        {"rts-debug", no_argument, NULL, 'd'},
        {"rts-ddb-host", required_argument, NULL, 'h'},
        {"rts-ddb-port", required_argument, NULL, 'p'},
        {"rts-ddb-replication", required_argument, NULL, 'r'},
        {"rts-mon", required_argument, NULL, 'm'},
        {"rts-verbose", no_argument, NULL, 'v'},
        {"rts-wthreads", required_argument, NULL, 'w'},
        {NULL, 0, NULL, 0}
    };

    while ((ch = getopt_long(argc, argv, "-", long_options, NULL)) != -1) {
        switch (ch) {
            case 'd':
                new_argc--;
                #ifndef DEV
                fprintf(stderr, "ERROR: RTS debug not supported.\n");
                fprintf(stderr, "HINT: Recompile this program using: actonc --rts-debug ...\n");
                exit(1);
                #endif
                rts_debug = 1;
                // Enabling rts debug implies verbose RTS output too
                rts_verbose = 10;
                break;
            case 'h':
                new_argc -= 2;
                ddb_host = realloc(ddb_host, ++ddb_no_host * sizeof *ddb_host);
                ddb_host[ddb_no_host-1] = optarg;
                break;
            case 'm':
                new_argc -= 2;
                rts_mon_path = optarg;
                break;
            case 'p':
                new_argc -= 2;
                ddb_port = atoi(optarg);
                break;
            case 'r':
                new_argc -= 2;
                ddb_replication = atoi(optarg);
                break;
            case 'v':
                new_argc--;
                rts_verbose = 1;
                break;
            case 'w':
                new_argc -= 2;
                num_wthreads = atoi(optarg);
                break;
        }
    }
    char** new_argv = malloc((new_argc+1) * sizeof *new_argv);

    // length of long_options array
    int lo_len = sizeof(long_options) / sizeof(long_options[0]) - 1;
    // where we map current (i) argc position into new_argc
    int new_argc_dst = 0;
    // stop scanning once we've seen '--', passing the rest verbatim
    int opt_scan = 1;
    for (int i = 0; i < argc; ++i) {
        if (strcmp(argv[i], "--") == 0) opt_scan = 0;
        if (opt_scan) {
            for (int j = 0; j < lo_len; j++) {
                // compare at +2 since in argv it is --foo while only foo in long_options
                if (strcmp(argv[i]+2, long_options[j].name) == 0) {
                    if (long_options[j].has_arg == 1) i++;
                    goto cnt; // continue on outer loop
                }
            }
        }
        new_argv[new_argc_dst++] = argv[i];
        cnt:;
    }
    new_argv[new_argc] = NULL;

    if (num_wthreads > MAX_WTHREADS) {
        fprintf(stderr, "ERROR: Maximum of %d worker threads supported.\n", MAX_WTHREADS);
        fprintf(stderr, "HINT: Run this program with fewer worker threads: %s --rts-wthreads %d\n", argv[0], MAX_WTHREADS);
        exit(1);
    }
    // Determine number of worker threads, normally 1:1 per CPU thread / core
    // For low core count systems we do a minimum of 4 worker threads
    if (num_wthreads < 4) {
        rtsv_printf(LOGPFX "Detected %ld CPUs: Using %ld worker threads, due to low CPU count. No CPU affinity used.\n", num_cores, num_wthreads);
        num_wthreads = 4;
        cpu_pin = 0;
    } else {
        rtsv_printf(LOGPFX "Detected %ld CPUs: Using %ld worker threads for 1:1 mapping with CPU affinity set.\n", num_cores, num_wthreads);
        cpu_pin = 1;
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

    $register_builtin();
    env$$__init__();
    $register_rts();
    $ROOTINIT();

    unsigned int seed;
    if (ddb_host) {
        GET_RANDSEED(&seed, 0);
        rtsv_printf(LOGPFX "Using distributed database backend replication factor of %d\n", ddb_replication);
        db = get_remote_db(ddb_replication);
        for (int i=0; i<ddb_no_host; i++) {
            char * colon = strchr(ddb_host[i], ':');
            int port = ddb_port;
            if (colon) {
                *colon = '\0';
                port = atoi(colon + 1);
            }

            rtsv_printf(LOGPFX "Using distributed database backend (DDB): %s:%d\n", ddb_host[i], port);
            add_server_to_membership(ddb_host[i], port, db, &seed);
        }
    }

    if (db) {
        snode_t* start_row = NULL, * end_row = NULL;
        rtsv_printf(LOGPFX "Checking for existing actor state in DDB... ");
        fflush(stdout);
        int no_items = remote_read_full_table_in_txn(&start_row, &end_row, ACTORS_TABLE, NULL, db);
        rtsv_printf(LOGPFX "done\n");
        if (no_items > 0) {
            rtsv_printf(LOGPFX "Found %d existing actors; Restoring actor state from DDB... ", no_items);
            fflush(stdout);
            deserialize_system(start_row);
            rtsv_printf(LOGPFX "done\n");
        } else {
            rtsv_printf(LOGPFX "No previous state in DDB; Initializing database...");
            fflush(stdout);
            int indices[] = {0};
            db_schema_t* db_schema = db_create_schema(NULL, 1, indices, 1, indices, 0, indices, 0);
            create_db_queue(TIMER_QUEUE);
            timer_consume_hd = 0;
            BOOTSTRAP(new_argc, new_argv);
            rtsv_printf(LOGPFX "done\n");
        }
    } else {
        BOOTSTRAP(new_argc, new_argv);
    }

    pthread_key_create(&self_key, NULL);
    cpu_set_t cpu_set;

    // Start worker threads
    // number of threads is workers + (IO + mon)
    pthread_t threads[num_wthreads + 2];
    // Keep track of total number of threads, worker threads and then some...
    int num_threads = num_wthreads;

    // eventloop + pin to CPU 0
    pthread_create(&threads[num_threads], NULL, $eventloop, (void*)num_threads);
    if (cpu_pin == 1) {
        CPU_ZERO(&cpu_set);
        CPU_SET(0, &cpu_set);
        pthread_setaffinity_np(threads[num_threads], sizeof(cpu_set), &cpu_set);
    }
    num_threads++;

    // Monitor listen + pin to CPU 0
    if (rts_mon_path) {
        pthread_create(&threads[num_threads], NULL, $mon_loop, (void*)argv[0]);
        if (cpu_pin) {
            CPU_ZERO(&cpu_set);
            CPU_SET(0, &cpu_set);
            pthread_setaffinity_np(threads[num_threads], sizeof(cpu_set), &cpu_set);
        }
        num_threads++;
    }

    int total_threads = 0;
    for(int idx = 0; idx <= num_cores; ++idx) {
        pthread_create(&threads[idx], NULL, main_loop, (void*)idx);
        // Note how we pin thread index + 1, so worker thread 0 is on CPU 1
        // We use CPU 0 for misc threads, like IO / mon etc
        if (cpu_pin) {
            CPU_ZERO(&cpu_set);
            CPU_SET(idx+1, &cpu_set);
            pthread_setaffinity_np(threads[idx], sizeof(cpu_set), &cpu_set);
        }
    }
    
    for(int idx = 0; idx < num_threads; ++idx) {
        pthread_join(threads[idx], NULL);
    }
    return 0;
}
