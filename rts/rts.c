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
#include <getopt.h>

#include "rts.h"
#include "../builtin/minienv.h"

// We want to pass through unknown arguments, so tell getopt to ignore invalid
// (unknown to it) options
extern int opterr;
int opterr = 0;

#include "../backend/client_api.h"
#include "../backend/fastrand.h"

#ifdef __gnu_linux__
    #define IS_GNU_LINUX
    #define USE_EPOLL
#elif  __APPLE__ && __MACH__
    #define IS_MACOS
    #define USE_KQUEUE
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
        printf("error getting the core count %d\n", ret);
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

pthread_key_t self_key;
pthread_mutex_t sleep_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t work_to_do = PTHREAD_COND_INITIALIZER;

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
    a->$offspring = NULL;
    a->$uterus = NULL;
    a->$waitsfor = NULL;
    a->$consume_hd = 0;
    a->$catcher = NULL;
    atomic_flag_clear(&a->$msg_lock);
    a->$globkey = get_next_key();
    //printf("# New Actor %ld at %p of class %s\n", a->$globkey, a, a->$class->$GCINFO);
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
    res->$offspring = NULL;
    res->$uterus = NULL;
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
void ENQ_timed($Msg m) {
    time_t m_baseline = m->$baseline;
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
    }
    spinlock_unlock(&timerQ_lock);
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
    //printf("#### Create queue %ld returns %d\n", key, ret);
    queue_callback * qc = get_queue_callback(dummy_callback);
	int64_t prev_read_head = -1, prev_consume_head = -1;
	ret = remote_subscribe_queue(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, qc, &prev_read_head, &prev_consume_head, db);
    //printf("   # Subscribe queue %ld returns %d\n", key, ret);
}

void BOOTSTRAP(int argc, char *argv[]) {
    $list args = $list$new(NULL,NULL);
    for (int i=0; i< argc; i++)
      $list_append(args,to$str(argv[i]));

    env_actor = $NEW($Env, args);
    $Actor ancestor0 = $NEW($Actor);
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    $Msg m = $NEW($Msg, ancestor0, &$NewRoot$cont, now.tv_sec, &$WriteRoot$cont);

    if (db) {
        create_db_queue(env_actor->$globkey);
        create_db_queue(ancestor0->$globkey);
        int ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)ancestor0->$globkey, NULL, db);
        //printf("   # enqueue bootstrap msg %ld to ancestor0 queue %ld returns %d\n", m->$globkey, ancestor0->$globkey, ret);
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
    if (self) {
        m->$baseline = self->$msg->$baseline;
        PUSH_outgoing(self, m);
    } else {
        struct timespec now;
        clock_gettime(CLOCK_MONOTONIC, &now);
        m->$baseline = now.tv_sec;
        if (ENQ_msg(m, to)) {
           ENQ_ready(to);
        }
    }
    pthread_mutex_lock(&sleep_lock);
    pthread_cond_signal(&work_to_do);
    pthread_mutex_unlock(&sleep_lock);
    return m;
}

$Msg $AFTER($int sec, $Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    $Actor to = self->$uterus ? self->$uterus : self;
    //printf("# AFTER towards %ld (current: %ld)\n", to->$globkey, self->$globkey);
    time_t baseline = self->$msg->$baseline + sec->val;
    $Msg m = $NEW($Msg, to, cont, baseline, &$Done$instance);
    PUSH_outgoing(self, m);
//    ENQ_timed(m);
    return m;
}

$R $AWAIT($Msg m, $Cont cont) {
    return $R_WAIT(cont, m);
}

void $NEWACT($Actor a) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    a->$next = self->$uterus;
    self->$uterus = a;
}

void $OLDACT() {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    $Actor a = self->$uterus;
    self->$uterus = a->$next;
    a->$next = self->$offspring;
    self->$offspring = a;
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
    //printf("#### FLUSH_outgoing messages from %ld\n", self->$globkey);
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
            ENQ_timed(m);
            dest = 0;
        }
        if (db) {
            int ret = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)dest, txnid, db);
            //if (dest)
            //    printf("   # enqueue msg %ld to queue %ld returns %d\n", m->$globkey, dest, ret);
            //else
            //    printf("   # enqueue msg %ld to TIMER_QUEUE returns %d\n", m->$globkey, ret);
        }
        m = next;
    }
}

////////////////////////////////////////////////////////////////////////////////////////

$dict globdict = NULL;

$WORD try_globdict($WORD w) {
    int key = (int)w;
    $WORD obj = $dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
    return obj;
}

long read_queued_msg(long key, int64_t *read_head) {
    snode_t *m_start, *m_end;
    int entries_read = 0;
    
    int ret = remote_read_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, 
                                       1, &entries_read, read_head, &m_start, &m_end, NULL, db);
    //printf("   # read msg from queue %ld returns %d, entries read: %d\n", key, ret, entries_read);

    if (!entries_read)
        return 0;
    db_row_t *r = (db_row_t*)m_start->value;
    //printf("# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
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
        printf("--- %d: class_id %d, blob_size: %d, blob: ", n, row->class_id, row->blob_size);
        for (int i = 0; i < row->blob_size; i++)
            printf("%ld ", (long)row->blob[i]);
        printf(".\n");
        n++;
        row = row->next;
    }
}

void print_msg($Msg m) {
    printf("==== Message %p\n", m);
    printf("     next: %p\n", m->$next);
    printf("     to: %p\n", m->$to);
    printf("     cont: %p\n", m->$cont);
    printf("     waiting: %p\n", m->$waiting);
    printf("     baseline: %ld\n", m->$baseline);
    printf("     value: %p\n", m->$value);
    printf("     globkey: %ld\n", m->$globkey);
}

void print_actor($Actor a) {
    printf("==== Actor %p\n", a);
    printf("     next: %p\n", a->$next);
    printf("     msg: %p\n", a->$msg);
    printf("     outgoing: %p\n", a->$outgoing);
    printf("     offspring: %p\n", a->$offspring);
    printf("     uterus: %p\n", a->$uterus);
    printf("     waitsfor: %p\n", a->$waitsfor);
    printf("     consume_hd: %ld\n", (long)a->$consume_hd);
    printf("     catcher: %p\n", a->$catcher);
    printf("     globkey: %ld\n", a->$globkey);
}

void deserialize_system(snode_t *actors_start) {
    snode_t *msgs_start, *msgs_end;
    remote_read_full_table_in_txn(&msgs_start, &msgs_end, MSGS_TABLE, NULL, db);
    
    globdict = $NEW($dict,($Hashable)$Hashable$int$witness,NULL,NULL);

    long min_key = 0;

    //printf("\n#### Msg allocation:\n");
    for(snode_t * node = msgs_start; node!=NULL; node=NEXT(node)) {
		db_row_t* r = (db_row_t*) node->value;
        //printf("# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
        long key = (long)r->key;
		if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            //printf("# r2 %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r2, (long)r2->key, r2->cells, r2->column_array, r2->no_columns, r2->last_blob_size);
            BlobHd *head = (BlobHd*)r2->column_array[0];
            $Msg msg = ($Msg)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            msg->$globkey = key;
            $dict_setitem(globdict, ($Hashable)$Hashable$int$witness, to$int(key), msg);
            //printf("# Allocated Msg %p = %ld of class %s = %d\n", msg, msg->$globkey, msg->$class->$GCINFO, msg->$class->$class_id);
            if (key < min_key)
                min_key = key;
        }
    }
    //printf("\n#### Actor allocation:\n");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
        db_row_t* r = (db_row_t*) node->value;
        //printf("# r %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r, (long)r->key, r->cells, r->column_array, r->no_columns, r->last_blob_size);
        long key = (long)r->key;
        if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            //printf("# r2 %p, key: %ld, cells: %p, columns: %p, no_cols: %d, blobsize: %d\n", r2, (long)r2->key, r2->cells, r2->column_array, r2->no_columns, r2->last_blob_size);
            BlobHd *head = (BlobHd*)r2->column_array[0];
            $Actor act = ($Actor)$GET_METHODS(head->class_id)->__deserialize__(NULL, NULL);
            act->$globkey = key;
            $dict_setitem(globdict, ($Hashable)$Hashable$int$witness, to$int(key), act);
            //printf("# Allocated Actor %p = %ld of class %s = %d\n", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            if (key < min_key)
                min_key = key;
        }
    }
    next_key = min_key;

    //printf("\n#### Msg contents:\n");
    for(snode_t * node = msgs_start; node!=NULL; node=NEXT(node)) {
		db_row_t* r = (db_row_t*) node->value;
        long key = (long)r->key;
		if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            $WORD *blob = ($WORD*)r2->column_array[0];
            int blob_size = r2->last_blob_size;
            $ROW row = extract_row(blob, blob_size);
            $Msg msg = ($Msg)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
            //printf("####### Deserializing msg %p = %ld of class %s = %d\n", msg, msg->$globkey, msg->$class->$GCINFO, msg->$class->$class_id);
            //print_rows(row);
            $glob_deserialize(($Serializable)msg, row, try_globdict);
            //print_msg(msg);
        }
    }

    //printf("\n#### Actor contents:\n");
    for(snode_t * node = actors_start; node!=NULL; node=NEXT(node)) {
		db_row_t* r = (db_row_t*) node->value;
        long key = (long)r->key;
		if (r->cells) {
            db_row_t* r2 = (HEAD(r->cells))->value;
            $WORD *blob = ($WORD*)r2->column_array[0];
            int blob_size = r2->last_blob_size;
            $ROW row = extract_row(blob, blob_size);
            $Actor act = ($Actor)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(key), NULL);
            //printf("####### Deserializing actor %p = %ld of class %s = %d\n", act, act->$globkey, act->$class->$GCINFO, act->$class->$class_id);
            //print_rows(row);
            $glob_deserialize(($Serializable)act, row, try_globdict);

            $Msg m = act->$waitsfor;
            if (m && m->$cont) {
                ADD_waiting(act, m);
                //printf("# Adding Actor %ld to wait for Msg %ld\n", act->$globkey, m->$globkey);
            }
            else {
                act->$waitsfor = NULL;
            }

            //printf("\n#### Reading nsgs queue %ld contents:\n", key);
            queue_callback * qc = get_queue_callback(dummy_callback);
            int64_t prev_read_head = -1, prev_consume_head = -1;
            int ret = remote_subscribe_queue(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, qc, &prev_read_head, &prev_consume_head, db);
            //printf("   # Subscribe queue %ld returns %d\n", key, ret);
            while (1) {
                long msg_key = read_queued_msg(key, &prev_read_head);
                if (!msg_key)
                    break;
                m = $dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(msg_key), NULL);
                //printf("# Adding Msg %ld to Actor %ld\n", m->$globkey, act->$globkey);
                ENQ_msg(m, act);
            }
            if (act->$msg && !act->$waitsfor) {
                ENQ_ready(act);
                //printf("# Adding Actor %ld to the readyQ\n", act->$globkey);
            }
            //print_actor(act);
        }
    }

    //printf("\n#### Reading timer queue contents:\n");
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    queue_callback * qc = get_queue_callback(dummy_callback);
	int64_t prev_read_head = -1, prev_consume_head = -1;
	int ret = remote_subscribe_queue(TIMER_QUEUE, 0, 0, MSG_QUEUE, TIMER_QUEUE, qc, &prev_read_head, &prev_consume_head, db);
    //printf("   # Subscribe queue 0 returns %d\n", ret);
    while (1) {
        long msg_key = read_queued_msg(TIMER_QUEUE, &prev_read_head);
        if (!msg_key)
            break;
        $Msg m = $dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(msg_key), NULL);
        if (m->$baseline < now.tv_sec)
            m->$baseline = now.tv_sec;
        //printf("# Adding Msg %ld to the timerQ\n", m->$globkey);
        ENQ_timed(m);
    }

    env_actor  = ($Env)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(-11), NULL);
    root_actor = ($Actor)$dict_get(globdict, ($Hashable)$Hashable$int$witness, to$int(-14), NULL);
    globdict = NULL;
    //printf("\n\n");
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

    //printf("## Built blob of size %ld\n", total);
    //for (int i = 0; i < total; i++) {
    //    printf("%lu ", (unsigned long)blob[i]);
    //}

    //printf("\n## Sanity check extract row:\n");
    //$ROW row1 = extract_row(blob, total*sizeof($WORD));
    //print_rows(row1);

    int ret = remote_insert_in_txn(column, 2, 1, 1, blob, total*sizeof($WORD), table, txnid, db);
    //printf("   # insert to table %ld, row %ld, returns %d\n", (long)table, key, ret);
}

void serialize_msg($Msg m, uuid_t *txnid) {
    //printf("#### Serializing Msg %ld\n", m->$globkey);
    $ROW row = $glob_serialize(($Serializable)m, try_globkey);
    //print_rows(row);
    insert_row(m->$globkey, $total_rowsize(row), row, MSGS_TABLE, txnid);
}

void serialize_actor($Actor a, uuid_t *txnid) {
    //printf("#### Serializing Actor %ld\n", a->$globkey);
    $ROW row = $glob_serialize(($Serializable)a, try_globkey);
    //print_rows(row);
    insert_row(a->$globkey, $total_rowsize(row), row, ACTORS_TABLE, txnid);

    $Msg out = a->$outgoing;
    while (out) {
        serialize_msg(out, txnid);
        out = out->$next;
    }
}

void FLUSH_offspring($Actor current, uuid_t *txnid) {
    $Actor a = current->$offspring;
    while (a) {
        if (db) {
            create_db_queue(a->$globkey);
            a->$consume_hd = 0;
            serialize_actor(a, txnid);
            FLUSH_outgoing(a, txnid);
        } else {
            FLUSH_outgoing(a, NULL);
        }
        $Actor b = a;
        a = a->$next;
        b->$next = NULL;
    }
    current->$offspring = NULL;
}

////////////////////////////////////////////////////////////////////////////////////////

void *main_loop(void *arg) {
    while (1) {
        $Actor current = DEQ_ready();
        if (current) {
            pthread_setspecific(self_key, current);
            $Msg m = current->$msg;
            $Cont cont = m->$cont;
            $WORD val = m->$value;
            
            $R r = cont->$class->__call__(cont, val);
            switch (r.tag) {
                case $RDONE: {
                    if (db) {
                        uuid_t * txnid = remote_new_txn(db);
                        current->$consume_hd++;
                        serialize_actor(current, txnid);
                        FLUSH_outgoing(current, txnid);
                        serialize_msg(current->$msg, txnid);
                        FLUSH_offspring(current, txnid);

                        long key = current->$globkey;
                        snode_t *m_start, *m_end;
                        int entries_read = 0;
                        int64_t read_head = -1;
                        int ret0 = remote_read_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, NULL, db);
                        //printf("   # dummy read msg from queue %ld returns %d, entries read: %d\n", key, ret0, entries_read);

                        int ret = remote_consume_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, read_head, txnid, db);
                        //printf("   # consume msg %ld from queue %ld returns %d\n", m->$globkey, key, ret);
                        remote_commit_txn(txnid, db);
                        //printf("############## Commit\n\n");
                    } else {
                        FLUSH_outgoing(current, NULL);
                        FLUSH_offspring(current, NULL);
                    }

                    m->$value = r.value;                 // m->value holds the response,
                    $Actor b = FREEZE_waiting(m);        // so set m->cont = NULL and stop further m->waiting additions
                    while (b) {
                        b->$msg->$value = r.value;
                        b->$waitsfor = NULL;
                        ENQ_ready(b);
                        b = b->$next;
                    }
                    if (DEQ_msg(current)) {
                        ENQ_ready(current);
                    }
                    break;
                }
                case $RCONT: {
                    m->$cont = r.cont;
                    m->$value = r.value;
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
                        FLUSH_offspring(current, txnid);
                        remote_commit_txn(txnid, db);
                        //printf("############## Commit\n\n");
                    } else {
                        FLUSH_outgoing(current, NULL);
                        FLUSH_offspring(current, NULL);
                    }
                    m->$cont = r.cont;
                    $Msg x = ($Msg)r.value;
                    if (ADD_waiting(current, x)) {      // x->cont != NULL: x is still being processed so current was added to x->waiting
                        current->$waitsfor = x;
                    } else {                            // x->cont == NULL: x->value holds the final response, current is not in x->waiting
                        m->$value = x->$value;
                        ENQ_ready(current);
                    }
                    break;
                }
            }
        } else {
            struct timespec now;
            clock_gettime(CLOCK_MONOTONIC, &now);
            $Msg m = DEQ_timed(now.tv_sec);
            if (m) {
                if (ENQ_msg(m, m->$to)) {
                    ENQ_ready(m->$to);
                }
                if (db) {
                    uuid_t *txnid = remote_new_txn(db);
                    timer_consume_hd++;

                    long key = TIMER_QUEUE;
                    snode_t *m_start, *m_end;
                    int entries_read = 0;
                    int64_t read_head = -1;
                    int ret0 = remote_read_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, 1, &entries_read, &read_head, &m_start, &m_end, NULL, db);
                    //printf("   # dummy read msg from TIMER_QUEUE returns %d, entries read: %d\n", ret0, entries_read);

                    int ret = remote_consume_queue_in_txn(($WORD)key, 0, 0, MSG_QUEUE, ($WORD)key, read_head, txnid, db);
                    //printf("   # consume msg %ld from TIMER_QUEUE returns %d\n", m->$globkey, ret);
                    int ret2 = remote_enqueue_in_txn(($WORD*)&m->$globkey, 1, NULL, 0, MSG_QUEUE, (WORD)m->$to->$globkey, txnid, db);
                    //printf("   # (timed) enqueue msg %ld to queue %ld returns %d\n", m->$globkey, m->$to->$globkey, ret2);
                    remote_commit_txn(txnid, db);
                    //printf("############## Commit\n\n");
                }
            } else {
                if  ((int)arg==0) {
                    $eventloop();
                } else {
                  pthread_mutex_lock(&sleep_lock);
                  pthread_cond_wait(&work_to_do, &sleep_lock);
                  pthread_mutex_unlock(&sleep_lock);
                  // static struct timespec idle_wait = { 0, 50000000 };  // 500ms
                  // nanosleep(&idle_wait, NULL);
                }
            }
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
    char *ddb_host = NULL;
    int ddb_port = 32000;
    int verbose = 0;
    int new_argc = argc;

    static struct option long_options[] = {
        {"rts-verbose", no_argument, NULL, 'v'},
        {"rts-ddb-host", required_argument, NULL, 'h'},
        {"rts-ddb-port", required_argument, NULL, 'p'},
        {NULL, 0, NULL, 0}
    };

    while ((ch = getopt_long(argc, argv, "-", long_options, NULL)) != -1) {
        switch (ch) {
            case 'h':
                new_argc -= 2;
                ddb_host = optarg;
                break;
            case 'p':
                new_argc -= 2;
                ddb_port = atoi(optarg);
                break;
            case 'v':
                new_argc--;
                verbose = 1;
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

    long num_cores = sysconf(_SC_NPROCESSORS_ONLN);
    //    printf("%ld worker threads\n", num_cores);
    kq = kqueue();
    $register_builtin();
    minienv$$__init__();
    $register_rts();
    $ROOTINIT();

    unsigned int seed;
    if (ddb_host) {
        if (verbose) printf("Acton RTS: using distributed database backend (DDB): %s:%d\n", ddb_host, ddb_port);
        GET_RANDSEED(&seed, 0);
        db = get_remote_db(1);
        add_server_to_membership(ddb_host, ddb_port, db, &seed);
    }

    if (db) {
        snode_t* start_row = NULL, * end_row = NULL;
        if (verbose) printf("Acton RTS: checking for previous actor state in DDB... ");
        fflush(stdout);
        int no_items = remote_read_full_table_in_txn(&start_row, &end_row, ACTORS_TABLE, NULL, db);
        if (verbose) printf("done\n");
        //printf("Found %d existing actors\n", no_items);
        if (no_items > 0) {
            if (verbose) printf("Acton RTS: restoring actor state from DDB... ");
            fflush(stdout);
            deserialize_system(start_row);
            if (verbose) printf("done\n");
        } else {
            int indices[] = {0};
            db_schema_t* db_schema = db_create_schema(NULL, 1, indices, 1, indices, 0, indices, 0);
            create_db_queue(TIMER_QUEUE);
            timer_consume_hd = 0;
            BOOTSTRAP(new_argc, new_argv);
        }
    } else {
        BOOTSTRAP(new_argc, new_argv);
    }

    pthread_key_create(&self_key, NULL);
    // start worker threads, one per CPU
    pthread_t threads[num_cores];
    cpu_set_t cpu_set;
    for(int idx = 0; idx < num_cores; ++idx) {
        pthread_create(&threads[idx], NULL, main_loop, (void*)idx);
        CPU_ZERO(&cpu_set);
        CPU_SET(idx, &cpu_set);
        pthread_setaffinity_np(threads[idx], sizeof(cpu_set), &cpu_set);
    }
    
    for(int idx = 0; idx < num_cores; ++idx) {
        pthread_join(threads[idx], NULL);
    }
    return 0;
}
