#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <time.h>
#ifdef ACTON_THREADS
#include <pthread.h>
#endif
#include <setjmp.h>

#include <uv.h>

#ifdef __gnu_linux__
    #define IS_GNU_LINUX
#elif  __APPLE__ && __MACH__
    #define IS_MACOS
#endif

#include "common.h"
#include "../builtin/builtin.h"

#define MSGQ 2
#ifdef ACTON_THREADS
#define MAX_WTHREADS 256
#define NUM_RQS (MAX_WTHREADS+1)
// The shared RQ is the top-most readyQ
#define SHARED_RQ (NUM_RQS-1)
#else
#define MAX_WTHREADS 1
#define NUM_RQS 1
#define SHARED_RQ 0
#endif

#define REQUIRED_STACK_SIZE 8*1024*1024

#include "q.h"

extern int64_t num_wthreads;
// Each worker updates its own entry after every continuation. Entries get cache
// lines of their own, so that workers do not slow each other down.
struct wt_stat {
    unsigned int idx;          // worker thread index
    char key[10];              // thread index as string for convenience
    unsigned int state;        // current thread state
    uint64_t sleeps;           // number of times thread slept

    // Executing actor continuations is the primary work of the RTS, we measure
    // the execution time of each and count to buckets to get a rough idea of
    // how long it takes
    uint64_t conts_count; // number of executed continuations
    uint64_t conts_sum;   // nanoseconds spent running continuations
    uint64_t conts_100ns; // bucket for <100ns
    uint64_t conts_1us;   // bucket for <1us
    uint64_t conts_10us;  // bucket for <10us
    uint64_t conts_100us; // bucket for <100us
    uint64_t conts_1ms;   // bucket for <1ms
    uint64_t conts_10ms;  // bucket for <10ms
    uint64_t conts_100ms; // bucket for <100ms
    uint64_t conts_1s;     // bucket for <1s
    uint64_t conts_10s;    // bucket for <10s
    uint64_t conts_100s;   // bucket for <100s
    uint64_t conts_inf;   // bucket for <+Inf
    // Bookkeeping is the time a worker spends between continuations, and from
    // the start of its work callback to the first one: handling the result of
    // a continuation, taking locks, taking the next actor from a queue,
    // talking to the database etc
    uint64_t bkeep_count; // number of bookkeeping rounds
    uint64_t bkeep_sum;   // nanoseconds spent bookkeeping
    uint64_t bkeep_100ns; // bucket for <100ns
    uint64_t bkeep_1us;   // bucket for <1us
    uint64_t bkeep_10us;  // bucket for <10us
    uint64_t bkeep_100us; // bucket for <100us
    uint64_t bkeep_1ms;   // bucket for <1ms
    uint64_t bkeep_10ms;  // bucket for <10ms
    uint64_t bkeep_100ms; // bucket for <100ms
    uint64_t bkeep_1s;     // bucket for <1s
    uint64_t bkeep_10s;    // bucket for <10s
    uint64_t bkeep_100s;   // bucket for <100s
    uint64_t bkeep_inf;   // bucket for <+Inf
    // How long the actors this worker took from a ready queue had waited:
    // from their ready stamp (ENQ_ready) to the start of their continuation,
    // recorded only while the RTS mon output is on (record_waits in rts.c).
    // Both times have microsecond resolution, and waits are kept in
    // nanoseconds, so they are multiples of 1000. wait_1us counts the actors
    // whose two times fall in the same microsecond, and those whose
    // difference is negative, which counts as 0.
    uint64_t wait_count;  // number of actors taken
    uint64_t wait_sum;    // nanoseconds waited
    uint64_t wait_max;    // longest wait in nanoseconds
    uint64_t wait_1us;    // bucket for <1us
    uint64_t wait_10us;   // bucket for <10us
    uint64_t wait_100us;  // bucket for <100us
    uint64_t wait_1ms;    // bucket for <1ms
    uint64_t wait_10ms;   // bucket for <10ms
    uint64_t wait_100ms;  // bucket for <100ms
    uint64_t wait_1s;     // bucket for <1s
    uint64_t wait_inf;    // bucket for <+Inf
    // Actors this worker took from other workers' local queues
    uint64_t steal_ops;   // steals by this worker while it had no work
    uint64_t steal_actors;
} __attribute__((aligned(128)));    // see struct mpmcq
extern struct wt_stat wt_stats[MAX_WTHREADS];

// Only the worker that an entry belongs to writes it. Other threads read
// entries while the workers run (stats_to_json on the monitor threads,
// rts_stats in acton.rts), so once the workers run, every write and every
// read by another thread of a field that the workers write is a relaxed
// atomic load or store. idx and key are written only before the workers
// start. The worker adds with a relaxed load and a relaxed store: it is the
// only writer, so it needs no atomic read-modify-write.
#define WT_STAT_ADD(f, d) \
    __atomic_store_n(&(f), __atomic_load_n(&(f), __ATOMIC_RELAXED) + (d), __ATOMIC_RELAXED)
#define WT_STAT_SET(f, v) __atomic_store_n(&(f), (v), __ATOMIC_RELAXED)
#define WT_STAT_GET(f) __atomic_load_n(&(f), __ATOMIC_RELAXED)

struct B_Msg;
struct $ConstCont;

#ifdef ACTON_THREADS
extern pthread_key_t pkey_wctx;
extern pthread_key_t pkey_uv_loop;

extern pthread_key_t self_key;
extern pthread_mutex_t sleep_lock;
extern pthread_cond_t work_to_do;

#define GET_WCTX() (WorkerCtx)pthread_getspecific(pkey_wctx)
#else
#define GET_WCTX() wctxs[0]
#endif

extern $Actor self_actor;

typedef struct B_Msg *B_Msg;
typedef struct $ConstCont *$ConstCont;

extern struct B_MsgG_class B_MsgG_methods;
extern struct $ActorG_class $ActorG_methods;
extern struct $CatcherG_class $CatcherG_methods;
extern struct $ContG_class $DoneG_methods;
extern struct $ContG_class $FailG_methods;
extern struct $ConstContG_class $ConstContG_methods;

#define MSG_HEADER              "Msg"
#define ACTOR_HEADER            "Actor"
#define CATCHER_HEADER          "Catcher"
#define CLOS_HEADER             "Clos"

/*       Defined in builtin/__builtin__.h with wrong type for __init__
struct B_MsgG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Msg, $Actor, $Cont, time_t, $WORD);
    void (*__serialize__)(B_Msg, $Serial$state);
    B_Msg (*__deserialize__)(B_Msg, $Serial$state);
    B_bool (*__bool__)(B_Msg);
    B_str (*__str__)(B_Msg);
    B_str (*__repr__)(B_Msg);
};
*/
struct B_Msg {
    struct B_MsgG_class *$class;
    B_Msg $next;
    $Actor $to;
    $Cont $cont;
    $Actor $waiting;
    time_t $baseline;
    $Lock $wait_lock;
    // Ready stamp (stamp_us in rts.c), in the padding after $wait_lock: the
    // time ENQ_ready was given when it last made the actor ready to run this
    // message. That is the end of the continuation whose result the worker
    // was handling, or else a read of the worker clock by the caller.
    uint32_t $ready_us;
    $WORD value;
    $int64 $globkey;
};

struct $ActorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Actor);
    void (*__serialize__)($Actor, $Serial$state);
    $Actor (*__deserialize__)($Actor, $Serial$state);
    bool (*__bool__)($Actor);
    B_str (*__str__)($Actor);
    B_str (*__repr__)($Actor);
    B_NoneType (*__resume__)($Actor);
    B_NoneType (*__cleanup__)($Actor);
};
struct $Actor {
    struct $ActorG_class *$class;
    $Actor $next;
    B_Msg B_Msg;
    B_Msg B_Msg_tail;
    $Lock B_Msg_lock;
    $int64 $affinity;
    B_Msg $outgoing;
    B_Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $int64 $globkey;
};

struct $CatcherG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Catcher, $Cont);
    void (*__serialize__)($Catcher, $Serial$state);
    $Catcher (*__deserialize__)($Catcher, $Serial$state);
    bool (*__bool__)($Catcher);
    B_str (*__str__)($Catcher);
    B_str (*__repr__)($Catcher);
};
struct $Catcher {
    struct $CatcherG_class *$class;
    $Catcher $next;
    $Cont $cont;
    B_BaseException xval;
};


struct $ConstContG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($ConstCont, $WORD, $Cont);
    void (*__serialize__)($ConstCont, $Serial$state);
    $ConstCont (*__deserialize__)($ConstCont, $Serial$state);
    bool (*__bool__)($ConstCont);
    B_str (*__str__)($ConstCont);
    B_str (*__repr__)($ConstCont);
    $R (*__call__)($ConstCont, $WORD);
};
struct $ConstCont {
    struct $ConstContG_class *$class;
    $WORD val;
    $Cont cont;
};
$Cont $CONSTCONT($WORD, $Cont);

B_Msg $ASYNC($Actor, $Cont);
B_Msg $AFTER(B_float, $Cont);
B_Msg $AFTER_NOW(B_float, $Cont);
$R $AWAIT($Cont, B_Msg);

void init_db_queue(int64_t key);
void register_actor(int64_t key);
void serialize_state_shortcut($Actor);

#ifdef ACTON_DB
#define INIT_DB_QUEUE(key) init_db_queue(key)
#define REGISTER_ACTOR(key) register_actor(key)
#else
#define INIT_DB_QUEUE(key)
#define REGISTER_ACTOR(key)
#endif

#define $NEWACTOR($T)       ({ $T $t = GC_malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## G_methods; \
                               $ActorG_methods.__init__(($Actor)$t); \
                               $t->$affinity = SHARED_RQ; \
                               INIT_DB_QUEUE($t->$globkey); \
                               REGISTER_ACTOR($t->$globkey); \
                               $t; })

#define $InstallFinalizer(act, fn) GC_register_finalizer(act, fn, NULL, NULL, NULL)

#ifdef ACTON_THREADS
#define GET_SELF() ($Actor)pthread_getspecific(self_key)
#define SET_SELF(a) pthread_setspecific(self_key, (void *)a)
#define GET_WTID() (int)pthread_getspecific(pkey_wtid);
#else
#define GET_SELF() self_actor
#define SET_SELF(a) self_actor = a
#define GET_WTID() 0
#endif

$R $PUSH_C($Cont);
B_BaseException $POP_C();
void $DROP_C();
#define $PUSHF_C $PUSH_C
#define $RAISE_C $RAISE

struct WorkerCtx;
typedef struct WorkerCtx *WorkerCtx;
// A worker writes its context for every new message and actor, and whenever
// it enters or leaves a try block, so no two contexts share a cache line (see
// struct mpmcq).
struct WorkerCtx {
    int64_t id;
    uv_loop_t *uv_loop;
    volatile JumpBuf jump_top;
    // The next key this worker hands out, and the distance to the one after
    // it (see get_next_key())
    int64_t key_next;
    int64_t key_step;
    // When this worker next looks at the injection queue first (look_due),
    // and the state of its random choice of a worker to steal from
    int64_t inject_due;
    uint32_t steal_seed;
    // The worker loop's exception frame, at the bottom of the frame stack.
    // The first continuation on the worker pushes it, with a setjmp in that
    // call of wt_work_cb(). It stays pushed, and a raise that no try catches
    // longjmps to it also from later calls, which relies on uv running
    // wt_work_cb() at the same stack depth every time. It outlives each call,
    // so it cannot live on the stack.
    struct JumpBuf jump0;
} __attribute__((aligned(128)));
extern WorkerCtx wctxs[MAX_WTHREADS];

WorkerCtx get_wctx();
int get_wtid();

extern pid_t pid;

void wake_wt(int wtid);

time_t current_time();
bool next_timeout(time_t *due);
bool handle_timeout(time_t now);
void rts_shutdown();

void pin_actor_affinity();
void set_actor_affinity(int wthread_id);

//typedef B_int B_Env;

void $Actor$serialize($Actor, B_NoneType);
void $Actor$deserialize($Actor, B_NoneType);
B_NoneType $ActorD___cleanup__($Actor);

bool B_MsgD___bool__(B_Msg self);
B_str B_MsgD___str__(B_Msg self);
B_str B_MsgD___repr__(B_Msg self);
void B_MsgD___serialize__(B_Msg self, $Serial$state state);
B_Msg B_MsgD___deserialize__(B_Msg res, $Serial$state state);


$ROW $serialize_rts();
void $deserialize_rts($ROW);


void $register_rts();
