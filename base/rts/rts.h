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
// Worker thread ids run from 0, the main thread, to ACTON_MAX_WTHREADS, which
// the max_wthreads build option sets. Every C file that includes this header
// must see the same value: $NEWACTOR stores SHARED_RQ in each new actor.
#define MAX_WTHREADS (ACTON_MAX_WTHREADS+1)
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
    uint64_t balance_ops; // lrq_balance takes, with or without work
    uint64_t balance_actors;
    // Looks by lrq_balance that read another worker's local queue. Each
    // look ends in a take (balance_ops) or in one of these: balance_young
    // when that queue was empty or the actor at its head did not become
    // ready BALANCE_NS before the actor at the head of this worker's queue,
    // or before the time this worker passed to DEQ_ready when its queue is
    // empty, and balance_stale when it did but lrq_cut, under that queue's
    // lock, found no actor at its head to take.
    uint64_t balance_looks;
    uint64_t balance_young;
    uint64_t balance_stale;
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
extern pthread_mutex_t sleep_lock;
extern pthread_cond_t work_to_do;

#define GET_WCTX() thread_wctx
#else
#define GET_WCTX() wctxs[0]
#endif

#ifndef ACTON_THREADS
// The actor whose continuation the program is running, or NULL when it runs
// none
extern $Actor self_actor;
#endif

typedef struct B_Msg *B_Msg;
typedef struct $ConstCont *$ConstCont;

extern struct B_MsgG_class B_MsgG_methods;
extern struct B_TimerHandleG_class B_TimerHandleG_methods;
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
    // Cancellation state of a timer message, TIMER_* below, in the padding
    // after $wait_lock, so that B_Msg keeps its size. Read and changed with
    // atomic operations: cancel(), the worker that is about to run the
    // message's continuation and handle_timeout can act on it at the same
    // time.
    uint8_t $timer;
    // Ready stamp (stamp_us in rts.c), in the padding after $wait_lock: the
    // time ENQ_ready was given when it last made the actor ready to run this
    // message. That is the end of the continuation whose result the worker
    // was handling, or else a read of the worker clock by the caller.
    uint32_t $ready_us;
    $WORD value;
    $int64 $globkey;
};

// States of B_Msg.$timer. A message starts TIMER_NONE, except a timer, the
// message of $AFTER or $AFTER_NOW, which starts TIMER_PENDING.
// TimerHandle.cancel() turns TIMER_PENDING into TIMER_CANCELLED, and the
// worker that is about to run the continuation of a TIMER_PENDING message
// turns it into TIMER_STARTED first. Both are compare-and-swaps, so only one
// of them happens: either the continuation never runs, or cancel() comes too
// late and does nothing. cancel() takes the message out of no queue. It goes
// on like any timer, from the outgoing queue to the timer queue or the
// mailbox, and the RTS drops it where it gets to it: in handle_timeout, which
// takes it from the timer queue when it is due and does not put it in the
// mailbox, or in the worker that takes it from the mailbox and finishes it
// without running the continuation.
#define TIMER_NONE       0      // not a timer
#define TIMER_PENDING    1      // a timer that cancel() can still stop
#define TIMER_STARTED    2      // a timer whose continuation has started
#define TIMER_CANCELLED  3      // dropped instead of run

// The TimerHandle builtin (class TimerHandle in __builtin__.act), which
// "h = after d: f()" binds. It refers to the timer message it can cancel. The
// compiler generates its class struct, in out/types/__builtin__.h, which
// builtin.h includes. The object layout is declared here, and the methods are
// defined in rts.c.
struct B_TimerHandle {
    struct B_TimerHandleG_class *$class;
    B_Msg $msg;             // the timer message that $AFTER or $AFTER_NOW made
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
B_TimerHandle $AFTER(B_float, $Cont);
B_TimerHandle $AFTER_NOW(B_float, $Cont);
B_TimerHandle B_TimerHandleG_newX(B_Msg m);
$R $AWAIT($Cont, B_Msg);

void init_db_queue(int64_t key);
void register_actor(int64_t key);

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
// A thread that is not a worker runs no continuation
#define GET_SELF() (thread_wctx ? thread_wctx->actor : NULL)
#define SET_SELF(a) thread_wctx->actor = a
#else
#define GET_SELF() self_actor
#define SET_SELF(a) self_actor = a
#endif

$R $PUSH_C($Cont);
B_BaseException $POP_C();
void $DROP_C();
#define $PUSHF_C $PUSH_C
#define $RAISE_C $RAISE

struct WorkerCtx;
typedef struct WorkerCtx *WorkerCtx;
// A worker writes its context for every continuation, new message and actor,
// and whenever it enters or leaves a try block, so no two contexts share a
// cache line (see struct mpmcq).
struct WorkerCtx {
    int64_t id;
    uv_loop_t *uv_loop;
    volatile JumpBuf jump_top;
    // The actor whose continuation this worker is running, or NULL when it
    // runs none. It is not a thread-local of its own: macOS keeps the
    // thread-locals of each thread in a small heap block, and the blocks of
    // several threads can share a cache line.
    $Actor actor;
    // The next key this worker hands out, and the distance to the one after
    // it (see get_next_key())
    int64_t key_next;
    int64_t key_step;
    // When this worker next looks at the injection queue first (look_due),
    // the state of its random choice of a worker to steal from, and whose
    // local queue it looks at next to balance the load (lrq_balance)
    int64_t inject_due;
    uint32_t steal_seed;
    uint64_t balance_next;
    // The worker loop's exception frame, at the bottom of the frame stack.
    // The first continuation on the worker pushes it, with a setjmp in that
    // call of wt_work_cb(). It stays pushed, and a raise that no try catches
    // longjmps to it also from later calls, which relies on uv running
    // wt_work_cb() at the same stack depth every time. It outlives each call,
    // so it cannot live on the stack.
    struct JumpBuf jump0;
} __attribute__((aligned(128)));
extern WorkerCtx wctxs[MAX_WTHREADS];
#ifdef ACTON_THREADS
// The context of the worker that runs on this thread, or NULL on a thread
// that is not a worker
extern _Thread_local WorkerCtx thread_wctx;
#endif

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

bool B_TimerHandleD___bool__(B_TimerHandle self);
B_str B_TimerHandleD___str__(B_TimerHandle self);
B_str B_TimerHandleD___repr__(B_TimerHandle self);
void B_TimerHandleD___serialize__(B_TimerHandle self, $Serial$state state);
B_TimerHandle B_TimerHandleD___deserialize__(B_TimerHandle res, $Serial$state state);
B_NoneType B_TimerHandleD_cancel(B_TimerHandle self);


$ROW $serialize_rts();
void $deserialize_rts($ROW);


void $register_rts();
