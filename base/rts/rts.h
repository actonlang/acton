#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <pthread.h>
#include <setjmp.h>

#ifdef __gnu_linux__
    #define IS_GNU_LINUX
#elif  __APPLE__ && __MACH__
    #define IS_MACOS
#endif

#include "common.h"
#include "../builtin/builtin.h"

#define MSGQ 2
#define MAX_WTHREADS 256
#define NUM_RQS (MAX_WTHREADS+1)
// The shared RQ is the top-most readyQ
#define SHARED_RQ (NUM_RQS-1)

#include "q.h"

extern long num_wthreads;
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
    // Avoid cache trashing by aligning on cache line size (64!?)
    char padding[56];
};
extern struct wt_stat wt_stats[MAX_WTHREADS];

extern pthread_key_t pkey_wtid;
extern pthread_key_t pkey_uv_loop;
struct B_Msg;
struct $ConstCont;

extern pthread_key_t self_key;
extern pthread_mutex_t sleep_lock;
extern pthread_cond_t work_to_do;

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
    $WORD value;
    $long $globkey;
};

struct $ActorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Actor);
    void (*__serialize__)($Actor, $Serial$state);
    $Actor (*__deserialize__)($Actor, $Serial$state);
    B_bool (*__bool__)($Actor);
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
    $long $globkey;
};

struct $CatcherG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Catcher, $Cont);
    void (*__serialize__)($Catcher, $Serial$state);
    $Catcher (*__deserialize__)($Catcher, $Serial$state);
    B_bool (*__bool__)($Catcher);
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
    B_bool (*__bool__)($ConstCont);
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
$R $AWAIT($Cont, B_Msg);

void init_db_queue(long);
void register_actor(long key);
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

#define $GCfinalizer(act, fn) GC_register_finalizer(act, fn, NULL, NULL, NULL)

$R $PUSH_C($Cont);
B_BaseException $POP_C();
void $DROP_C();
#define $PUSHF_C $PUSH_C
#define $RAISE_C $RAISE

struct JumpBuf;
typedef struct JumpBuf *JumpBuf;
struct JumpBuf {
    jmp_buf buf;
    B_BaseException xval;
    JumpBuf prev;
};

JumpBuf $PUSH_BUF();
B_BaseException $POP();
void $DROP();
void $RAISE(B_BaseException e);
#define $PUSH()             (!setjmp($PUSH_BUF()->buf))
#define $PUSHF $PUSH

extern B_Msg timerQ;

void wake_wt(int wtid);

time_t current_time();
time_t next_timeout();
void handle_timeout();
void rts_shutdown();

void pin_actor_affinity();
void set_actor_affinity(int wthread_id);

//typedef B_int B_Env;

void $Actor$serialize($Actor, B_NoneType);
void $Actor$deserialize($Actor, B_NoneType);
B_NoneType $ActorD___cleanup__($Actor);

B_bool B_MsgD___bool__(B_Msg self); 
B_str B_MsgD___str__(B_Msg self);
B_str B_MsgD___repr__(B_Msg self); 
void B_MsgD___serialize__(B_Msg self, $Serial$state state);
B_Msg B_MsgD___deserialize__(B_Msg res, $Serial$state state);


$ROW $serialize_rts();
void $deserialize_rts($ROW);


void $register_rts();
