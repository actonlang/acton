#pragma once

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <pthread.h>

#ifdef __gnu_linux__
    #define IS_GNU_LINUX
#elif  __APPLE__ && __MACH__
    #define IS_MACOS
#endif

#include "../builtin/builtin.h"

#define MAX_WTHREADS 256

extern long num_wthreads;

extern pthread_key_t pkey_wtid;
extern pthread_key_t pkey_uv_loop;
struct B_Msg;
struct $Actor;
struct $Catcher;
struct $ConstCont;

extern pthread_key_t self_key;
extern pthread_mutex_t sleep_lock;
extern pthread_cond_t work_to_do;

typedef struct B_Msg *B_Msg;
typedef struct $Actor *$Actor;
typedef struct $Catcher *$Catcher;
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

#define $Lock                   volatile atomic_flag

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
struct B_Msg {
    struct B_MsgG_class *$class;
    B_Msg $next;
    $Actor $to;
    $Cont $cont;
    $Actor $waiting;
    time_t $baseline;
    $Lock $wait_lock;
    $WORD B_value;
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
};
struct $Actor {
    struct $ActorG_class *$class;
    $Actor $next;
    B_Msg B_Msg;
    B_Msg $outgoing;
    B_Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock B_Msg_lock;
    $long $globkey;
    $int64 $affinity;
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

#define $NEWACTOR($T)       ({ $T $t = malloc(sizeof(struct $T)); \
                               $t->$class = &$T ## G_methods; \
                               $ActorG_methods.__init__(($Actor)$t); \
                               $t->$affinity = 0; \
                               init_db_queue($t->$globkey); \
                               register_actor($t->$globkey); \
                               $t; })

void $PUSH($Cont);
void $POP(B_i64);

extern B_Msg timerQ;

void wake_wt(int wtid);

time_t current_time();
time_t next_timeout();
void handle_timeout();
void rts_shutdown();

void pin_actor_affinity();

//typedef B_int B_Env;

void $Actor$serialize($Actor, B_NoneType);
void $Actor$deserialize($Actor, B_NoneType);

$ROW $serialize_rts();
void $deserialize_rts($ROW);


void $register_rts();
