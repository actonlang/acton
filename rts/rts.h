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
struct $Msg;
struct $Actor;
struct $Catcher;
struct $ConstCont;

extern pthread_key_t self_key;
extern pthread_mutex_t sleep_lock;
extern pthread_cond_t work_to_do;

typedef struct $Msg *$Msg;
typedef struct $Actor *$Actor;
typedef struct $Catcher *$Catcher;
typedef struct $ConstCont *$ConstCont;

extern struct $MsgG_class $MsgG_methods;
extern struct $ActorG_class $ActorG_methods;
extern struct $CatcherG_class $CatcherG_methods;
extern struct $ContG_class $DoneG_methods;
extern struct $ConstContG_class $ConstContG_methods;

#define MSG_HEADER              "Msg"
#define ACTOR_HEADER            "Actor"
#define CATCHER_HEADER          "Catcher"
#define CLOS_HEADER             "Clos"

#define $Lock                   volatile atomic_flag

struct $MsgG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Msg, $Actor, $Cont, time_t, $WORD);
    void (*__serialize__)($Msg, $NoneType);
    $Msg (*__deserialize__)($Msg, $NoneType);
    B_bool (*__bool__)($Msg);
    B_str (*__str__)($Msg);
    B_str (*__repr__)($Msg);
};
struct $Msg {
    struct $MsgG_class *$class;
    $Msg $next;
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
    void (*__serialize__)($Actor, $NoneType);
    $Actor (*__deserialize__)($Actor, $NoneType);
    B_bool (*__bool__)($Actor);
    B_str (*__str__)($Actor);
    B_str (*__repr__)($Actor);
    $NoneType (*__resume__)($Actor);
};
struct $Actor {
    struct $ActorG_class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Msg $waitsfor;
    B_int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
    B_int64 $affinity;
};

struct $CatcherG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)($Catcher, $Cont);
    void (*__serialize__)($Catcher, $NoneType);
    $Catcher (*__deserialize__)($Catcher, $NoneType);
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
    void (*__serialize__)($ConstCont, $NoneType);
    $ConstCont (*__deserialize__)($ConstCont, $NoneType);
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

$Msg $ASYNC($Actor, $Cont);
$Msg $AFTER(B_float, $Cont);
$R $AWAIT($Cont, $Msg);

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
void $POP();

extern $Msg timerQ;

void wake_wt(int wtid);

time_t current_time();
time_t next_timeout();
void handle_timeout();
void rts_shutdown();

void pin_actor_affinity();

//typedef B_int $Env;

void $Actor$serialize($Actor, $NoneType);
void $Actor$deserialize($Actor, $NoneType);

$ROW $serialize_rts();
void $deserialize_rts($ROW);


void $register_rts();
