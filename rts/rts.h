#pragma once

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <pthread.h>

#include "../builtin/builtin.h"

struct $Msg;
struct $Actor;
struct $Catcher;
struct $function;
struct $Cont;
struct $ConstCont;

pthread_key_t self_key;
extern pthread_mutex_t sleep_lock;
extern pthread_cond_t work_to_do;

typedef struct $Msg *$Msg;
typedef struct $Actor *$Actor;
typedef struct $Catcher *$Catcher;
typedef struct $function *$function;
typedef struct $Cont *$Cont;
typedef struct $ConstCont *$ConstCont;

extern struct $Msg$class $Msg$methods;
extern struct $Actor$class $Actor$methods;
extern struct $Catcher$class $Catcher$methods;
extern struct $function$class $function$methods;
extern struct $Cont$class $Cont$methods;
extern struct $Cont$class $Done$methods;
extern struct $ConstCont$class $ConstCont$methods;

enum $RTAG { $RDONE, $RFAIL, $RCONT, $RWAIT };
typedef enum $RTAG $RTAG;

struct $R {
    $RTAG tag;
    $Cont cont;
    $WORD value;
};
typedef struct $R $R;

#define $R_CONT(cont, arg)      ($R){$RCONT, (cont), ($WORD)(arg)}
#define $R_DONE(value)          ($R){$RDONE, NULL,   (value)}
#define $R_FAIL(value)          ($R){$RFAIL, NULL,   (value)}
#define $R_WAIT(cont, value)    ($R){$RWAIT, (cont), (value)}

#define MSG_HEADER              "Msg"
#define ACTOR_HEADER            "Actor"
#define CATCHER_HEADER          "Catcher"
#define CLOS_HEADER             "Clos"
#define CONT_HEADER             "Cont"

#define $Lock                   volatile atomic_flag

struct $Msg$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Msg, $Actor, $Cont, time_t, $WORD);
    void (*__serialize__)($Msg, $Serial$state);
    $Msg (*__deserialize__)($Serial$state);
    $bool (*__bool__)($Msg);
    $str (*__str__)($Msg);
};
struct $Msg {
    struct $Msg$class *$class;
    $Msg next;
    $Actor to;
    $Cont cont;
    $Actor waiting;
    time_t baseline;
    $Lock wait_lock;
    $WORD value;
};

struct $Actor$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Actor);
    void (*__serialize__)($Actor, $Serial$state);
    $Actor (*__deserialize__)($Serial$state);
    $bool (*__bool__)($Actor);
    $str (*__str__)($Actor);
};
struct $Actor {
    struct $Actor$class *$class;
    $Actor next;
    $Msg msg;
    $Msg outgoing;
    $Catcher catcher;
    $Lock msg_lock;
};

struct $Catcher$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Catcher, $Cont);
    void (*__serialize__)($Catcher, $Serial$state);
    $Catcher (*__deserialize__)($Serial$state);
    $bool (*__bool__)($Catcher);
    $str (*__str__)($Catcher);
};
struct $Catcher {
    struct $Catcher$class *$class;
    $Catcher next;
    $Cont cont;
};

struct $function$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($function);
    void (*__serialize__)($function, $Serial$state);
    $function (*__deserialize__)($Serial$state);
    $bool (*__bool__)($function);
    $str (*__str__)($function);
    $WORD (*__call__)($function, ...);
};
struct $function {
    struct $function$class *$class;
};

struct $Cont$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Cont);
    void (*__serialize__)($Cont, $Serial$state);
    $Cont (*__deserialize__)($Serial$state);
    $bool (*__bool__)($Cont);
    $str (*__str__)($Cont);
    $R (*__call__)($Cont, ...);
};
struct $Cont {
    struct $Cont$class *$class;
};

struct $ConstCont$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($ConstCont, $WORD, $Cont);
    void (*__serialize__)($ConstCont, $Serial$state);
    $ConstCont (*__deserialize__)($Serial$state);
    $bool (*__bool__)($ConstCont);
    $str (*__str__)($ConstCont);
    $R (*__call__)($ConstCont, $WORD);
};
struct $ConstCont {
    struct $ConstCont$class *$class;
    $WORD val;
    $Cont cont;
};
$Cont $CONSTCONT($WORD, $Cont);

$Msg $ASYNC($Actor, $Cont);
$Msg $AFTER($int, $Cont);
$R $AWAIT($Msg, $Cont);

void $PUSH($Cont);
void $POP();

//typedef $int $Env;

$ROW $serialize_rts();
void $deserialize_rts($ROW);


void $register_rts();
