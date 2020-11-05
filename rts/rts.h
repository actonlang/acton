#pragma once

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>

#include "../builtin/builtin.h"

struct $Msg;
struct $Actor;
struct $Catcher;
struct $Clos;
struct $Cont;
struct $RetNew;

typedef struct $Msg *$Msg;
typedef struct $Actor *$Actor;
typedef struct $Catcher *$Catcher;
typedef struct $Clos *$Clos;
typedef struct $Cont *$Cont;
typedef struct $RetNew *$RetNew;

extern struct $Msg$class $Msg$methods;
extern struct $Actor$class $Actor$methods;
extern struct $Catcher$class $Catcher$methods;
extern struct $Clos$class $Clos$methods;
extern struct $Cont$class $Cont$methods;
extern struct $Cont$class $Done$methods;
extern struct $RetNew$class $RetNew$methods;

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

struct $Clos$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($Clos);
    void (*__serialize__)($Clos, $Serial$state);
    $Clos (*__deserialize__)($Serial$state);
    $bool (*__bool__)($Clos);
    $str (*__str__)($Clos);
    $WORD (*enter)($Clos, $WORD);
};
struct $Clos {
    struct $Clos$class *$class;
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
    $R (*enter)($Cont, $WORD);
};
struct $Cont {
    union {
        struct $Cont$class *$class;
        struct $Clos super;
    };
};

struct $RetNew$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)($RetNew, $Cont, $Actor);
    void (*__serialize__)($RetNew, $Serial$state);
    $RetNew (*__deserialize__)($Serial$state);
    $bool (*__bool__)($RetNew);
    $str (*__str__)($RetNew);
    $R (*enter)($RetNew, $WORD);
};
struct $RetNew {
    struct $RetNew$class *$class;
    $Cont cont;
    $Actor act;
};

$Msg $ASYNC($Actor, $Cont);
$Msg $AFTER(time_t, $Cont);
$R $AWAIT($Msg, $Cont);

void $PUSH($Cont);
void $POP();

typedef int $Env;

$ROW $serialize_rts();
void $deserialize_rts($ROW);


void $register_rts();
