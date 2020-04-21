#pragma once

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>

#include "../builtin/builtin.h"

typedef void *$WORD;

struct $R;
struct $Msg;
struct $Actor;
struct $Catcher;
struct $Clos;
struct $Cont;

typedef struct $R $R;
typedef struct $Msg *$Msg;
typedef struct $Actor *$Actor;
typedef struct $Catcher *$Catcher;
typedef struct $Clos *$Clos;
typedef struct $Cont *$Cont;

struct $Msg$class;
struct $Actor$class;
struct $Catcher$class;
struct $Clos$class;
struct $Cont$class;

extern struct $Msg$class $Msg$methods;
extern struct $Actor$class $Actor$methods;
extern struct $Catcher$class $Catcher$methods;
extern struct $Clos$class $Clos$methods;
extern struct $Cont$class $Cont$methods;

enum $RTAG { $RDONE, $RFAIL, $RCONT, $RWAIT };
typedef enum $RTAG $RTAG;

struct $R {
    $RTAG tag;
    $Cont cont;
    $WORD value;
};

#define $R_CONT(cont, arg)      ($R){$RCONT, (cont), ($WORD)(arg)}
#define $R_DONE(value)          ($R){$RDONE, NULL,   (value)}
#define $R_FAIL(value)          ($R){$RFAIL, NULL,   (value)}
#define $R_WAIT(cont, value)    ($R){$RWAIT, (cont), (value)}

#define $None ($WORD)0

#define MSG_HEADER      "Msg"
#define ACTOR_HEADER    "ACTOR"
#define CATCHER_HEADER  "Catcher"
#define CLOS_HEADER     "CLOS"
#define CONT_HEADER     "Cont"

struct $Msg {
    struct $Msg$class *$class;
    $Msg next;
    $Actor to;
    $Cont cont;
    $Actor waiting;
    time_t baseline;
    volatile atomic_flag wait_lock;
    $WORD value;
};
struct $Msg$class {
    char *header;
    void (*__init__)($Msg, $Actor, $Cont, time_t, $WORD);
};

struct $Actor {
    struct $Actor$class *$class;
    $Actor next;
    $Msg msg;
    $Catcher catcher;
    volatile atomic_flag msg_lock;
};
struct $Actor$class {
    char *GCINFO;
    void (*__init__)($Actor);
    $WORD (*$enter)($Actor, $WORD);
};

struct $Catcher {
    struct $Catcher$class *$class;
    $Catcher next;
    $Cont cont;
};
struct $Catcher$class {
    char *header;
    void (*__init__)($Catcher, $Cont);
};

struct $Clos {
    struct $Clos$class *$class;
};
struct $Clos$class {
    char *GCINFO;
    void (*__init__)($Clos);
    $WORD (*$enter)($Clos, $WORD);
};

struct $Cont {
    union {
        struct $Cont$class *$class;
        struct $Clos super;
    };
};
struct $Cont$class {
    char *GCINFO;
    void (*__init__)($Cont);
    $R (*$enter)($Cont, $WORD);
};

$Msg $ASYNC($Actor, $Cont);
$Msg $AFTER(time_t, $Cont);
$R $AWAIT($Msg, $Cont);

void $PUSH($Cont);
void $POP();

#define $NEW($T, ...)       ({ $T $tmp = malloc(sizeof(struct $T)); $tmp->$class = &$T ## $methods; $tmp->$class->__init__($tmp, ##__VA_ARGS__); $tmp; })
#define $NEWCC($T, $c, ...) ({ $T $tmp = malloc(sizeof(struct $T)); $tmp->$class = &$T ## $methods; $tmp->$class->__init__($tmp, ##__VA_ARGS__, $c); })

typedef int $Env;
