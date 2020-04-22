#pragma once

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>

#include "../builtin/builtin.h"

typedef void *$WORD;

struct $Msg;
struct $Actor;
struct $Catcher;
struct $Clos;
struct $Cont;

typedef struct $Msg *$Msg;
typedef struct $Actor *$Actor;
typedef struct $Catcher *$Catcher;
typedef struct $Clos *$Clos;
typedef struct $Cont *$Cont;

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
typedef struct $R $R;

#define $R_CONT(cont, arg)      ($R){$RCONT, (cont), ($WORD)(arg)}
#define $R_DONE(value)          ($R){$RDONE, NULL,   (value)}
#define $R_FAIL(value)          ($R){$RFAIL, NULL,   (value)}
#define $R_WAIT(cont, value)    ($R){$RWAIT, (cont), (value)}

#define $None ($WORD)0

#define MSG_HEADER              "Msg"
#define ACTOR_HEADER            "Actor"
#define CATCHER_HEADER          "Catcher"
#define CLOS_HEADER             "Clos"
#define CONT_HEADER             "Cont"

struct $Msg$class {
    char *$GCINFO;
    void (*__serialize__)($Msg, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
    $Msg (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    void (*__init__)($Msg, $Actor, $Cont, time_t, $WORD);
};
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

struct $Actor$class {
    char *$GCINFO;
    void (*__serialize__)($Actor, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
    $Actor (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    void (*__init__)($Actor);
};
struct $Actor {
    struct $Actor$class *$class;
    $Actor next;
    $Msg msg;
    $Catcher catcher;
    volatile atomic_flag msg_lock;
};

struct $Catcher$class {
    char *$GCINFO;
    void (*__serialize__)($Catcher, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
    $Catcher (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    void (*__init__)($Catcher, $Cont);
};
struct $Catcher {
    struct $Catcher$class *$class;
    $Catcher next;
    $Cont cont;
};

struct $Clos$class {
    char *$GCINFO;
    void (*__serialize__)($Clos, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
    $Clos (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    void (*__init__)($Clos);
    $WORD (*enter)($Clos, $WORD);
};
struct $Clos {
    struct $Clos$class *$class;
};

struct $Cont$class {
    char *$GCINFO;
    void (*__serialize__)($Cont, $Mapping$dict, $WORD*, int, $dict, $ROWLISTHEADER);
    $Cont (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    void (*__init__)($Cont);
    $R (*enter)($Cont, $WORD);
};
struct $Cont {
    union {
        struct $Cont$class *$class;
        struct $Clos super;
    };
};

$Msg $ASYNC($Actor, $Cont);
$Msg $AFTER(time_t, $Cont);
$R $AWAIT($Msg, $Cont);

void $PUSH($Cont);
void $POP();

#define $NEW($T, ...)       ({ $T $tmp = malloc(sizeof(struct $T)); $tmp->$class = &$T ## $methods; $tmp->$class->__init__($tmp, ##__VA_ARGS__); $tmp; })
#define $NEWCC($T, $c, ...) ({ $T $tmp = malloc(sizeof(struct $T)); $tmp->$class = &$T ## $methods; $tmp->$class->__init__($tmp, ##__VA_ARGS__, $c); })

typedef int $Env;
