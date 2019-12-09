#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>


typedef void *WORD;

struct R;
struct $CLOS;
struct Msg;
struct Actor;
struct Catcher;

typedef struct R R;
typedef struct $CLOS *$CLOS;
typedef struct Msg *Msg;
typedef struct Actor *Actor;
typedef struct Catcher *Catcher;

enum RTAG { RDONE, RFAIL, RCONT, RWAIT };
typedef enum RTAG RTAG;

struct R {
    RTAG  tag;
    $CLOS cont;
    WORD  value;
};

#define None (WORD)0

#define _DONE(value)       (R){RDONE, NULL,   (value)}
#define _FAIL(value)       (R){RFAIL, NULL,   (value)}
#define _CONT(cont, value) (R){RCONT, (cont), (value)}
#define _WAIT(cont, value) (R){RWAIT, (cont), (value)}

#define CLOS_HEADER     "CLOS"
#define MSG_HEADER      "Msg"
#define ACTOR_HEADER    "Actor"
#define CATCHER_HEADER  "Catcher"

struct $CLOS {
    char *header;
    R (*code)(WORD);
    int nvar;
    WORD var[];
};

struct Msg {
    char *header;
    Msg next;
    Actor to;
    $CLOS clos;
    Actor waiting;
    time_t baseline;
    volatile atomic_flag wait_lock;
    WORD value;
};

struct Actor {
    char *header;
    Actor next;
    Msg msg;
    Catcher catcher;
    volatile atomic_flag msg_lock;
    int nstate;
    WORD state[];
};

struct Catcher {
    char *header;
    Catcher next;
    $CLOS clos;
};

Msg MSG(Actor to, $CLOS clos, time_t baseline, WORD value);
Actor ACTOR(int n);
Catcher CATCHER($CLOS clos);

typedef R (*$fun1)(WORD);
typedef R (*$fun2)(WORD,WORD);
typedef R (*$fun3)(WORD,WORD,WORD);
typedef R (*$fun4)(WORD,WORD,WORD,WORD);

#define $CLOSE(code, nvar, ...)         $close((R(*)(WORD))code, nvar, __VA_ARGS__)
#define $ENTER(clos, arg)               $enter(clos, (WORD)arg)

$CLOS $close(R (*code)(WORD), int nvar, ...);
R $enter($CLOS, WORD);

Msg ASYNC(Actor to, $CLOS c);

Msg POSTPONE(Actor to, time_t sec, $CLOS c);

R AWAIT(Msg m, $CLOS th);

void PUSH($CLOS clos);

void POP();
