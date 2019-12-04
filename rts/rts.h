#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>


typedef void *WORD;

struct R;
struct Clos;
struct Msg;
struct Actor;
struct Catcher;

typedef struct R R;
typedef struct Clos *Clos;
typedef struct Msg *Msg;
typedef struct Actor *Actor;
typedef struct Catcher *Catcher;

enum RTAG { RDONE, RFAIL, RCONT, RWAIT };
typedef enum RTAG RTAG;

struct R {
    RTAG tag;
    Clos cont;
    WORD value;
};

#define None (WORD)0

#define _DONE(value)       (R){RDONE, NULL,   (value)}
#define _FAIL(value)       (R){RFAIL, NULL,   (value)}
#define _CONT(cont, value) (R){RCONT, (cont), (value)}
#define _WAIT(cont, value) (R){RWAIT, (cont), (value)}

#define CLOS_HEADER     "Clos"
#define MSG_HEADER      "Msg"
#define ACTOR_HEADER    "Actor"
#define CATCHER_HEADER  "Catcher"

struct Clos {
    char *header;
    R (*code)(Clos, WORD);
    int nvar;
    WORD var[];
};

struct Msg {
    char *header;
    Msg next;
    Actor to;
    Clos clos;
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
    Clos clos;
};

Msg MSG(Actor to, Clos clos, time_t baseline, WORD value);
Actor ACTOR(int n);
Catcher CATCHER(Clos clos);

Clos CLOS(R (*code)(Clos, WORD), int n);
Clos CLOS1(R (*code)(Clos,WORD), WORD v0);
Clos CLOS2(R (*code)(Clos,WORD), WORD v0, WORD v1);    
Clos CLOS3(R (*code)(Clos,WORD), WORD v0, WORD v1, WORD v2);

Msg ASYNC(Actor to, Clos c);

Msg POSTPONE(Actor to, time_t sec, Clos c);

R AWAIT(Msg m, Clos th);

void PUSH(Clos clos);

void POP();
