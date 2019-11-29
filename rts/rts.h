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
struct TimedMsg;

typedef struct R R;
typedef struct Clos *Clos;
typedef struct Msg *Msg;
typedef struct Actor *Actor;
typedef struct TimedMsg *TimedMsg;

enum RTAG { RDONE, RCONT, RWAIT, REXIT };
typedef enum RTAG RTAG;

struct R {
    RTAG tag;
    Clos cont;
    WORD value;
};

#define None (WORD)0

#define _DONE(cont, value) (R){RDONE, (cont), (value)}
#define _CONT(cont, value) (R){RCONT, (cont), (value)}
#define _WAIT(cont, value) (R){RWAIT, (cont), (value)}
#define _EXIT(cont, value) (R){REXIT, (cont), (value)}

struct Clos {
    R (*code)(Clos, WORD);
    int nvar;
    WORD var[];
};

struct Msg {
    Msg next;
    Actor waiting;
    Clos clos;
    time_t baseline;
    volatile atomic_flag wait_lock;
    WORD value;
};

struct Actor {
    Actor next;
    Msg msg;
    volatile atomic_flag msg_lock;
    WORD state[];
};

struct TimedMsg {
    TimedMsg next;
    Actor to;
    Clos clos;
    time_t baseline;  
};

Msg MSG(Clos clos);
Actor ACTOR(int n);
Clos CLOS(R (*code)(Clos, WORD), int n);
Clos CLOS1(R (*code)(Clos,WORD), WORD v0);
Clos CLOS2(R (*code)(Clos,WORD), WORD v0, WORD v1);    
Clos CLOS3(R (*code)(Clos,WORD), WORD v0, WORD v1, WORD v2);

Msg ASYNC(Actor to, Clos c);

TimedMsg POSTPONE(Actor to, time_t sec, Clos c);

R AWAIT(Msg m, Clos th);
