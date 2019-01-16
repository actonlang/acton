#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

//#define BASIC_OPS
#define  MUTEX_OPS
//#define  LFREE_OPS
//#define  OPSYS_OPS

#ifdef MUTEX_OPS
#include <pthread.h>
#endif

#ifdef LFREE_OPS
#include <stdatomic.h>
#endif


typedef void *WORD;

struct R;
struct Clos;
struct Msg;
struct Actor;

typedef struct R R;
typedef struct Clos *Clos;
typedef struct Msg *Msg;
typedef struct Actor *Actor;

enum RTAG { RDONE, RCONT, RWAIT };

struct R {
    int tag;
    Clos cont;
    WORD value;
};

struct Clos {
    R (*code)(Clos, WORD);
    WORD var[];
};

struct Msg {
    Msg next;
    Actor waiting;
    Clos clos;
#if defined(MUTEX_OPS)
    pthread_mutex_t mut;
#endif
    WORD value;
};

struct Actor {
    Actor next;
    Msg msg;
#if defined(MUTEX_OPS)
    pthread_mutex_t mut;
#endif
    WORD state[];
};

Clos    CLOS(R (*code)(Clos, WORD), int n);
Msg     MSG(Clos clos);
Actor   ACTOR(int n);

void    ENQ_ready(Actor n);
Actor   DEQ_ready();

bool    ENQ_msg(Msg m, Actor to);
bool    DEQ_msg(Actor to);

bool    ADD_waiting(Actor a, Msg m);
Actor   FREEZE_waiting(Msg m);
