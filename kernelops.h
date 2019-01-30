#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <pthread.h>

//#define  BASIC_OPS
//#define  MUTEX_OPS
#define  LSPIN_OPS
//#define  LFREE_OPS


typedef void *WORD;

struct R;
struct Clos;
struct Msg;
struct Actor;

typedef struct R R;
typedef struct Clos *Clos;
typedef struct Msg *Msg;
typedef struct Actor *Actor;

enum RTAG { RDONE, RCONT, RWAIT, REXIT };
typedef enum RTAG RTAG;

struct R {
    RTAG tag;
    Clos cont;
    WORD value;
};

struct Clos {
    R (*code)(Clos, WORD);
    int nvar;
    WORD var[];
};

extern Actor readyQ;

#if defined(BASIC_OPS)
struct Msg {
    Msg next;
    Actor waiting;
    Clos clos;
    WORD value;
};

struct Actor {
    Actor next;
    Msg msgQ;
    Msg msgTail;
    WORD state[];
};
#endif

#if defined(MUTEX_OPS)
struct Msg {
    Msg next;
    Actor waiting;
    Clos clos;
    pthread_mutex_t mut;
    WORD value;
};

struct Actor {
    Actor next;
    Msg msgQ;
    Msg msgTail;
    pthread_mutex_t mut;
    WORD state[];
};
#endif

#if defined(LSPIN_OPS)

extern volatile int readySize;

struct Msg {
    Msg next;
    Actor waiting;
    Clos clos;
    _Atomic(int) lock;
    WORD value;
};

struct Actor {
    Actor next;
    Msg msgQ;
    Msg msgTail;
    _Atomic(int) lock;
    WORD state[];
};
#endif

#if defined(LFREE_OPS)
struct Msg {
    Msg next;
    Actor waiting;
    Clos clos;
    WORD value;
};

struct Actor {
    Actor next;
    Msg msgQ;
    Msg msgTail;
    WORD state[];
};
#endif


// Allocate a Clos node with space for n var words.
Clos    CLOS(R (*code)(Clos, WORD), int n);
// Allocate a Msg node.
Msg     MSG(Clos clos);
// Allocate an Actor node with space for n state words.
Actor   ACTOR(int n);

// Atomically enqueue actor "a" onto the global ready-queue.
void    ENQ_ready(Actor a);
// Atomically dequeue and return the first actor from the global ready-queue, 
// or return NULL.
Actor   DEQ_ready();

// Atomically enqueue message "m" onto the queue of actor "a", 
// return true if the queue was previously empty.
bool    ENQ_msg(Msg m, Actor a);
// Atomically dequeue the first message from the queue of actor "a",
// return true if the queue still holds messages.
bool    DEQ_msg(Actor a);

// Atomically add actor "a" to the waiting list of messasge "m" if it is not frozen (and return true),
// else immediately return false.
bool    ADD_waiting(Actor a, Msg m);
// Atomically freeze message "m" and return its list of waiting actors. 
Actor   FREEZE_waiting(Msg m);
