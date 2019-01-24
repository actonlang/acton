#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

//#define  BASIC_OPS
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

// Allocate a Clos node with space for n var words.
Clos    CLOS(R (*code)(Clos, WORD), int n);
// Allocate a Msg node.
Msg     MSG(Clos clos);
// Allocate an Actor node with space for n state words.
Actor   ACTOR(int n);


// Atomic operaions required by the inner-most message processing loop:

// Atomically push actor "a" to the global ready-set.
void    ready_PUSH(Actor a);
// Atomically pops an actor from the global ready-set and,
// returns actor, or NULL if none are ready.
Actor   ready_POP();

// Atomically enqueue message "m" onto the message queue of actor "a", 
// return true if this was the first message in the queue.
bool    msg_ENQ(Msg m, Actor a);
// Atomically dequeue the first message from the queue of actor "a",
// return true if the queue still holds messages after operation.
bool    msg_DEQ(Actor a);

// Atomically add actor "a" to the waiting list of message "m" if list is not frozen,
// return whether the message was added or not.
bool    waiting_ADD(Actor a, Msg m);
// Atomically "freeze" waiting list of message "m",
// return waiting actors.
Actor   waiting_FREEZE(Msg m);
