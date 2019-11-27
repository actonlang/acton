#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>


static inline void spinlock_lock(volatile atomic_flag *f) {
    while (atomic_flag_test_and_set(f) == true) {
        // spin until we could set the flag
    }
}
static inline void spinlock_unlock(volatile atomic_flag *f) {
    atomic_flag_clear(f);
}


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
    volatile atomic_flag wait_lock;
    WORD value;
};

struct Actor {
    Actor next;
    Msg msg;
    volatile atomic_flag msg_lock;
    WORD state[];
};

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
