#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <pthread.h>
#include <time.h>  // e.g. clock_gettime

#define MUTEX 0
#define LFREE 1
#define SPIN 2

// select implementation for ready Q
//#define READYQ MUTEX
//#define READYQ LFREE
#define READYQ SPIN

// select implementation for message Q
//#define MSGQ MUTEX
//#define MSGQ LFREE
#define MSGQ SPIN

// select implementation for waiting Q
#define WAITQ SPIN
//#define WAITQ LFREE

// select implementation for timer Q
#define TIMERQ SPIN

// validate ops implementation selection
#if READYQ != MUTEX && READYQ != SPIN
#error One of these must be defined: READYQ = (MUTEX | SPIN)
#endif
#if MSGQ != MUTEX && MSGQ != LFREE && MSGQ != SPIN
#error One of these must be defined: MSGQ = (MUTEX |_LFREE | SPIN)
#endif
#if WAITQ != MUTEX && WAITQ != LFREE && WAITQ != SPIN
#error One of these must be defined: WAITQ = (MUTEX | LFREE | SPIN)
#endif
#if TIMERQ != SPIN
#error One of these must be defined: TIMERQ = SPIN
#endif

#if READYQ == MUTEX || MSGQ == MUTEX || WAITQ == MUTEX
#include <pthread.h>
#endif

#include <stdatomic.h>

extern atomic_uint_least32_t clos_create_count;
extern atomic_uint_least64_t clos_create_time;
extern atomic_uint_least32_t msg_create_count;
extern atomic_uint_least64_t msg_create_time;
extern atomic_uint_least32_t readyQ_ins_count;
extern atomic_uint_least64_t readyQ_ins_time;
extern atomic_uint_least32_t readyQ_poll_count;
extern atomic_uint_least64_t readyQ_poll_time;
extern atomic_uint_least32_t msg_enq_count;
extern atomic_uint_least64_t msg_enq_time;
extern atomic_uint_least32_t msg_deq_count;
extern atomic_uint_least64_t msg_deq_time;
extern atomic_uint_least32_t wait_freeze_count;
extern atomic_uint_least32_t timer_ins_count;
extern atomic_uint_least64_t timer_ins_time;
extern atomic_uint_least32_t timer_poll_count;
extern atomic_uint_least64_t timer_poll_time;

typedef void *WORD;

typedef uint64_t monotonic_time;     // absolute time point, in nanoseconds since an unknown time in the past
typedef int64_t monotonic_duration;  // a time duration, expressed as the difference between two monotonic_time (might be negative)
#define MT_NANOS  ((monotonic_duration)1ull)
#define MT_MICROS ((monotonic_duration)(MT_NANOS*1000ull))
#define MT_MILLIS ((monotonic_duration)(MT_MICROS*1000ull))
#define MT_SECOND ((monotonic_duration)(MT_MILLIS*1000ull))
#define MT_MINUTE ((monotonic_duration)(MT_SECOND*60ull))
#define MT_HOUR   ((monotonic_duration)(MT_MINUTE*60ull))
//extern const monotonic_duration MT_MICROS;
//extern const monotonic_duration MT_MILLIS;
//extern const monotonic_duration MT_SECOND;
//extern const monotonic_duration MT_MINUTE;
//extern const monotonic_duration MT_HOUR;

static inline monotonic_time monotonic_now() {
    struct timespec t;

    clock_gettime(CLOCK_MONOTONIC, &t);

    return (uint64_t)(t.tv_sec * 1000000000ull) + (uint64_t)t.tv_nsec;
}


// forward declarations
struct Clos;
struct Msg;
struct Actor;

typedef struct Clos *Clos;
typedef struct Msg *Msg;
typedef struct Actor *Actor;

enum RTAG { RDONE, RCONT, RWAIT, REXIT };
typedef enum RTAG RTAG;

typedef struct {
    RTAG tag;
    Clos cont;
    WORD value;
} R;

typedef R (*code_t)(Clos, WORD);

struct Clos {
    code_t code;
    monotonic_time time_baseline;
    int nvar;
    WORD var[];
};
typedef struct Clos *Clos;

struct Actor {
    Actor next;
    Msg msgQ;
    Msg msgTail;
#if MSGQ == MUTEX
    pthread_mutex_t msg_lock;
#elif MSGQ == SPIN
    volatile atomic_flag msg_lock;
#endif
    WORD state[];
};
typedef struct Actor *Actor;

struct Msg {
    Actor to;
    Msg next;
    Actor waiting;
    Clos clos;
    monotonic_time time_baseline;
#if WAITQ == MUTEX
    pthread_mutex_t wait_lock;
#elif WAITQ == SPIN
    volatile atomic_flag wait_lock;
#endif
    WORD value;
};
typedef struct Msg *Msg;

struct TimedMsg {
    _Atomic Msg m;
    monotonic_time trigger_time;
    size_t pqueue_pos;   // used by priority queue implementation
};
typedef struct TimedMsg *TimedMsg;


// Allocate a Clos node with space for n var words.
Clos    CLOS(code_t code, int n);
// Allocate a Msg node.
Msg     MSG(Actor to, Clos clos);
// Allocate an Actor node with space for n state words.
Actor   ACTOR(int n);



// Atomic operaions required by the inner-most message processing loop:

// Initialize some global things ~ must be called precisely once.
void kernelops_INIT();
// Undos what INIT did ~ no kernel ops functions must be called after.
void kernelops_CLOSE();

// Operations for: Global ready "set"
//   these operations accesses the ready set atomically.
//   this container has no predetermined ordering.
// Push actor "a" to the global ready-set.
void    ready_INSERT(Actor a);
// Pops an actor from the global ready-set and,
//   returns actor, or NULL if none are ready.
Actor   ready_POLL();

// Operations for: An actor's message queue
//   these operations accesses an actor's message queue atomically.
//   this container is a FIFO.
// Enqueue message "m" onto the message queue of actor "a",
//   return true if this was the first message in the queue.
bool    msg_ENQ(Msg m, Actor a);
// Peeks at first message of actor "a"
//   returns NULL if there are no messages.
Msg     msg_PEEK(Actor a);
// Dequeue the first message from the queue of actor "a",
//   return true if the queue still holds messages after operation.
bool    msg_DEQ(Actor a);

// Operations for: A message's waiting "set"
//   these operations accesses a message's queue of waiting actors atomically.
//   this container has no predetermined ordering.
// Add actor "a" to the waiting "set" of message "m" if list is not frozen,
//   return whether the message was added or not.
bool    waiting_INSERT(Actor a, Msg m);
// "freeze" waiting list of message "m",
//   return waiting actors.
Actor   waiting_FREEZE(Msg m);

// Operations for: global postpone queue
//   thses operations accesses the global timer queue
//   the queue is ordered by their trigger timestamp.
// "trigger_time" is the timestamp specifying when to trigger the event.
//   the event, when triggered, executes the "c" closure.
TimedMsg timer_INSERT(monotonic_time trigger_time, Msg m);
// Check if the top-most element in the queue has passed (or is "close enough") its trigger time.
// the "now" timestamp is the current time.
// if so, the element is popped from the queue and a closure and an actor is returned.
//   if "trigger_time" is non-NULL, the actual scheduled trigger time is returned.
// if no element is to be triggered, it returns NULL leaving the queue ("a" and "trigger_time") unmodified.
TimedMsg timer_POLL(monotonic_time now);



static inline void spinlock_lock(volatile atomic_flag *f) {
    while (atomic_flag_test_and_set(f) == true) {
        // spin until we could set the flag
    }
}
static inline void spinlock_unlock(volatile atomic_flag *f) {
    atomic_flag_clear(f);
}


typedef uint64_t tsc_t;
#if defined(__clang__)
#define timestamp_tsc() ((tsc_t)__rdtsc())
#elif defined(__x86_64__)
static inline tsc_t timestamp_tsc() {
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return ( (tsc_t)lo)|( ((tsc_t)hi)<<32 );
}
#endif

double tsc2ns(tsc_t tsc);
extern double _tsc2ns;
