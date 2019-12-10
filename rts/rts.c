#include <unistd.h>  // sysconf()
#include <pthread.h>
#include <stdio.h>
#include <stdarg.h>

#include "rts.h"

#ifdef __gnu_linux__
    #define IS_GNU_LINUX
    #define USE_EPOLL
#elif  __APPLE__ && __MACH__
    #define IS_MACOS
    #define USE_KQUEUE
#endif


#if defined(IS_MACOS)
///////////////////////////////////////////////////////////////////////////////////////////////
#include <sys/types.h>
#include <sys/sysctl.h>
#include <mach/mach_init.h>
#include <mach/thread_policy.h>

#define SYSCTL_CORE_COUNT   "machdep.cpu.core_count"

typedef struct cpu_set {
  uint32_t    count;
} cpu_set_t;

static inline void
CPU_ZERO(cpu_set_t *cs) { cs->count = 0; }

static inline void
CPU_SET(int num, cpu_set_t *cs) { cs->count |= (1 << num); }

static inline int
CPU_ISSET(int num, cpu_set_t *cs) { return (cs->count & (1 << num)); }

int sched_getaffinity(pid_t pid, size_t cpu_size, cpu_set_t *cpu_set)
{
  int32_t core_count = 0;
  size_t  len = sizeof(core_count);
  int ret = sysctlbyname(SYSCTL_CORE_COUNT, &core_count, &len, 0, 0);
  if (ret) {
    printf("error while get core count %d\n", ret);
    return -1;
  }
  cpu_set->count = 0;
  for (int i = 0; i < core_count; i++) {
    cpu_set->count |= (1 << i);
  }

  return 0;
}

kern_return_t thread_policy_set(
					thread_t thread,
					thread_policy_flavor_t flavor,
					thread_policy_t policy_info,
					mach_msg_type_number_t count);

int pthread_setaffinity_np(pthread_t thread, size_t cpu_size, cpu_set_t *cpu_set) {
    int core = 0;
    for (core = 0; core < 8 * cpu_size; core++) {
        if (CPU_ISSET(core, cpu_set))
            break;
    }
    thread_affinity_policy_data_t policy = { core };

    thread_port_t mach_thread = pthread_mach_thread_np(thread);
    thread_policy_set(mach_thread, THREAD_AFFINITY_POLICY, (thread_policy_t)&policy, 1);

    return 0;
}
///////////////////////////////////////////////////////////////////////////////////////////////
#endif


$Actor readyQ = NULL;
volatile atomic_flag readyQ_lock;

$Msg timerQ = NULL;
volatile atomic_flag timerQ_lock;

pthread_key_t self_key;

$Actor root_actor = NULL;

static inline void spinlock_lock(volatile atomic_flag *f) {
    while (atomic_flag_test_and_set(f) == true) {
        // spin until we could set the flag
    }
}
static inline void spinlock_unlock(volatile atomic_flag *f) {
    atomic_flag_clear(f);
}

////////////////////////////////////////////////////////////////////////////////////////

// Allocate a Cont node.
$Cont $continuation($R (*code)(), int nvar, ...) {
    $Cont c = malloc(sizeof(struct $Cont) + nvar * sizeof($WORD));
    c->header = CONT_HEADER;
    c->code = code;
    c->nvar = nvar;
    va_list ap;
    va_start (ap, nvar);
    for (int x = 0; x < nvar; ++x) {
        c->var[x] = va_arg(ap, $WORD);
    }
    return c;
}

typedef $R (*$Cont0)();
typedef $R (*$Cont1)($WORD);
typedef $R (*$Cont2)($WORD,$WORD);
typedef $R (*$Cont3)($WORD,$WORD,$WORD);
typedef $R (*$Cont4)($WORD,$WORD,$WORD,$WORD);

// Apply a Cont node.
$R $continue($Cont c, $WORD arg) {
    switch (c->nvar) {
        case 3:  return (*(($Cont4)c->code))(c->var[0], c->var[1], c->var[2], arg);
        case 2:  return (*(($Cont3)c->code))(c->var[0], c->var[1], arg);
        case 1:  return (*(($Cont2)c->code))(c->var[0], arg);
        case 0:  return (*(($Cont1)c->code))(arg);
        default: return (*(($Cont0)c->code))();
    }
}

// Allocate a Msg node.
$Msg $MSG($Actor to, $Cont cont, time_t baseline, $WORD value) {
    $Msg m = malloc(sizeof(struct $Msg));
    m->header = MSG_HEADER;
    m->next = NULL;
    m->to = to;
    m->cont = cont;
    m->waiting = NULL;
    m->baseline = baseline;
    m->value = value;
    atomic_flag_clear(&m->wait_lock);
    return m;
}

// Allocate an Actor node with space for n state words.
$Actor $ACTOR(int n) {
    $Actor a = malloc(sizeof(struct $Actor) + n * sizeof($WORD));
    a->header = ACTOR_HEADER;
    a->next = NULL;
    a->msg = NULL;
    a->catcher = NULL;
    a->nstate = n;
    atomic_flag_clear(&a->msg_lock);
    return a;
}

$Catcher $CATCHER($Cont cont) {
    $Catcher c = malloc(sizeof(struct $Catcher));
    c->header = CATCHER_HEADER;
    c->next = NULL;
    c->cont = cont;
    return c;
}

////////////////////////////////////////////////////////////////////////////////////////

// Atomically enqueue actor "a" onto the global ready-queue.
void ENQ_ready($Actor a) {
    spinlock_lock(&readyQ_lock);
    if (readyQ) {
        $Actor x = readyQ;
        while (x->next)
            x = x->next;
        x->next = a;
    } else {
        readyQ = a;
    }
    a->next = NULL;
    spinlock_unlock(&readyQ_lock);
}

// Atomically dequeue and return the first actor from the global ready-queue, 
// or return NULL.
$Actor DEQ_ready() {
    spinlock_lock(&readyQ_lock);
    $Actor res = readyQ;
    if (res) {
        readyQ = res->next;
        res->next = NULL;
    }
    spinlock_unlock(&readyQ_lock);
    return res;
}

// Atomically enqueue message "m" onto the queue of actor "a", 
// return true if the queue was previously empty.
bool ENQ_msg($Msg m, $Actor a) {
    bool did_enq = true;
    spinlock_lock(&a->msg_lock);
    m->next = NULL;
    if (a->msg) {
        $Msg x = a->msg;
        while (x->next)
            x = x->next;
        x->next = m;
        did_enq = false;
    } else {
        a->msg = m;
    }
    spinlock_unlock(&a->msg_lock);
    return did_enq;
}

// Atomically dequeue the first message from the queue of actor "a",
// return true if the queue still holds messages.
bool DEQ_msg($Actor a) {
    bool has_more = false;
    spinlock_lock(&a->msg_lock);
    if (a->msg) {
        $Msg x = a->msg;
        a->msg = x->next;
        x->next = NULL;
        has_more = a->msg != NULL;
    }
    spinlock_unlock(&a->msg_lock);
    return has_more;
}

// Atomically add actor "a" to the waiting list of messasge "m" if it is not frozen (and return true),
// else immediately return false.
bool ADD_waiting($Actor a, $Msg m) {
    bool did_add = false;
    spinlock_lock(&m->wait_lock);
    if (m->cont) {
        a->next = m->waiting;
        m->waiting = a;
        did_add = true;
    }
    spinlock_unlock(&m->wait_lock);
    return did_add;
}

// Atomically freeze message "m" and return its list of waiting actors. 
$Actor FREEZE_waiting($Msg m) {
    spinlock_lock(&m->wait_lock);
    m->cont = NULL;
    spinlock_unlock(&m->wait_lock);
    $Actor res = m->waiting;
    m->waiting = NULL;
    return res;
}

// Atomically enqueue timed message "m" onto the global timer-queue, at position
// given by "m->baseline".
void ENQ_timed($Msg m) {
    time_t m_baseline = m->baseline;
    spinlock_lock(&timerQ_lock);
    $Msg x = timerQ;
    if (x && x->baseline <= m_baseline) {
        $Msg next = x->next;
        while (next && next->baseline <= m_baseline) {
            x = next;
            next = x->next;
        }
        x->next = m;
        m->next = next;
    } else {
        timerQ = m;
        m->next = x;
    }
    spinlock_unlock(&timerQ_lock);
}

// Atomically dequeue and return the first message from the global timer-queue if 
// its baseline is less or equal to "now", else return NULL.
$Msg DEQ_timed(time_t now) {
    spinlock_lock(&timerQ_lock);
    $Msg res = timerQ;
    if (res) {
        if (res->baseline <= now) {
            timerQ = res->next;
            res->next = NULL;
        } else {
            res = NULL;
        }
    }
    spinlock_unlock(&timerQ_lock);
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

char *RTAG_name($RTAG tag) {
    switch (tag) {
        case $RDONE: return "RDONE"; break;
        case $RFAIL: return "RFAIL"; break;
        case $RCONT: return "RCONT"; break;
        case $RWAIT: return "RWAIT"; break;
    }
}

void dump_cont($Cont c) {
    if (c == NULL) {
        printf("<NULL cont>");
    } else {
        printf("$Cont(%p", c->code);
        if (c->nvar > 0) printf(", ");
        for (int idx = 0; idx < c->nvar; ++idx) {
            if (idx > 0) printf(", ");
            printf("%p", c->var[idx]);
        }
        printf(")");
    }
    printf("\n");
}

#define _DONE(value)       ($R){$RDONE, NULL,   (value)}
#define _FAIL(value)       ($R){$RFAIL, NULL,   (value)}
#define _CONT(cont, value) ($R){$RCONT, (cont), (value)}
#define _WAIT(cont, value) ($R){$RWAIT, (cont), (value)}

$R DONE($WORD val) {
    return _DONE(val);
}

struct $Cont doneC = { CONT_HEADER, DONE, 0 };

$R WRITE_ROOT($WORD val) {
    root_actor = ($Actor)val;
    return _DONE($None);
}

struct $Cont write_rootC = { CONT_HEADER, WRITE_ROOT, 0 };

void BOOTSTRAP($Cont c) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    $Actor ancestor0 = $ACTOR(0);
    $Msg m = $MSG(ancestor0, c, now.tv_sec, &write_rootC);
    if (ENQ_msg(m, ancestor0)) {
        ENQ_ready(ancestor0);
    }
}

void PUSH_catcher($Actor a, $Catcher c) {
    c->next = a->catcher;
    a->catcher = c;
}

$Catcher POP_catcher($Actor a) {
    $Catcher c = a->catcher;
    a->catcher = c->next;
    c->next = NULL;
    return c;
}

$Msg $ASYNC($Actor to, $Cont c) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    time_t baseline = self->msg->baseline;
    $Msg m = $MSG(to, c, baseline, &doneC);
    if (ENQ_msg(m, to)) {
        ENQ_ready(to);
    }
    return m;
}

$Msg $AFTER(time_t sec, $Cont c) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    time_t baseline = self->msg->baseline + sec;
    $Msg m = $MSG(self, c, baseline, &doneC);
    ENQ_timed(m);
    return m;
}

$R $AWAIT($Msg m, $Cont th) {
    return _WAIT(th, m);
}

void $PUSH($Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    $Catcher c = $CATCHER(cont);
    PUSH_catcher(self, c);
}

void $POP() {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    POP_catcher(self);
}

////////////////////////////////////////////////////////////////////////////////////////

void *main_loop(void *arg) {
    while (1) {
        $Actor current = DEQ_ready();
        if (current) {
            pthread_setspecific(self_key, current);
            $Msg m = current->msg;

            $R r = $CONTINUE(m->cont, m->value);

            switch (r.tag) {
                case $RDONE: {
                    m->value = r.value;
                    $Actor b = FREEZE_waiting(m);        // Sets m->clos = NULL
                    while (b) {
                        b->msg->value = r.value;
                        ENQ_ready(b);
                        b = b->next;
                    }
                    if (DEQ_msg(current)) {
                        ENQ_ready(current);
                    }
                    break;
                }
                case $RCONT: {
                    m->cont = r.cont;
                    m->value = r.value;
                    ENQ_ready(current);
                    break;
                }
                case $RFAIL: {
                    $Catcher c = POP_catcher(current);
                    m->cont = c->cont;
                    m->value = r.value;
                    ENQ_ready(current);
                    break;
                }
                case $RWAIT: {
                    m->cont = r.cont;
                    $Msg x = ($Msg)r.value;
                    if (!ADD_waiting(current, x)) {
                        m->value = x->value;
                        ENQ_ready(current);
                    }
                    break;
                }
            }
        } else {
            struct timespec now;
            clock_gettime(CLOCK_MONOTONIC, &now);
            $Msg m = DEQ_timed(now.tv_sec);
            if (m) {
                if (ENQ_msg(m, m->to)) {
                    ENQ_ready(m->to);
                }
            } else {

                // TODO: do I/O polling

                static struct timespec idle_wait = { 0, 500000000 };  // 500ms
                nanosleep(&idle_wait, NULL);
            }
       }
    }
}


////////////////////////////////////////////////////////////////////////////////////////


$R ROOT($WORD arg, $Cont then);

int main(int argc, char **argv) {
    long num_cores = sysconf(_SC_NPROCESSORS_ONLN);
    printf("%ld worker threads\n", num_cores);

    pthread_key_create(&self_key, NULL);
    // start worker threads, one per CPU
    pthread_t threads[num_cores];
    cpu_set_t cpu_set;
    for(int idx = 0; idx < num_cores; ++idx) {
        pthread_create(&threads[idx], NULL, main_loop, (void*)idx);
        CPU_ZERO(&cpu_set);
        CPU_SET(idx, &cpu_set);
        pthread_setaffinity_np(threads[idx], sizeof(cpu_set), &cpu_set);
    }
    
    BOOTSTRAP($CONTINUATION(ROOT, 1, ($WORD)1));

    for(int idx = 0; idx < num_cores; ++idx) {
        pthread_join(threads[idx], NULL);
    }
}

/*

int selector_fd = kqueue()

handler[MAX_FD] = empty
event_spec[MAX_FD] = empty
sock_addr[MAX_FD] = empty
data_buffer[MAX_FD] = empty

for each thread:
    while (1):
        event_t event
        int nready = kevent_WAIT(selector_fd, &event, 1)
        int fd = (int)event.ident
        handler[fd](fd):
            handle_listen(fd):
                sockaddr
                while (int fd2 = accept(fd, &sockaddr)):
                    set_non_blocking(fd2)
                    try:
                        handler[fd2] = handle_connect
                        sock_addr[fd2] = sockaddr
                        bzero(&data_buffer[fd2])
                        EV_SET(event_spec[fd2], fd2, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, NULL)
                        kevent_CHANGE(selector_fd, &event_spec[fd2], 1)
                    catch:
                        event_spec[fd2].flags = EV_DELETE
                        kevent_CHANGE(selector_fd, &event_spec[fd2], 1)
            handle_connect(fd):
                if event_spec[fd].filter == EVFILT_READ:
                    count = read(&data_buffer[fd])
                elif event_spec[fd].filter == EVFILT_WRITE:
                    count = write(&data_buffer[fd])


to listen:
    try:
        fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        handler[fd] = handle_listen
        sock_addr[fd] = { AF_INET, address, htons(port) }
        bind(fd, sock_addr[fd])
        listen(fd, 65535)
        EV_SET(event_spec[fd], fd, EVFILT_READ, EV_ADD, 0, 0, NULL)
        kevent_CHANGE(selector_fd, &event_spec[fd], 1)
    catch:
        event_spec[fd].flags = EV_DELETE
        kevent_CHANGE(selector_fd, &event_spec[fd], 1)

    

*/
