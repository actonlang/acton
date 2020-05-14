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

$Env $ENV = 10;

extern $R $ROOT($Env, $Cont);

$Actor root_actor = NULL;

$Actor readyQ = NULL;
volatile atomic_flag readyQ_lock;

$Msg timerQ = NULL;
volatile atomic_flag timerQ_lock;

pthread_key_t self_key;

static inline void spinlock_lock(volatile atomic_flag *f) {
    while (atomic_flag_test_and_set(f) == true) {
        // spin until we could set the flag
    }
}
static inline void spinlock_unlock(volatile atomic_flag *f) {
    atomic_flag_clear(f);
}

////////////////////////////////////////////////////////////////////////////////////////

void $Msg$__init__($Msg m, $Actor to, $Cont cont, time_t baseline, $WORD value) {
    m->next = NULL;
    m->to = to;
    m->cont = cont;
    m->waiting = NULL;
    m->baseline = baseline;
    m->value = value;
    atomic_flag_clear(&m->wait_lock);
}

void $Msg$__serialize__($Msg self, $Serial$state state) {
    $step_serialize(self->next,state);
    $step_serialize(self->to,state);
    $step_serialize(self->cont,state);
    $step_serialize(self->waiting,state);
    $val_serialize(ITEM_ID,&self->baseline,state);
    $step_serialize(self->value,state);
}


$Msg $Msg$__deserialize__($Serial$state state) {
  $Msg res = $DNEW($Msg,state);
    res->next = ($Msg)$step_deserialize(state);
    res->to = ($Actor)$step_deserialize(state);
    res->cont = ($Cont)$step_deserialize(state);
    res->waiting = ($Actor)$step_deserialize(state);
    res->baseline = (time_t)$val_deserialize(state);
    res->value = ($WORD)$step_deserialize(state);
    atomic_flag_clear(&res->wait_lock);
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $Actor$__init__($Actor a) {
    a->next = NULL;
    a->msg = NULL;
    a->catcher = NULL;
    atomic_flag_clear(&a->msg_lock);
}

void $Actor$__serialize__($Actor self, $Serial$state state) {
    $step_serialize(self->next,state);
    $step_serialize(self->msg,state);
    $step_serialize(self->catcher,state);
}

$Actor $Actor$__deserialize__($Serial$state state) {
  $Actor res = $DNEW($Actor,state);
    res->next = ($Actor)$step_deserialize(state);
    res->msg = ($Msg)$step_deserialize(state);
    res->catcher = ($Catcher)$step_deserialize(state);
    atomic_flag_clear(&res->msg_lock);
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $Catcher$__init__($Catcher c, $Cont cont) {
    c->next = NULL;
    c->cont = cont;
}

void $Catcher$__serialize__($Catcher self, $Serial$state state) {
    $step_serialize(self->next,state);
    $step_serialize(self->cont,state);
}

$Catcher $Catcher$__deserialize__($Serial$state state) {
    $Catcher res = $DNEW($Catcher,state);
    res->next = ($Catcher)$step_deserialize(state);
    res->cont = ($Cont)$step_deserialize(state);
    return res;
}
////////////////////////////////////////////////////////////////////////////////////////

void $Clos$__init__($Clos $this) { }

void $Clos$__serialize__($Clos self, $Serial$state state) {
    // TBD
}

$Clos $Clos$__deserialize__($Serial$state state) {
    // TBD
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////////////

void $Cont$__init__($Cont $this) { }

void $Cont$__serialize__($Cont self, $Serial$state state) {
    // TBD
}

$Cont $Cont$__deserialize__($Serial$state state) {
    // TBD
    return NULL;
}

////////////////////////////////////////////////////////////////////////////////////////

void $RetNew$__init__($RetNew $this, $Cont cont, $Actor act) {
    $this->cont = cont;
    $this->act = act;
}

void $RetNew$__serialize__($RetNew self, $Serial$state state) {
      $step_serialize(self->cont,state);
      $step_serialize(self->act,state);
}

$RetNew $RetNew$__deserialize__($Serial$state state) {
    $RetNew res = $DNEW($RetNew,state);
    res->cont = ($Cont)$step_deserialize(state);
    res->act = ($Actor)$step_deserialize(state);
    return res;
}

$R $RetNew$enter($RetNew $this, $WORD _ignore) {
    $Cont cont = $this->cont;
    return cont->$class->enter(cont, $this->act);
}

////////////////////////////////////////////////////////////////////////////////////////

struct $Msg$class $Msg$methods = {
    MSG_HEADER,
    NULL,
    $Msg$__init__,
    $Msg$__serialize__,
    $Msg$__deserialize__
};

struct $Actor$class $Actor$methods = {
    ACTOR_HEADER,
    NULL,
    $Actor$__init__,
    $Actor$__serialize__,
    $Actor$__deserialize__
};

struct $Catcher$class $Catcher$methods = {
    CATCHER_HEADER,
    NULL,
    $Catcher$__init__,
    $Catcher$__serialize__,
    $Catcher$__deserialize__
};

struct $Clos$class $Clos$methods = {
    CLOS_HEADER,
    NULL,
    $Clos$__init__,
    $Clos$__serialize__,
    $Clos$__deserialize__,
    NULL
};

struct $Cont$class $Cont$methods = {
    CONT_HEADER,
    NULL,
    $Cont$__init__,
    $Cont$__serialize__,
    $Cont$__deserialize__,
    NULL
};

struct $RetNew$class $RetNew$methods = {
    "$RetNew",
    NULL,
    $RetNew$__init__,
    $RetNew$__serialize__,
    $RetNew$__deserialize__,
    $RetNew$enter
};

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
////////////////////////////////////////////////////////////////////////////////////////
$R $DONE$enter($Cont $this, $WORD val) {
    return $R_DONE(val);
}

void $Done__serialize__($Cont self, $Serial$state state) {
  return;
}

$Cont $Done__deserialize__($Serial$state state) {
  $Cont res = $DNEW($Cont,state);
  res->$class = &$Done$methods;
  return res;
}

struct $Cont$class $Done$methods = {
    CONT_HEADER,
    NULL,
    $Cont$__init__,
    $Done__serialize__,
    $Done__deserialize__,
    $DONE$enter
};
struct $Cont $Done$instance = {
    &$Done$methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $NewRoot$enter ($Cont $this, $WORD val) {
    $Cont then = ($Cont)val;
    return $ROOT($ENV, then);
}

struct $Cont$class $NewRoot$methods = {
    CONT_HEADER,
    NULL,
    $Cont$__init__,
    NULL,
    NULL,
    $NewRoot$enter
};
struct $Cont $NewRoot$instance = {
    &$NewRoot$methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $WriteRoot$enter($Cont $this, $WORD val) {
    root_actor = ($Actor)val;
    return $R_DONE(val);
}

struct $Cont$class $WriteRoot$methods = {
    CONT_HEADER,
    NULL,
    $Cont$__init__,
    NULL,
    NULL,
    $WriteRoot$enter
};
struct $Cont $WriteRoot$instance = {
    &$WriteRoot$methods
};
////////////////////////////////////////////////////////////////////////////////////////

void BOOTSTRAP() {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    $Cont cont = &$NewRoot$instance;
    $Actor ancestor0 = $NEW($Actor);
    $Msg m = $NEW($Msg, ancestor0, cont, now.tv_sec, &$WriteRoot$instance);
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

$Msg $ASYNC($Actor to, $Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    time_t baseline = self->msg->baseline;
    $Msg m = $NEW($Msg, to, cont, baseline, &$Done$instance);
    if (ENQ_msg(m, to)) {
        ENQ_ready(to);
    }
    return m;
}

$Msg $AFTER(time_t sec, $Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    time_t baseline = self->msg->baseline + sec;
    $Msg m = $NEW($Msg, self, cont, baseline, &$Done$instance);
    ENQ_timed(m);
    return m;
}

$R $AWAIT($Msg m, $Cont cont) {
    return $R_WAIT(cont, m);
}

void $PUSH($Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    $Catcher c = $NEW($Catcher, cont);
    PUSH_catcher(self, c);
}

void $POP() {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    POP_catcher(self);
}

////////////////////////////////////////////////////////////////////////////////////////

void *main_loop(void *arg) {
#ifdef EXPERIMENT
    int i = 0;
#endif
    while (1) {
        $Actor current = DEQ_ready();
        if (current) {
            pthread_setspecific(self_key, current);
            $Msg m = current->msg;
            $Cont cont = m->cont;
            $WORD val = m->value;
            
            $R r = cont->$class->enter(cont, val);
            switch (r.tag) {
                case $RDONE: {
                    m->value = r.value;
                    $Actor b = FREEZE_waiting(m);        // Sets m->cont = NULL
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
#ifdef EXPERIMENT
                if ((int)arg==0) {
                  i++;         
                  if (i== 3) {
                      printf("# Serializing root = %ld\n", (long)root_actor);
                    $write_serialized($serialize_rts(),"rts.bin");
                  }
                  if (i == 20) {
                      printf("# Deserializing\n");
                    $ROW row = $read_serialized("rts.bin");
                    $deserialize_rts(row);
                    $write_serialized($serialize_rts(),"rts2.bin");
                    i = 0;
                  }
                }
#endif
                // TODO: do I/O polling
              static struct timespec idle_wait = { 0, 500000000 };  // 500ms
              nanosleep(&idle_wait, NULL);
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////

$ROW $serialize_rts() {
  $Serial$state state = malloc(sizeof(struct $Serial$state));
  state->done = $NEW($dict,($Hashable)$Hashable$WORD$witness,NULL);
  state->row_no = 0;
  state->fst = NULL;
  state->row = NULL;
  $step_serialize(root_actor,state);
  spinlock_lock(&readyQ_lock);
  spinlock_lock(&timerQ_lock);
  $step_serialize(readyQ,state);
  $step_serialize(timerQ,state);
  spinlock_unlock(&timerQ_lock);
  spinlock_unlock(&readyQ_lock);
  return state->fst;
}

void $deserialize_rts($ROW row) {
  $Serial$state state = malloc(sizeof(struct $Serial$state));
  state->done = $NEW($dict,($Hashable)$Hashable$int$witness,NULL);
  state->row_no = 0;
  state->row = row;
  root_actor = ($Actor)$step_deserialize(state);
  spinlock_lock(&readyQ_lock);
  spinlock_lock(&timerQ_lock);
  readyQ = ($Actor)$step_deserialize(state);
  timerQ = ($Msg)$step_deserialize(state);
  spinlock_unlock(&timerQ_lock);
  spinlock_unlock(&readyQ_lock);
}

////////////////////////////////////////////////////////////////////////////////////////

void $register_rts () {
  $register_force(MSG_ID,($Serializable$methods)&$Msg$methods);
  $register_force(ACTOR_ID,($Serializable$methods)&$Actor$methods);
  $register_force(CATCHER_ID,($Serializable$methods)&$Catcher$methods);
  $register_force(CLOS_ID,($Serializable$methods)&$Clos$methods);
  $register_force(CONT_ID,($Serializable$methods)&$Cont$methods);
  $register_force(DONE_ID,($Serializable$methods)&$Done$methods);
  $register_force(RETNEW_ID,($Serializable$methods)&$RetNew$methods);
}
 
////////////////////////////////////////////////////////////////////////////////////////

int main(int argc, char **argv) {
#ifdef EXPERIMENT
    long num_cores = 1;    
#else
    long num_cores = sysconf(_SC_NPROCESSORS_ONLN);
#endif
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
    $register_builtin();
    $register_rts();
    
    BOOTSTRAP();

    for(int idx = 0; idx < num_cores; ++idx) {
        pthread_join(threads[idx], NULL);
    }
    return 0;
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
