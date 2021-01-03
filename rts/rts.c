#include <unistd.h>  // sysconf()
#include <pthread.h>
#include <stdio.h>
#include <stdarg.h>

#include "rts.h"
#include "../builtin/minienv.h"
#include "../backend/client_api.h"

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
#endif

extern $R $ROOT($Env, $Cont);

$Actor root_actor = NULL;

$Actor readyQ = NULL;
$Lock readyQ_lock;

$Msg timerQ = NULL;
$Lock timerQ_lock;

int64_t next_key = 0;
$Lock next_key_lock;

$list args = NULL;

pthread_key_t self_key;
pthread_mutex_t sleep_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t work_to_do = PTHREAD_COND_INITIALIZER;

static inline void spinlock_lock($Lock *f) {
    while (atomic_flag_test_and_set(f) == true) {
        // spin until we could set the flag
    }
}
static inline void spinlock_unlock($Lock *f) {
    atomic_flag_clear(f);
}

int64_t get_next_key() {
    spinlock_lock(&next_key_lock);
    int64_t res = --next_key;
    spinlock_unlock(&next_key_lock);
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $Msg$__init__($Msg m, $Actor to, $Cont cont, time_t baseline, $WORD value) {
    m->$next = NULL;
    m->$to = to;
    m->$cont = cont;
    m->$waiting = NULL;
    m->$baseline = baseline;
    m->$value = value;
    atomic_flag_clear(&m->$wait_lock);
    m->$globkey = get_next_key();
}

$bool $Msg$__bool__($Msg self) {
  return $True;
}

$str $Msg$__str__($Msg self) {
  char *s;
  asprintf(&s,"<$Msg object at %p>",self);
  return to$str(s);
}

void $Msg$__serialize__($Msg self, $Serial$state state) {
    $step_serialize(self->$next,state);          // REMOVE!
    $step_serialize(self->$to,state);
    $step_serialize(self->$cont,state);
    $val_serialize(ITEM_ID,&self->$baseline,state);
    $step_serialize(self->$value,state);
    $WORD tmp = ($WORD)(long)self->$globkey;
    $val_serialize(INT_ID,&tmp,state);          // REMOVE!
}


$Msg $Msg$__deserialize__($Msg res, $Serial$state state) {
    if (!res) {
        if (!state) {
            res = malloc(sizeof (struct $Msg));
            res->$class = &$Msg$methods;
            return res;
        }
        res = $DNEW($Msg,state);
    }
    res->$next = $step_deserialize(state);       // REMOVE!
    res->$to = $step_deserialize(state);
    res->$cont = $step_deserialize(state);
    res->$waiting = NULL;
    res->$baseline = (time_t)$val_deserialize(state);
    res->$value = $step_deserialize(state);
    atomic_flag_clear(&res->$wait_lock);
    $WORD tmp = $val_deserialize(state);        // REMOVE!
    res->$globkey = (int)tmp;
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $Actor$__init__($Actor a) {
    a->$next = NULL;
    a->$msg = NULL;
    a->$outgoing = NULL;
    a->$offspring = NULL;
    a->$waitsfor = NULL;
    a->$catcher = NULL;
    atomic_flag_clear(&a->$msg_lock);
    a->$globkey = get_next_key();
}

$bool $Actor$__bool__($Actor self) {
  return $True;
}

$str $Actor$__str__($Actor self) {
  char *s;
  asprintf(&s,"<$Actor object at %p>",self);
  return to$str(s);
}

void $Actor$__serialize__($Actor self, $Serial$state state) {
    $step_serialize(self->$next,state);          // REMOVE!
    $step_serialize(self->$msg,state);           // REMOVE!
    $step_serialize(self->$waitsfor,state);
    $step_serialize(self->$catcher,state);
    $WORD tmp = ($WORD)(long)self->$globkey;
    $val_serialize(INT_ID,&tmp,state);          // REMOVE!
}

$Actor $Actor$__deserialize__($Actor res, $Serial$state state) {
    if (!res) {
        if (!state) {
            res = malloc(sizeof(struct $Actor));
            res->$class = &$Actor$methods;
            return res;
        }
        res = $DNEW($Actor, state);
    }
    res->$next = $step_deserialize(state);       // REMOVE!
    res->$msg = $step_deserialize(state);        // REMOVE!
    res->$outgoing = NULL;
    res->$offspring = NULL;
    res->$waitsfor = $step_deserialize(state);
    res->$catcher = $step_deserialize(state);
    atomic_flag_clear(&res->$msg_lock);
    $WORD tmp = $val_deserialize(state);        // REMOVE!
    res->$globkey = (int)tmp;
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

void $Catcher$__init__($Catcher c, $Cont cont) {
    c->$next = NULL;
    c->$cont = cont;
}

$bool $Catcher$__bool__($Catcher self) {
  return $True;
}

$str $Catcher$__str__($Catcher self) {
  char *s;
  asprintf(&s,"<$Catcher object at %p>",self);
  return to$str(s);
}

void $Catcher$__serialize__($Catcher self, $Serial$state state) {
    $step_serialize(self->$next,state);
    $step_serialize(self->$cont,state);
}

$Catcher $Catcher$__deserialize__($Catcher self, $Serial$state state) {
    $Catcher res = $DNEW($Catcher,state);
    res->$next = $step_deserialize(state);
    res->$cont = $step_deserialize(state);
    return res;
}
///////////////////////////////////////////////////////////////////////////////////////

void $Cont$__init__($Cont $this) { }

$bool $Cont$__bool__($Cont self) {
  return $True;
}

$str $Cont$__str__($Cont self) {
  char *s;
  asprintf(&s,"<$Cont object at %p>",self);
  return to$str(s);
}

void $Cont$__serialize__($Cont self, $Serial$state state) {
    // TBD
}

$Cont $Cont$__deserialize__($Cont self, $Serial$state state) {
    // TBD
    return NULL;
}

////////////////////////////////////////////////////////////////////////////////////////

void $ConstCont$__init__($ConstCont $this, $WORD val, $Cont cont) {
    $this->val = val;
    $this->cont = cont;
}

$bool $ConstCont$__bool__($ConstCont self) {
  return $True;
}

$str $ConstCont$__str__($ConstCont self) {
  char *s;
  asprintf(&s,"<$ConstCont object at %p>",self);
  return to$str(s);
}

void $ConstCont$__serialize__($ConstCont self, $Serial$state state) {
    $step_serialize(self->val,state);
      $step_serialize(self->cont,state);
}

$ConstCont $ConstCont$__deserialize__($ConstCont self, $Serial$state state) {
    $ConstCont res = $DNEW($ConstCont,state);
    res->val = $step_deserialize(state);
    res->cont = $step_deserialize(state);
    return res;
}

$R $ConstCont$__call__($ConstCont $this, $WORD _ignore) {
    $Cont cont = $this->cont;
    return cont->$class->__call__(cont, $this->val);
}

$Cont $CONSTCONT($WORD val, $Cont cont){
    $ConstCont obj = malloc(sizeof(struct $ConstCont));
    obj->$class = &$ConstCont$methods;
    $ConstCont$methods.__init__(obj, val, cont);
    return ($Cont)obj;
}

////////////////////////////////////////////////////////////////////////////////////////

struct $Msg$class $Msg$methods = {
    MSG_HEADER,
    UNASSIGNED,
    NULL,
    $Msg$__init__,
    $Msg$__serialize__,
    $Msg$__deserialize__,
    $Msg$__bool__,
    $Msg$__str__
};

struct $Actor$class $Actor$methods = {
    ACTOR_HEADER,
    UNASSIGNED,
    NULL,
    $Actor$__init__,
    $Actor$__serialize__,
    $Actor$__deserialize__,
    $Actor$__bool__,
    $Actor$__str__
};

struct $Catcher$class $Catcher$methods = {
    CATCHER_HEADER,
    UNASSIGNED,
    NULL,
    $Catcher$__init__,
    $Catcher$__serialize__,
    $Catcher$__deserialize__,
    $Catcher$__bool__,
    $Catcher$__str__
};

struct $Cont$class $Cont$methods = {
    CONT_HEADER,
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    $Cont$__serialize__,
    $Cont$__deserialize__,
    $Cont$__bool__,
    $Cont$__str__,
    NULL
};

struct $ConstCont$class $ConstCont$methods = {
    "$ConstCont",
    UNASSIGNED,
    NULL,
    $ConstCont$__init__,
    $ConstCont$__serialize__,
    $ConstCont$__deserialize__,
    $ConstCont$__bool__,
    $ConstCont$__str__,
    $ConstCont$__call__
};

////////////////////////////////////////////////////////////////////////////////////////

// Atomically enqueue actor "a" onto the global ready-queue.
void ENQ_ready($Actor a) {
    spinlock_lock(&readyQ_lock);
    if (readyQ) {
        $Actor x = readyQ;
        while (x->$next)
            x = x->$next;
        x->$next = a;
    } else {
        readyQ = a;
    }
    a->$next = NULL;
    spinlock_unlock(&readyQ_lock);
}

// Atomically dequeue and return the first actor from the global ready-queue, 
// or return NULL.
$Actor DEQ_ready() {
    spinlock_lock(&readyQ_lock);
    $Actor res = readyQ;
    if (res) {
        readyQ = res->$next;
        res->$next = NULL;
    }
    spinlock_unlock(&readyQ_lock);
    return res;
}

// Atomically enqueue message "m" onto the queue of actor "a", 
// return true if the queue was previously empty.
bool ENQ_msg($Msg m, $Actor a) {
    bool did_enq = true;
    spinlock_lock(&a->$msg_lock);
    m->$next = NULL;
    if (a->$msg) {
        $Msg x = a->$msg;
        while (x->$next)
            x = x->$next;
        x->$next = m;
        did_enq = false;
    } else {
        a->$msg = m;
    }
    spinlock_unlock(&a->$msg_lock);
    return did_enq;
}

// Atomically dequeue the first message from the queue of actor "a",
// return true if the queue still holds messages.
bool DEQ_msg($Actor a) {
    bool has_more = false;
    spinlock_lock(&a->$msg_lock);
    if (a->$msg) {
        $Msg x = a->$msg;
        a->$msg = x->$next;
        x->$next = NULL;
        has_more = a->$msg != NULL;
    }
    spinlock_unlock(&a->$msg_lock);
    return has_more;
}

// Atomically add actor "a" to the waiting list of messasge "m" if it is not frozen (and return true),
// else immediately return false.
bool ADD_waiting($Actor a, $Msg m) {
    bool did_add = false;
    spinlock_lock(&m->$wait_lock);
    if (m->$cont) {
        a->$next = m->$waiting;
        m->$waiting = a;
        did_add = true;
    }
    spinlock_unlock(&m->$wait_lock);
    return did_add;
}

// Atomically freeze message "m" and return its list of waiting actors. 
$Actor FREEZE_waiting($Msg m) {
    spinlock_lock(&m->$wait_lock);
    m->$cont = NULL;
    spinlock_unlock(&m->$wait_lock);
    $Actor res = m->$waiting;
    m->$waiting = NULL;
    return res;
}

// Atomically enqueue timed message "m" onto the global timer-queue, at position
// given by "m->baseline".
void ENQ_timed($Msg m) {
    time_t m_baseline = m->$baseline;
    spinlock_lock(&timerQ_lock);
    $Msg x = timerQ;
    if (x && x->$baseline <= m_baseline) {
        $Msg next = x->$next;
        while (next && next->$baseline <= m_baseline) {
            x = next;
            next = x->$next;
        }
        x->$next = m;
        m->$next = next;
    } else {
        timerQ = m;
        m->$next = x;
    }
    spinlock_unlock(&timerQ_lock);
}

// Atomically dequeue and return the first message from the global timer-queue if 
// its baseline is less or equal to "now", else return NULL.
$Msg DEQ_timed(time_t now) {
    spinlock_lock(&timerQ_lock);
    $Msg res = timerQ;
    if (res) {
        if (res->$baseline <= now) {
            timerQ = res->$next;
            res->$next = NULL;
        } else {
            res = NULL;
        }
    }
    spinlock_unlock(&timerQ_lock);
    return res;
}

// Place a message in the outgoing buffer of the sender. Not protected (never
// exposed to data races).
void PUSH_outgoing($Actor self, $Msg m) {
    m->$next = self->$outgoing;
    self->$outgoing = m;
}

// Actually send all buffered messages of the sender. Not protected (never
// exposed to data races).
void FLUSH_outgoing($Actor self) {
    $Msg prev = NULL;
    $Msg m = self->$outgoing;
    self->$outgoing = NULL;
    while (m) {
        $Msg next = m->$next;
        m->$next = prev;
        prev = m;
        m = next;
    }
    m = prev;
    while (m) {
        $Msg next = m->$next;
        m->$next = NULL;
        if (m->$baseline == self->$msg->$baseline) {
            $Actor to = m->$to;
            if (ENQ_msg(m, to)) {
                ENQ_ready(to);
            }
        } else {
            ENQ_timed(m);
        }
        m = next;
    }
}

// Just clear the list of created actors and the links between them.
// Not protected (never exposed to data races).
void CLEAR_offspring($Actor current) {
    $Actor a = current->$offspring;
    while (a) {
        //printf("## Actor %p created offpring %p of class %s\n", current, a, a->$class->$GCINFO);
        $Actor b = a;
        a = a->$next;
        b->$next = NULL;
    }
    current->$offspring = NULL;
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
$R $DONE$__call__($Cont $this, $WORD val) {
    return $R_DONE(val);
}

$bool $Done$__bool__($Cont self) {
  return $True;
}

$str $Done$__str__($Cont self) {
  char *s;
  asprintf(&s,"<$Done object at %p>",self);
  return to$str(s);
}

void $Done__serialize__($Cont self, $Serial$state state) {
  return;
}

$Cont $Done__deserialize__($Cont self, $Serial$state state) {
  $Cont res = $DNEW($Cont,state);
  res->$class = &$Done$methods;
  return res;
}

struct $Cont$class $Done$methods = {
    CONT_HEADER,
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    $Done__serialize__,
    $Done__deserialize__,
    $Done$__bool__,
    $Done$__str__,
    ($R (*)($Cont, ...))$DONE$__call__
};
struct $Cont $Done$instance = {
    &$Done$methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $NewRoot$__call__ ($Cont $this, $WORD val) {
    $Cont then = ($Cont)val;
    $Env env = $NEW($Env,args,NULL);
    return $ROOT(env, then);
}

struct $Cont$class $NewRoot$methods = {
    CONT_HEADER,
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    NULL,
    NULL,
    NULL,
    NULL,
    ($R (*)($Cont, ...))$NewRoot$__call__
};
struct $Cont $NewRoot$instance = {
    &$NewRoot$methods
};
////////////////////////////////////////////////////////////////////////////////////////
$R $WriteRoot$__call__($Cont $this, $WORD val) {
    root_actor = ($Actor)val;
    return $R_DONE(val);
}

struct $Cont$class $WriteRoot$methods = {
    CONT_HEADER,
    UNASSIGNED,
    NULL,
    $Cont$__init__,
    NULL,
    NULL,
    NULL,
    NULL,
    ($R (*)($Cont, ...))$WriteRoot$__call__
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
    c->$next = a->$catcher;
    a->$catcher = c;
}

$Catcher POP_catcher($Actor a) {
    $Catcher c = a->$catcher;
    a->$catcher = c->$next;
    c->$next = NULL;
    return c;
}

$Msg $ASYNC($Actor to, $Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    time_t baseline = 0;
    $Msg m = $NEW($Msg, to, cont, baseline, &$Done$instance);
    if (self) {
        m->$baseline = self->$msg->$baseline;
        PUSH_outgoing(self, m);
    } else {
        struct timespec now;
        clock_gettime(CLOCK_MONOTONIC, &now);
        m->$baseline = now.tv_sec;
        if (ENQ_msg(m, to)) {
           ENQ_ready(to);
        }
    }
    pthread_mutex_lock(&sleep_lock);
    pthread_cond_signal(&work_to_do);
    pthread_mutex_unlock(&sleep_lock);
    return m;
}

$Msg $AFTER($int sec, $Cont cont) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    time_t baseline = self->$msg->$baseline + sec->val;
    $Msg m = $NEW($Msg, self, cont, baseline, &$Done$instance);
    PUSH_outgoing(self, m);
//    ENQ_timed(m);
    return m;
}

$R $AWAIT($Msg m, $Cont cont) {
    return $R_WAIT(cont, m);
}

void $NEWACT($Actor a) {
    $Actor self = ($Actor)pthread_getspecific(self_key);
    a->$next = self->$offspring;
    self->$offspring = a;
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
            $Msg m = current->$msg;
            $Cont cont = m->$cont;
            $WORD val = m->$value;
            
            $R r = cont->$class->__call__(cont, val);
            switch (r.tag) {
                case $RDONE: {
                    FLUSH_outgoing(current);
                    CLEAR_offspring(current);
                    m->$value = r.value;
                    $Actor b = FREEZE_waiting(m);        // Sets m->cont = NULL now that m->value holds the response
                    while (b) {
                        b->$msg->$value = r.value;
                        b->$waitsfor = NULL;
                        ENQ_ready(b);
                        b = b->$next;
                    }
                    if (DEQ_msg(current)) {
                        ENQ_ready(current);
                    }
                    break;
                }
                case $RCONT: {
                    m->$cont = r.cont;
                    m->$value = r.value;
                    ENQ_ready(current);
                    break;
                }
                case $RFAIL: {
                    $Catcher c = POP_catcher(current);
                    m->$cont = c->$cont;
                    m->$value = r.value;
                    ENQ_ready(current);
                    break;
                }
                case $RWAIT: {
                    FLUSH_outgoing(current);
                    CLEAR_offspring(current);
                    m->$cont = r.cont;
                    $Msg x = ($Msg)r.value;
                    if (ADD_waiting(current, x)) {      // x->cont != NULL: x is still being processed
                        current->$waitsfor = x;
                    } else {                            // x->cont == NULL: x->value holds the final response
                        m->$value = x->$value;
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
                if (ENQ_msg(m, m->$to)) {
                    ENQ_ready(m->$to);
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
                    i = 0;
                  }
                }
#endif
                if  ((int)arg==0) {
                    $eventloop();
                } else {
                  pthread_mutex_lock(&sleep_lock);
                  pthread_cond_wait(&work_to_do, &sleep_lock);
                  pthread_mutex_unlock(&sleep_lock);
                  // static struct timespec idle_wait = { 0, 50000000 };  // 500ms
                  // nanosleep(&idle_wait, NULL);
                }
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////

// we assume that (de)serialization takes place without need for spinlock protection.

$dict glob_dict;

$WORD try_glob_attr($WORD obj) {
    $Serializable$class c = (($Serializable)obj)->$class;
    if (c->$class_id == MSG_ID) {
        int64_t key = (($Msg)obj)->$globkey;
        //printf("## try_glob_attr Msg %p = %d\n", obj, key);
        $dict_setitem(glob_dict, ($Hashable)$Hashable$int$witness, to$int(key), obj);
        return 0;
        //return ($WORD)(long)key;
    } else if (c->$class_id == ACTOR_ID || c->$superclass && c->$superclass->$class_id == ACTOR_ID) {
        int64_t key = (($Actor)obj)->$globkey;
        //printf("## try_glob_attr Actor %p = %d\n", obj, key);
        $dict_setitem(glob_dict, ($Hashable)$Hashable$int$witness, to$int(key), obj);
        return 0;
        //return ($WORD)(long)key;
    }
    return 0;
}

$ROW $serialize_rts() {
  glob_dict = $NEW($dict,($Hashable)$Hashable$int$witness,NULL,NULL);
  return $serialize(($Serializable)$NEW($tuple,3,root_actor,readyQ,timerQ), try_glob_attr);
}

$WORD try_glob_dict($WORD w) {
    int key = (int)w;
    $WORD obj = $dict_get(glob_dict, ($Hashable)$Hashable$int$witness, to$int(key), try_glob_dict);
    return obj;
}

void $deserialize_rts($ROW row) {
  $tuple t = ($tuple)$deserialize(row, NULL);
  root_actor = t->components[0];
  readyQ = t->components[1];
  timerQ = t->components[2];
}

////////////////////////////////////////////////////////////////////////////////////////

void $register_rts () {
  $register_force(MSG_ID,&$Msg$methods);
  $register_force(ACTOR_ID,&$Actor$methods);
  $register_force(CATCHER_ID,&$Catcher$methods);
  $register_force(CLOS_ID,&$function$methods);
  $register_force(CONT_ID,&$Cont$methods);
  $register_force(DONE_ID,&$Done$methods);
  $register_force(CONSTCONT_ID,&$ConstCont$methods);
}
 
////////////////////////////////////////////////////////////////////////////////////////

int main(int argc, char **argv) {
#ifdef EXPERIMENT
    long num_cores = 1;    
#else
    long num_cores = sysconf(_SC_NPROCESSORS_ONLN);
#endif
    //    printf("%ld worker threads\n", num_cores);
    kq = kqueue();
    $register_builtin();
    $register_rts();

//    int primary_key_idx = 0;
//    int clustering_key_idxs[2] = {1, 2};
//    int index_key_idx=3;
//    db_schema_t* db_schema = db_create_schema(NULL, 3, &primary_key_idx, 1, clustering_key_idxs, 1, &index_key_idx, 1);
//    printf("db_create_schema returns %p\n", db_schema);


    args = $list$new(NULL,NULL);
    for (int i=0; i< argc; i++)
      $list_append(args,to$str(argv[i]));
    
    BOOTSTRAP();
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
    
    for(int idx = 0; idx < num_cores; ++idx) {
        pthread_join(threads[idx], NULL);
    }
    return 0;
}
