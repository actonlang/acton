#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>


typedef void *$WORD;

struct $R;
struct $Clos;
struct $Cont;
struct $Msg;
struct $ACTOR;
struct $Catcher;

typedef struct $R $R;
typedef struct $Clos *$Clos;
typedef struct $Cont *$Cont;
typedef struct $Msg *$Msg;
typedef struct $ACTOR *$ACTOR;
typedef struct $Catcher *$Catcher;

struct $Msg$class;
struct $ACTOR$class;
struct $Catcher$class;

extern struct $Msg$class $Msg$methods;
extern struct $ACTOR$class $ACTOR$methods;
extern struct $Catcher$class $Catcher$methods;


enum $RTAG { $RDONE, $RFAIL, $RCONT, $RWAIT };
typedef enum $RTAG $RTAG;

struct $R {
    $RTAG tag;
    $Cont cont;
    $WORD value;
};

#define $None ($WORD)0

#define CONT_HEADER     "Cont"
#define MSG_HEADER      "Msg"
#define ACTOR_HEADER    "Actor"
#define CATCHER_HEADER  "Catcher"

struct $Cont {
    char *header;
    $R (*code)();
    int nvar;
    $WORD var[];
};

struct $Msg {
    struct $Msg$class *__class__;
    $Msg next;
    $ACTOR to;
    $Cont cont;
    $ACTOR waiting;
    time_t baseline;
    volatile atomic_flag wait_lock;
    $WORD value;
};
struct $Msg$class {
    char *header;
    void (*__init__)($Msg, $ACTOR, $Cont, time_t, $WORD);
};

struct $ACTOR {
    struct $ACTOR$class *__class__;
    $ACTOR next;
    $Msg msg;
    $Catcher catcher;
    volatile atomic_flag msg_lock;
};
struct $ACTOR$class {
    char *GCINFO;
    void (*__init__)($ACTOR);
    $WORD (*$enter)($ACTOR, $WORD);
};

struct $Catcher {
    struct $Catcher$class *__class__;
    $Catcher next;
    $Cont cont;
};
struct $Catcher$class {
    char *header;
    void (*__init__)($Catcher, $Cont);
};

#define $CONTINUATION(code, nvar, ...)  $continuation(code, nvar, __VA_ARGS__)
#define $CONTINUE(cont, arg)            ($R){$RCONT, (cont), ($WORD)(arg)}
#define $CONTINUE_(cont, arg)           ((cont)->__class__->$enter((cont),(WORD)arg))


$Cont $continuation($R (*code)(), int nvar, ...);


$Msg $ASYNC($ACTOR to, $Cont c);
$Msg $AFTER(time_t sec, $Cont c);
$R $AWAIT($Msg m, $Cont th);

void $PUSH($Cont Cont);
void $POP();

int $RTS_RUN(int argc, char **argv, $R (*root)());



#define $NEW($T, ...) ({ $T $tmp = malloc(sizeof(struct $T)); $tmp->__class__ = &$T ## $methods; $tmp->__class__->__init__($tmp,__VA_ARGS__); $tmp; })
#define $NEW0($T) ({ $T $tmp = malloc(sizeof(struct $T)); $tmp->__class__ = &$T ## $methods; $tmp->__class__->__init__($tmp); $tmp; })

#define $NEW2($T, $c, ...) ({ $T $tmp = malloc(sizeof(struct $T)); $tmp->__class__ = &$T ## $methods; $tmp->__class__->__init__($tmp,__VA_ARGS__,$c); })

#define $to_time(i) (i)

//////////////// $CLOS

struct $CLOS;
struct $CLOS$class;
typedef struct $CLOS *$CLOS;
struct $CLOS {
    struct $CLOS$class *__class__;
};
struct $CLOS$class {
    char *GCINFO;
    void (*__init__)($CLOS);
    $WORD (*$enter)($CLOS, $WORD);
};

//////////////// $CONT

struct $CONT;
struct $CONT$class;
typedef struct $CONT *$CONT;
struct $CONT {
    union {
        struct $CONT$class *__class__;
        struct $CLOS super;
    };
};
struct $CONT$class {
    char *GCINFO;
    void (*__init__)($CONT);
    $R (*$enter)($CONT, $WORD);
};
