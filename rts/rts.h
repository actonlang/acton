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
struct $Actor;
struct $Catcher;

typedef struct $R $R;
typedef struct $Clos *$Clos;
typedef struct $Cont *$Cont;
typedef struct $Msg *$Msg;
typedef struct $Actor *$Actor;
typedef struct $Catcher *$Catcher;

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
    char *header;
    $Msg next;
    $Actor to;
    $Cont cont;
    $Actor waiting;
    time_t baseline;
    volatile atomic_flag wait_lock;
    $WORD value;
};

struct $Actor {
    char *header;
    $Actor next;
    $Msg msg;
    $Catcher catcher;
    volatile atomic_flag msg_lock;
    int nstate;
    $WORD state[];
};

struct $Catcher {
    char *header;
    $Catcher next;
    $Cont cont;
};

#define $CONTINUATION(code, nvar, ...)  $continuation(code, nvar, __VA_ARGS__)
#define $CONTINUE(cont, arg)            $continue(cont, ($WORD)arg)
#define $CONTINUE_(cont, arg)           ($R){$RCONT, (cont), ($WORD)(arg)}

$Cont $continuation($R (*code)(), int nvar, ...);
$R $continue($Cont cont, $WORD arg);

$Msg $MSG($Actor to, $Cont cont, time_t baseline, $WORD value);
$Actor $ACTOR(int n);
$Catcher $CATCHER($Cont cont);

$Msg $ASYNC($Actor to, $Cont c);
$Msg $AFTER(time_t sec, $Cont c);
$R $AWAIT($Msg m, $Cont th);

void $PUSH($Cont Cont);
void $POP();

////////////////////////////////////////////////////////////////////////////////////////////////

typedef int $int;

#define $int_add(a,b)       ((int)a + (int)b)
#define $int_mul(a,b)       ((int)a * (int)b)
#define $int_neg(a)         (-(int)a)
