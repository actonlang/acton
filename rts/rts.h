#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>


typedef void *WORD;

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

enum RTAG { RDONE, RFAIL, RCONT, RWAIT };
typedef enum RTAG RTAG;

struct $R {
    RTAG  tag;
    $Cont cont;
    WORD  value;
};

#define None (WORD)0

#define _DONE(value)       ($R){RDONE, NULL,   (value)}
#define _FAIL(value)       ($R){RFAIL, NULL,   (value)}
#define _CONT(cont, value) ($R){RCONT, (cont), (value)}
#define _WAIT(cont, value) ($R){RWAIT, (cont), (value)}

typedef WORD (*$Fun0)();
typedef WORD (*$Fun1)(WORD);
typedef WORD (*$Fun2)(WORD,WORD);
typedef WORD (*$Fun3)(WORD,WORD,WORD);
typedef WORD (*$Fun4)(WORD,WORD,WORD,WORD);

typedef $R (*$Cont0)();
typedef $R (*$Cont1)(WORD);
typedef $R (*$Cont2)(WORD,WORD);
typedef $R (*$Cont3)(WORD,WORD,WORD);
typedef $R (*$Cont4)(WORD,WORD,WORD,WORD);

#define CLOS_HEADER     "Clos"
#define Cont_HEADER     "Cont"
#define MSG_HEADER      "Msg"
#define ACTOR_HEADER    "Actor"
#define CATCHER_HEADER  "Catcher"

struct $Clos {
    char *header;
    $Fun1 code;
    int nvar;
    WORD var[];
};

struct $Cont {
    char *header;
    $Cont1 code;
    int nvar;
    WORD var[];
};

struct $Msg {
    char *header;
    $Msg next;
    $Actor to;
    $Cont cont;
    $Actor waiting;
    time_t baseline;
    volatile atomic_flag wait_lock;
    WORD value;
};

struct $Actor {
    char *header;
    $Actor next;
    $Msg msg;
    $Catcher catcher;
    volatile atomic_flag msg_lock;
    int nstate;
    WORD state[];
};

struct $Catcher {
    char *header;
    $Catcher next;
    $Cont cont;
};

$Msg MSG($Actor to, $Cont cont, time_t baseline, WORD value);
$Actor ACTOR(int n);
$Catcher CATCHER($Cont cont);

#define $CLOSE(code, nvar, ...)         $close(($Fun1)code, nvar, __VA_ARGS__)
#define $ENTER(clos, arg)               $enter(clos, (WORD)arg)

$Clos $close($Fun1 code, int nvar, ...);
WORD $enter($Clos, WORD);

#define $CONTINUATION(code, nvar, ...)  $continuation(($Cont1)code, nvar, __VA_ARGS__)
#define $CONTINUE(cont, arg)            $continue(cont, (WORD)arg)

$Cont $continuation($Cont1 code, int nvar, ...);
$R $continue($Cont, WORD);

$Msg ASYNC($Actor to, $Cont c);

$Msg POSTPONE($Actor to, time_t sec, $Cont c);

$R AWAIT($Msg m, $Cont th);

void PUSH($Cont Cont);

void POP();
