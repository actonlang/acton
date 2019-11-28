#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>


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
