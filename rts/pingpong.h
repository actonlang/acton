#pragma once

#include "rts.h"

struct lambda$1;
struct lambda$2;
struct Pingpong;

struct lambda$1$class;
struct lambda$2$class;
struct Pingpong$class;

typedef struct lambda$1 *lambda$1;
typedef struct lambda$2 *lambda$2;
typedef struct Pingpong *Pingpong;

void lambda$1$__init__(lambda$1, Pingpong, $int, $int);
$R lambda$1$enter(lambda$1, $Cont);

void lambda$2$__init__(lambda$2, Pingpong, $int);
$R lambda$2$enter(lambda$2, $Cont);

$R Pingpong$__init__(Pingpong, $int, $Cont);
$R Pingpong$ping(Pingpong, $int, $Cont);
$R Pingpong$pong(Pingpong, $int, $int, $Cont);

struct lambda$1 {
    union {
        struct lambda$1$class *$class;
        struct $Cont super;
    };
    Pingpong self;
    $int count;
    $int q;
};
struct lambda$1$class {
    char *GCINFO;
    void (*__init__)(lambda$1, Pingpong, $int, $int);
    $R (*enter)(lambda$1, $Cont);
};

struct lambda$2 {
    union {
        struct lambda$2$class *$class;
        struct $Cont super;
    };
    Pingpong self;
    $int q;
};
struct lambda$2$class {
    char *GCINFO;
    void (*__init__)(lambda$2, Pingpong, $int);
    $R (*enter)(lambda$2, $Cont);
};

struct Pingpong {
    union {
        struct Pingpong$class *$class;
        struct $Actor super;
    };
    $int i;
    $int count;
};
struct Pingpong$class {
    char *GCINFO;
    $R (*__init__)(Pingpong, $int, $Cont);
    $R (*ping)(Pingpong, $int, $Cont);
    $R (*pong)(Pingpong, $int, $int, $Cont);
};

extern struct lambda$1$class lambda$1$methods;
extern struct lambda$2$class lambda$2$methods;
extern struct Pingpong$class Pingpong$methods;