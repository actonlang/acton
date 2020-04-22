#pragma once

#include "rts.h"

struct lambda$1;
struct lambda$2;
struct Pingpong;

typedef struct lambda$1 *lambda$1;
typedef struct lambda$2 *lambda$2;
typedef struct Pingpong *Pingpong;

struct lambda$1$class {
    char *$GCINFO;
    void (*__serialize__)(lambda$1, $Mapping$dict, $WORD, int, $dict, $ROWLISTHEADER);
    lambda$1 (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    void (*__init__)(lambda$1, Pingpong, $int, $int);
    $R (*enter)(lambda$1, $Cont);
};
struct lambda$1 {
    union {
        struct lambda$1$class *$class;
        struct $Cont super;
    };
    Pingpong self;
    $int count;
    $int q;
};

struct lambda$2$class {
    char *$GCINFO;
    void (*__serialize__)(lambda$2, $Mapping$dict, $WORD, int, $dict, $ROWLISTHEADER);
    lambda$2 (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    void (*__init__)(lambda$2, Pingpong, $int);
    $R (*enter)(lambda$2, $Cont);
};
struct lambda$2 {
    union {
        struct lambda$2$class *$class;
        struct $Cont super;
    };
    Pingpong self;
    $int q;
};

struct Pingpong$class {
    char *$GCINFO;
    void (*__serialize__)(Pingpong, $Mapping$dict, $WORD, int, $dict, $ROWLISTHEADER);
    Pingpong (*__deserialize__)($Mapping$dict, $ROW*, $dict);
    $R (*__init__)(Pingpong, $int, $Cont);
    $R (*ping)(Pingpong, $int, $Cont);
    $R (*pong)(Pingpong, $int, $int, $Cont);
};
struct Pingpong {
    union {
        struct Pingpong$class *$class;
        struct $Actor super;
    };
    $int i;
    $int count;
};

extern struct lambda$1$class lambda$1$methods;
extern struct lambda$2$class lambda$2$methods;
extern struct Pingpong$class Pingpong$methods;