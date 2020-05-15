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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(lambda$1, Pingpong, $int, $int);
    void (*__serialize__)(lambda$1, $Serial$state);
    lambda$1 (*__deserialize__)($Serial$state);
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
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(lambda$2, Pingpong, $int);
    void (*__serialize__)(lambda$2, $Serial$state);
    lambda$2 (*__deserialize__)($Serial$state);
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
    int $class_id;
    $Super$class $superclass;
    $R (*__init__)(Pingpong, $int, $Cont);
    void (*__serialize__)(Pingpong, $Serial$state);
    Pingpong (*__deserialize__)($Serial$state);
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
