#pragma once

#include "builtin/builtin.h"
#include "builtin/env.h"
#include "rts/rts.h"

struct lambda$1;
struct lambda$2;
struct Pingpong;

typedef struct lambda$1 *lambda$1;
typedef struct lambda$2 *lambda$2;
typedef struct Pingpong *Pingpong;

struct lambda$1G_class {
    GC_descr $GCdescr;
    char *$name;
    $SuperG_class $superclass;
    void (*__init__)(lambda$1, Pingpong, B_int);
    void (*__serialize__)(lambda$1, B_NoneType);
    lambda$1 (*__deserialize__)(lambda$1, B_NoneType);
    $R (*__call__)(lambda$1, $Cont);
};
struct lambda$1 {
    union {
        struct lambda$1G_class *$class;
        struct $Cont super;
    };
    Pingpong self;
    B_int count;
};
extern GC_word lambda$1D_gcbm[GC_BITMAP_SIZE(struct lambda$1)];

struct lambda$2G_class {
    GC_descr $GCdescr;
    char *$name;
    $SuperG_class $superclass;
    void (*__init__)(lambda$2, Pingpong);
    void (*__serialize__)(lambda$2, B_NoneType);
    lambda$2 (*__deserialize__)(lambda$2, B_NoneType);
    $R (*__call__)(lambda$2, $Cont);
};
struct lambda$2 {
    union {
        struct lambda$2G_class *$class;
        struct $Cont super;
    };
    Pingpong self;
};
extern GC_word lambda$2D_gcbm[GC_BITMAP_SIZE(struct lambda$2)];

struct PingpongG_class {
    GC_descr $GCdescr;
    char *$name;
    $SuperG_class $superclass;
    $R (*__init__)(Pingpong, B_Env, $Cont);
    void (*__serialize__)(Pingpong, B_NoneType);
    Pingpong (*__deserialize__)(Pingpong, B_NoneType);
    $R (*ping)(Pingpong, $Cont);
    $R (*pong)(Pingpong, B_int, $Cont);
}; 
struct Pingpong {
    union {
        struct PingpongG_class *$class;
        struct $Actor super;
    };
    B_int i;
    B_int count;
};
extern GC_word PingpongD_gcbm[GC_BITMAP_SIZE(struct Pingpong)];

extern struct lambda$1G_class lambda$1G_methods;
extern struct lambda$2G_class lambda$2G_methods;
extern struct PingpongG_class PingpongG_methods;
