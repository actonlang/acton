#pragma once

#include "rts.h"

struct lambda$1;
struct lambda$2;
struct lambda$3;
struct Pingpong;

typedef struct lambda$1 *lambda$1;
typedef struct lambda$2 *lambda$2;
typedef struct lambda$3 *lambda$3;
typedef struct Pingpong *Pingpong;

struct lambda$1G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(lambda$1, Pingpong, B_int, B_int);
    void (*__serialize__)(lambda$1, $NoneType);
    lambda$1 (*__deserialize__)(lambda$1, $NoneType);
    B_bool (*__bool__)(lambda$1);
    B_str (*__str__)(lambda$1);
    $R (*__call__)(lambda$1, $Cont);
};
struct lambda$1 {
    struct lambda$1G_class *$class;
    Pingpong self;
    B_int count;
    B_int q;
};
lambda$1 lambda$1G_new(Pingpong, B_int, B_int);

struct lambda$2G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(lambda$2, Pingpong, B_int);
    void (*__serialize__)(lambda$2, $NoneType);
    lambda$2 (*__deserialize__)(lambda$2, $NoneType);
    B_bool (*__bool__)(lambda$2);
    B_str (*__str__)(lambda$2);
    $R (*__call__)(lambda$2, $Cont);
};
struct lambda$2 {
    struct lambda$2G_class *$class;
    Pingpong self;
    B_int q;
};
lambda$2 lambda$2G_new(Pingpong, B_int);

struct lambda$3G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(lambda$3, $Cont);
    void (*__serialize__)(lambda$3, $NoneType);
    lambda$3 (*__deserialize__)(lambda$3, $NoneType);
    B_bool (*__bool__)(lambda$3);
    B_str (*__str__)(lambda$3);
    $R (*__call__)(lambda$3, $NoneType);
};
struct lambda$3 {
    struct lambda$3G_class *$class;
    $Cont cont;
};
lambda$3 lambda$3G_new($Cont);

struct PingpongG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $R (*__init__)(Pingpong, B_int, $Cont);
    void (*__serialize__)(Pingpong, $NoneType);
    Pingpong (*__deserialize__)(Pingpong, $NoneType);
    B_bool (*__bool__)(Pingpong);
    B_str (*__str__)(Pingpong);
    $R (*ping)(Pingpong, B_int, $Cont);
    $R (*pong)(Pingpong, B_int, B_int, $Cont);
};
struct Pingpong {
    struct PingpongG_class *$class;
    $Actor $next;
    B_Msg B_Msg;
    B_Msg $outgoing;
    $Actor $offspring;
    $Actor $uterus;
    B_Msg $waitsfor;
    $Catcher $catcher;
    $Lock B_Msg_lock;
    $long $globkey;
    B_int i;
    B_int count;
};
$R PingpongG_new(B_int, $Cont);

extern struct lambda$1G_class lambda$1G_methods;
extern struct lambda$2G_class lambda$2G_methods;
extern struct lambda$3G_class lambda$3G_methods;
extern struct PingpongG_class PingpongG_methods;
