#pragma once
#include "builtin/builtin.h"
#include "builtin/env.h"
#include "rts/rts.h"
struct foo$$l$2lambda;
struct foo$$l$3lambda;
struct foo$$Bar;
typedef struct foo$$l$2lambda *foo$$l$2lambda;
typedef struct foo$$l$3lambda *foo$$l$3lambda;
typedef struct foo$$Bar *foo$$Bar;
$R foo$$l$1c$1cont (foo$$Bar, $Cont, $NoneType);
struct foo$$l$2lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (foo$$l$2lambda, foo$$Bar);
    void (*__serialize__) (foo$$l$2lambda, $Serial$state);
    foo$$l$2lambda (*__deserialize__) (foo$$l$2lambda, $Serial$state);
    $bool (*__bool__) (foo$$l$2lambda);
    $str (*__str__) (foo$$l$2lambda);
    $R (*__call__) (foo$$l$2lambda, $Cont);
};
struct foo$$l$2lambda {
    struct foo$$l$2lambda$class *$class;
    foo$$Bar d$tmp;
};
struct foo$$l$3lambda$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (foo$$l$3lambda, foo$$Bar, $Cont);
    void (*__serialize__) (foo$$l$3lambda, $Serial$state);
    foo$$l$3lambda (*__deserialize__) (foo$$l$3lambda, $Serial$state);
    $bool (*__bool__) (foo$$l$3lambda);
    $str (*__str__) (foo$$l$3lambda);
    $R (*__call__) (foo$$l$3lambda, $NoneType);
};
struct foo$$l$3lambda {
    struct foo$$l$3lambda$class *$class;
    foo$$Bar d$tmp;
    $Cont c$cont;
};
struct foo$$Bar$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $R (*__init__) (foo$$Bar, $Cont);
    void (*__serialize__) (foo$$Bar, $Serial$state);
    foo$$Bar (*__deserialize__) (foo$$Bar, $Serial$state);
    $bool (*__bool__) (foo$$Bar);
    $str (*__str__) (foo$$Bar);
    $NoneType (*__resume__) (foo$$Bar);
};
struct foo$$Bar {
    struct foo$$Bar$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Msg $waitsfor;
    $int64 $consume_hd;
    $Catcher $catcher;
    $Lock $msg_lock;
    $long $globkey;
};
$R foo$$Bar$newact ($Cont);
extern struct foo$$l$2lambda$class foo$$l$2lambda$methods;
foo$$l$2lambda foo$$l$2lambda$new(foo$$Bar);
extern struct foo$$l$3lambda$class foo$$l$3lambda$methods;
foo$$l$3lambda foo$$l$3lambda$new(foo$$Bar, $Cont);
extern struct foo$$Bar$class foo$$Bar$methods;
$R foo$$Bar$new($Cont);
void foo$$__init__ ();