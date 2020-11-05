#ifndef test
#define test

#include "../../../builtin/builtin.h"
#include "rts.h"
 
struct test$$l$1lambda;
struct test$$Env;
typedef struct test$$l$1lambda *test$$l$1lambda;
typedef struct test$$Env *test$$Env;
struct test$$l$1lambda$class {
    char *$GCINFO;
    $Super$class $superclass;
    void* (*__init__) (test$$l$1lambda, test$$Env, $str);
    void* (*__serialize__) (test$$l$1lambda, $Serial$state);
    test$$l$1lambda (*__deserialize__) ($Serial$state);
    $R (*__enter__) (test$$l$1lambda,$Clos);
};
struct test$$l$1lambda {
    struct test$$l$1lambda$class *$class;
    test$$Env __self__;
    $str s;
};
struct test$$Env$class {
    char *$GCINFO;
    $Super$class $superclass;
    $R (*__init__) (test$$Env, $Cont);  //snd param changed from $Clos!!
    void* (*__serialize__) (test$$Env, $Serial$state);
    test$$Env (*__deserialize__) ($Serial$state);
    $R (*write$local) (test$$Env, $str,$Clos);
    $R (*write) (test$$Env, $str, $Clos);
};
struct test$$Env {
    struct test$$Env$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Catcher $catcher;
    volatile atomic_flag wait_lock;
  //    $Lock $lock;
};
extern struct test$$l$1lambda$class test$$l$1lambda$methods;
extern struct test$$Env$class test$$Env$methods;
struct test$$Root;
typedef struct test$$Root *test$$Root;
struct test$$Root$class {
    char *$GCINFO;
    $Super$class $superclass;
    $R (*__init__) (test$$Root, test$$Env, $Cont);//last param changed from $Clos!!
    void* (*__serialize__) (test$$Root, $Serial$state);
    test$$Root (*__deserialize__) ($Serial$state);
};
struct test$$Root {
    struct test$$Root$class *$class;
    $Actor $next;
    $Msg $msg;
    $Msg $outgoing;
    $Catcher $catcher;
    volatile atomic_flag wait_lock;
  //    $Lock $lock;
    test$$Env env;
};
extern struct test$$Root$class test$$Root$methods;

void test$$__init__();
#endif
