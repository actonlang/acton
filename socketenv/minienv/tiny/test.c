#include "test.h"

#define $SKIPRES(x) x

void* test$$l$1lambda$__init__ (test$$l$1lambda l$self, test$$Env __self__, $str s) {
    l$self->__self__ = __self__;
    l$self->s = s;
    return $None;
}
$R test$$l$1lambda$__enter__ (test$$l$1lambda l$self,$Clos c$cont) {
    test$$Env __self__ = l$self->__self__;
    $str s = l$self->s;
    return __self__->$class->write$local(__self__,s,c$cont);
}
struct test$$l$1lambda$class test$$l$1lambda$methods;
$R test$$Env$__init__ (test$$Env __self__, $Cont c$cont) {
  return $R_CONT(($Cont)c$cont, $None); 
}
$R test$$Env$write$local (test$$Env __self__, $str s, $Clos c$cont) {
    printf("%s\n",s->str);
    return $R_CONT(($Cont)c$cont, $None);
}
$R test$$Env$write (test$$Env __self__, $str s, $Clos c$cont) {
  return $R_CONT(($Cont)c$cont, $ASYNC(($Actor)__self__, ($Cont)$NEW(test$$l$1lambda,__self__, s)));
}
struct test$$Env$class test$$Env$methods;
$R test$$Root$__init__ (test$$Root __self__, test$$Env env, $Cont c$cont) {
    __self__->env = env;
    return __self__->env->$class->write(__self__->env,to$str("Hi!"), ($Clos)$SKIPRES(c$cont));
}
struct test$$Root$class test$$Root$methods;
int test$$done$ = 0;
void test$$__init__ () {
    if (test$$done$) return;
    test$$done$ = 1;
    {
        test$$l$1lambda$methods.$GCINFO = "test$$l$1lambda";
        test$$l$1lambda$methods.$superclass = ($Super$class)&$Clos$methods;
        test$$l$1lambda$methods.__init__ = test$$l$1lambda$__init__;
        test$$l$1lambda$methods.__enter__ = test$$l$1lambda$__enter__;
    }
    {
        test$$Env$methods.$GCINFO = "test$$Env";
        test$$Env$methods.$superclass = ($Super$class)&$Actor$methods;
        test$$Env$methods.__init__ = test$$Env$__init__;
        test$$Env$methods.write$local = test$$Env$write$local;
        test$$Env$methods.write = test$$Env$write;
    }
    {
        test$$Root$methods.$GCINFO = "test$$Root";
        test$$Root$methods.$superclass = ($Super$class)&$Actor$methods;
        test$$Root$methods.__init__ = test$$Root$__init__;
    }
}

$R $ROOT(test$$Env env, $Cont then) {
  $register(&test$$l$1lambda$methods);
  $register(&test$$Env$methods);
  $register(&test$$Root$methods);
  return $NEWCC(test$$Root,then,env);
}
