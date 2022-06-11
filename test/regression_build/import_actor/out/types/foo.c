#include "types/foo.h"
$R foo$$l$1c$1cont (foo$$Bar d$tmp, $Cont c$cont, $NoneType c$2res) {
    return $R_CONT(c$cont, d$tmp);
}
$NoneType foo$$l$2lambda$__init__ (foo$$l$2lambda p$self, foo$$Bar d$tmp) {
    p$self->d$tmp = d$tmp;
    return $None;
}
$R foo$$l$2lambda$__call__ (foo$$l$2lambda p$self, $Cont c$cont) {
    foo$$Bar d$tmp = p$self->d$tmp;
    return d$tmp->$class->__init__(d$tmp, c$cont);
}
void foo$$l$2lambda$__serialize__ (foo$$l$2lambda self, $Serial$state state) {
    $step_serialize(self->d$tmp, state);
}
foo$$l$2lambda foo$$l$2lambda$__deserialize__ (foo$$l$2lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct foo$$l$2lambda));
            self->$class = &foo$$l$2lambda$methods;
            return self;
        }
        self = $DNEW(foo$$l$2lambda, state);
    }
    self->d$tmp = $step_deserialize(state);
    return self;
}
foo$$l$2lambda foo$$l$2lambda$new(foo$$Bar p$1) {
    foo$$l$2lambda $tmp = malloc(sizeof(struct foo$$l$2lambda));
    $tmp->$class = &foo$$l$2lambda$methods;
    foo$$l$2lambda$methods.__init__($tmp, p$1);
    return $tmp;
}
struct foo$$l$2lambda$class foo$$l$2lambda$methods;
$NoneType foo$$l$3lambda$__init__ (foo$$l$3lambda p$self, foo$$Bar d$tmp, $Cont c$cont) {
    p$self->d$tmp = d$tmp;
    p$self->c$cont = c$cont;
    return $None;
}
$R foo$$l$3lambda$__call__ (foo$$l$3lambda p$self, $NoneType p$1) {
    foo$$Bar d$tmp = p$self->d$tmp;
    $Cont c$cont = p$self->c$cont;
    return foo$$l$1c$1cont(d$tmp, c$cont, p$1);
}
void foo$$l$3lambda$__serialize__ (foo$$l$3lambda self, $Serial$state state) {
    $step_serialize(self->d$tmp, state);
    $step_serialize(self->c$cont, state);
}
foo$$l$3lambda foo$$l$3lambda$__deserialize__ (foo$$l$3lambda self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct foo$$l$3lambda));
            self->$class = &foo$$l$3lambda$methods;
            return self;
        }
        self = $DNEW(foo$$l$3lambda, state);
    }
    self->d$tmp = $step_deserialize(state);
    self->c$cont = $step_deserialize(state);
    return self;
}
foo$$l$3lambda foo$$l$3lambda$new(foo$$Bar p$1, $Cont p$2) {
    foo$$l$3lambda $tmp = malloc(sizeof(struct foo$$l$3lambda));
    $tmp->$class = &foo$$l$3lambda$methods;
    foo$$l$3lambda$methods.__init__($tmp, p$1, p$2);
    return $tmp;
}
struct foo$$l$3lambda$class foo$$l$3lambda$methods;
$R foo$$Bar$__init__ (foo$$Bar __self__, $Cont c$cont) {
    return $R_CONT(c$cont, $None);
}
void foo$$Bar$__serialize__ (foo$$Bar self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
}
foo$$Bar foo$$Bar$__deserialize__ (foo$$Bar self, $Serial$state state) {
    if (!self) {
        if (!state) {
            self = malloc(sizeof(struct foo$$Bar));
            self->$class = &foo$$Bar$methods;
            return self;
        }
        self = $DNEW(foo$$Bar, state);
    }
    $Actor$methods.__deserialize__(($Actor)self, state);
    return self;
}
$R foo$$Bar$new($Cont p$1) {
    foo$$Bar $tmp = malloc(sizeof(struct foo$$Bar));
    $tmp->$class = &foo$$Bar$methods;
    return foo$$Bar$methods.__init__($tmp, $CONSTCONT($tmp, p$1));
}
struct foo$$Bar$class foo$$Bar$methods;
$R foo$$Bar$newact ($Cont c$cont) {
    foo$$Bar d$tmp = $NEWACTOR(foo$$Bar);
    return $AWAIT($ASYNC((($Actor)d$tmp), (($Cont)foo$$l$2lambda$new(d$tmp))), (($Cont)foo$$l$3lambda$new(d$tmp, c$cont)));
}
int foo$$done$ = 0;
void foo$$__init__ () {
    if (foo$$done$) return;
    foo$$done$ = 1;
    {
        foo$$l$2lambda$methods.$GCINFO = "foo$$l$2lambda";
        foo$$l$2lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        foo$$l$2lambda$methods.__bool__ = ($bool (*) (foo$$l$2lambda))$value$methods.__bool__;
        foo$$l$2lambda$methods.__str__ = ($str (*) (foo$$l$2lambda))$value$methods.__str__;
        foo$$l$2lambda$methods.__init__ = foo$$l$2lambda$__init__;
        foo$$l$2lambda$methods.__call__ = foo$$l$2lambda$__call__;
        foo$$l$2lambda$methods.__serialize__ = foo$$l$2lambda$__serialize__;
        foo$$l$2lambda$methods.__deserialize__ = foo$$l$2lambda$__deserialize__;
        $register(&foo$$l$2lambda$methods);
    }
    {
        foo$$l$3lambda$methods.$GCINFO = "foo$$l$3lambda";
        foo$$l$3lambda$methods.$superclass = ($Super$class)&$Cont$methods;
        foo$$l$3lambda$methods.__bool__ = ($bool (*) (foo$$l$3lambda))$value$methods.__bool__;
        foo$$l$3lambda$methods.__str__ = ($str (*) (foo$$l$3lambda))$value$methods.__str__;
        foo$$l$3lambda$methods.__init__ = foo$$l$3lambda$__init__;
        foo$$l$3lambda$methods.__call__ = foo$$l$3lambda$__call__;
        foo$$l$3lambda$methods.__serialize__ = foo$$l$3lambda$__serialize__;
        foo$$l$3lambda$methods.__deserialize__ = foo$$l$3lambda$__deserialize__;
        $register(&foo$$l$3lambda$methods);
    }
    {
        foo$$Bar$methods.$GCINFO = "foo$$Bar";
        foo$$Bar$methods.$superclass = ($Super$class)&$Actor$methods;
        foo$$Bar$methods.__bool__ = ($bool (*) (foo$$Bar))$Actor$methods.__bool__;
        foo$$Bar$methods.__str__ = ($str (*) (foo$$Bar))$Actor$methods.__str__;
        foo$$Bar$methods.__resume__ = ($NoneType (*) (foo$$Bar))$Actor$methods.__resume__;
        foo$$Bar$methods.__init__ = foo$$Bar$__init__;
        foo$$Bar$methods.__serialize__ = foo$$Bar$__serialize__;
        foo$$Bar$methods.__deserialize__ = foo$$Bar$__deserialize__;
        $register(&foo$$Bar$methods);
    }
}