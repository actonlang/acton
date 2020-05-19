#include "rts.h"
#include "pingpong.h"

void lambda$1$__init__(lambda$1 $this, Pingpong self, $int count, $int q) {
    $this->self = self;
    $this->count = count;
    $this->q = q;
}

void lambda$1$__serialize__(lambda$1 self, $Serial$state state) {
    $step_serialize(self->self,state);
    $step_serialize(self->count,state);
    $step_serialize(self->q,state);
}

lambda$1 lambda$1$__deserialize__($Serial$state state) {
    lambda$1 res = $DNEW(lambda$1,state);
    res->self = $step_deserialize(state);
    res->count = $step_deserialize(state);
    res->q = $step_deserialize(state);
    return res;
}

$R lambda$1$enter(lambda$1 $this, $Cont then) {
    Pingpong self = $this->self;
    $int count = $this->count;
    $int q = $this->q;
    return self->$class->pong(self, count, $Complex$int$witness->$class->__neg__($Complex$int$witness, q), then);
}

////////////////////////////////////////////////////////////////////////////////////////

void lambda$2$__init__(lambda$2 $this, Pingpong self, $int q) {
    $this->self = self;
    $this->q = q;
}

void lambda$2$__serialize__(lambda$2 self, $Serial$state state) {
    $step_serialize(self->self,state);
    $step_serialize(self->q,state);
}


lambda$2 lambda$2$__deserialize__($Serial$state state) {
    lambda$2 res = $DNEW(lambda$2,state);
    res->self = $step_deserialize(state);
    res->q = $step_deserialize(state);
    return res;
}


$R lambda$2$enter (lambda$2 $this, $Cont then) {
    Pingpong self = $this->self;
    $int q = $this->q;
    return self->$class->ping(self, q, then);
}

////////////////////////////////////////////////////////////////////////////////////////

$R Pingpong$__init__(Pingpong self, $int i, $Cont then) {
    $Actor$methods.__init__(($Actor)self);
    self->i = i;
    self->count = i;
    return self->$class->ping(self, i, then);
}

void Pingpong$__serialize__(Pingpong self, $Serial$state state) {
    $step_serialize(self->i,state);
    $step_serialize(self->count,state);
}

Pingpong Pingpong$__deserialize__($Serial$state state) {
  Pingpong res = $DNEW(Pingpong,state);
    res->i = $step_deserialize(state);
    res->count = $step_deserialize(state);
    return res;
}

////////////////////////////////////////////////////////////////////////////////////////

$R Pingpong$ping(Pingpong self, $int q, $Cont then) {
    self->count = $Plus$int$witness->$class->__add__($Plus$int$witness, self->count, to$int(1));
    $int j = $Complex$int$witness->$class->__mul__($Complex$int$witness, self->count, q);
    printf("Ping %8ld\n", j->val);
    $AFTER(1, ($Cont)$NEW(lambda$1, self, self->count, q));
    return $R_CONT(then, $None);
}
$R Pingpong$pong(Pingpong self, $int n, $int q, $Cont then) {
    $int j = $Complex$int$witness->$class->__mul__($Complex$int$witness, n, q);
    printf("     %8ld Pong\n", j->val);
    $AFTER(2, ($Cont)$NEW(lambda$2, self, $Complex$int$witness->$class->__neg__($Complex$int$witness, q)));
    return $R_CONT(then, $None);
}

struct lambda$1$class lambda$1$methods = {
    "lambda$1",
    UNASSIGNED,
    NULL,
    lambda$1$__init__,
    lambda$1$__serialize__,
    lambda$1$__deserialize__,
    lambda$1$enter
};
struct lambda$2$class lambda$2$methods = {
    "lambda$2",
    UNASSIGNED,
    NULL,
    lambda$2$__init__,
    lambda$2$__serialize__,
    lambda$2$__deserialize__,
    lambda$2$enter
};
struct Pingpong$class Pingpong$methods = {
    "Pingpong",
    UNASSIGNED,
    NULL,
    Pingpong$__init__,
    Pingpong$__serialize__,
    Pingpong$__deserialize__,
    Pingpong$ping,
    Pingpong$pong
};

$R $ROOT($Env env, $Cont then) {
    $register(&lambda$1$methods);
    $register(&lambda$2$methods);
    $register(&Pingpong$methods);
    return $NEWCC(Pingpong, then, to$int(env));
}

