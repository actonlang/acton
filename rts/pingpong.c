#include "rts.h"
#include "pingpong.h"

struct lambda$1$class lambda$1$methods = {
    "lambda$1",
    lambda$1$__init__,
    lambda$1$enter
};
struct lambda$2$class lambda$2$methods = {
    "lambda$2",
    lambda$2$__init__,
    lambda$2$enter
};
struct Pingpong$class Pingpong$methods = {
    "Pingpong",
    Pingpong$__init__,
    Pingpong$ping,
    Pingpong$pong
};

void lambda$1$__init__(lambda$1 $this, Pingpong self, $int count, $int q) {
    $this->self = self;
    $this->count = count;
    $this->q = q;
}
$R lambda$1$enter (lambda$1 $this, $Cont then) {
    Pingpong self = $this->self;
    $int count = $this->count;
    $int q = $this->q;
    return self->$class->pong(self, count, q, then);
}

void lambda$2$__init__(lambda$2 $this, Pingpong self, $int q) {
    $this->self = self;
    $this->q = q;
}
$R lambda$2$enter (lambda$2 $this, $Cont then) {
    Pingpong self = $this->self;
    $int q = $this->q;
    return self->$class->ping(self, q, then);
}

$R Pingpong$__init__(Pingpong self, $int i, $Cont then) {
    $Actor$methods.__init__(($Actor)self);
    self->count = i;
    return self->$class->ping(self, i, then);
}
$R Pingpong$ping(Pingpong self, $int q, $Cont then) {
    self->count = $Plus$int$witness->$class->__add__($Plus$int$witness, self->count, to$int(1));
    $int j = $Complex$int$witness->$class->__mul__($Complex$int$witness, self->count, q);
    printf("Ping %8ld\n", j->val);
    $AFTER(1, ($Cont)$NEW(lambda$1, self, self->count, $Complex$int$witness->$class->__neg__($Complex$int$witness, q)));
    return $R_CONT(then, $None);
}
$R Pingpong$pong(Pingpong self, $int n, $int q, $Cont then) {
    $int j = $Complex$int$witness->$class->__mul__($Complex$int$witness, n, q);
    printf("     %8ld Pong\n", j->val);
    $AFTER(2, ($Cont)$NEW(lambda$2, self, $Complex$int$witness->$class->__neg__($Complex$int$witness, q)));
    return $R_CONT(then, $None);
}

$R $ROOT($Env env, $Cont then) {
    return $NEWCC(Pingpong, then, to$int(env));
}
