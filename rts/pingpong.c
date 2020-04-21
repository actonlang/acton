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

void lambda$1$__init__(lambda$1 $this, Pingpong self, int count, int q) {
    $this->self = self;
    $this->count = count;
    $this->q = q;
}
$R lambda$1$enter (lambda$1 $this, $Cont then) {
    Pingpong self = $this->self;
    int count = $this->count;
    int q = $this->q;
    return self->__class__->pong(self, count, q, then);
}

void lambda$2$__init__(lambda$2 $this, Pingpong self, int q) {
    $this->self = self;
    $this->q = q;
}
$R lambda$2$enter (lambda$2 $this, $Cont then) {
    Pingpong self = $this->self;
    int q = $this->q;
    return self->__class__->ping(self, q, then);
}

$R Pingpong$__init__(Pingpong self, int i, $Cont then) {
    $Actor$methods.__init__(($Actor)self);
    self->count = i;
    return self->__class__->ping(self, i, then);
}
$R Pingpong$ping(Pingpong self, int q, $Cont then) {
    self->count = int_add(self->count, 1);
    int j = int_mul(self->count, q);
    printf("Ping %8d\n", j);
    $AFTER(1, ($Cont)$NEW(lambda$1, self, self->count, int_neg(q)));
    return $R_CONT(then, j);
}
$R Pingpong$pong(Pingpong self, int n, int q, $Cont then) {
    int j = int_mul(n, q);
    printf("     %8d Pong\n", j);
    $AFTER(2, ($Cont)$NEW(lambda$2, self, int_neg(q)));
    return $R_CONT(then, $None);
}
