#include "ActorRing.h"

/// lambda$1

void lambda$1$__init__(lambda$1 $this, $Cont cont$0) {
    $this->cont$0 = cont$0;
}

void lambda$1$__serialize__(lambda$1 self, $Mapping$dict wit, $WORD* prefix, $int prefix_size, $dict done, struct $ROWLISTHEADER accum) {
    // TBD
}

lambda$1 lambda$1$__deserialize__($Mapping$dict wit, $ROW *row, $dict done) {
    // TBD
    return NULL;
}

$R lambda$1$enter(lambda$1 $this, $Msg _ignore) {
    return $this->cont$0->$class->enter($this->cont$0, $None);
}

struct lambda$1$class lambda$1$methods = {
    "lambda$1",
    NULL,
    lambda$1$__init__,
    lambda$1$__serialize__,
    lambda$1$__deserialize__,
    lambda$1$enter
};

/// lambda$2

void lambda$2$__init__(lambda$2 $this, Act self, $int from, $list table) {
    $this->self = self;
    $this->from = from;
    $this->table = table;
}

void lambda$2$__serialize__(lambda$2 self, $Mapping$dict wit, $WORD* prefix, $int prefix_size, $dict done, struct $ROWLISTHEADER accum) {
    // TBD
}

lambda$2 lambda$2$__deserialize__($Mapping$dict wit, $ROW *row, $dict done) {
    // TBD
    return NULL;
}

$R lambda$2$enter(lambda$2 $this, $Cont c$1) {
    return $this->self->$class->act$local($this->self, $this->from, $this->table, c$1);
}

struct lambda$2$class lambda$2$methods = {
    "lambda$2",
    NULL,
    lambda$2$__init__,
    lambda$2$__serialize__,
    lambda$2$__deserialize__,
    lambda$2$enter
};

/// Act

$R Act$__init__(Act self, $int i, $Cont cont$0) {
    $Actor$methods.__init__(($Actor)self);
    self->i = i;
    self->count = to$int(0);
    self->rcv_dict = $NEW($dict, ($Hashable)$Hashable$int$witness, $None);
    self->snd_dict = $NEW($dict, ($Hashable)$Hashable$int$witness, $None);
    return $R_CONT(cont$0, $None);
}

void Act$__serialize__(Act self, $Mapping$dict wit, $WORD* prefix, $int prefix_size, $dict done, struct $ROWLISTHEADER accum) {
    // TBD
}

Act Act$__deserialize__($Mapping$dict wit, $ROW *row, $dict done) {
    // TBD
    return NULL;
}

$R Act$act$local(Act self, $int from, $list table, $Cont cont$0) {
    if (from$bool($Integral$int$witness->$class->__lt__($Integral$int$witness, self->count, total_msgs))) {
        self->count = $Plus$int$witness->$class->__add__($Plus$int$witness, self->count, to$int(1));
        $int to = $Integral$int$witness->$class->__mod__($Integral$int$witness, $Plus$int$witness->$class->__add__($Plus$int$witness, self->i, to$int(1)), no_actors);
        $Indexed$dict$witness->$class->__setitem__($Indexed$dict$witness, self->rcv_dict, from, $Plus$int$witness->$class->__add__($Plus$int$witness, $Mapping$dict$witness->$class->get($Mapping$dict$witness, self->rcv_dict, from, to$int(0)), to$int(1)));
        $Indexed$dict$witness->$class->__setitem__($Indexed$dict$witness, self->snd_dict, to,   $Plus$int$witness->$class->__add__($Plus$int$witness, $Mapping$dict$witness->$class->get($Mapping$dict$witness, self->snd_dict, to, to$int(0)), to$int(1)));
        printf("Actor %ld: count=%ld, from=%ld, to=%ld\n", from$int(self->i), from$int(self->count), from$int(from), from$int(to));
        Act tmp$1 = $Sequence$list$witness->$class->__getitem__($Sequence$list$witness, table, to);
        return tmp$1->$class->act(tmp$1, self->i, table, ($Cont)$NEW(lambda$1, cont$0));
    }
    return $R_CONT(cont$0, $None);
}
    
$R Act$act(Act self, $int from, $list table, $Cont cont$0) {
    return $R_CONT(cont$0, $ASYNC(($Actor)self, ($Cont)$NEW(lambda$2, self, from, table)));
}

struct Act$class Act$methods = {
    "Act",
    NULL,
    Act$__init__,
    Act$__serialize__,
    Act$__deserialize__,
    Act$act$local,
    Act$act
};

/// lambda$3

void lambda$3$__init__(lambda$3 $this, Root self, $Iterator iter$1, $Cont cont$0) {
    $this->self = self;
    $this->iter$1 = iter$1;
    $this->cont$0 = cont$0;
}

void lambda$3$__serialize__(lambda$3 self, $Mapping$dict wit, $WORD* prefix, $int prefix_size, $dict done, struct $ROWLISTHEADER accum) {
    // TBD
}

lambda$3 lambda$3$__deserialize__($Mapping$dict wit, $ROW *row, $dict done) {
    // TBD
    return NULL;
}

$R lambda$3$enter(lambda$3 $this, Act $res) {
    return cont$1($this->self, $this->iter$1, $this->cont$0, $res);
}

struct lambda$3$class lambda$3$methods = {
    "lambda$3",
    NULL,
    lambda$3$__init__,
    lambda$3$__serialize__,
    lambda$3$__deserialize__,
    lambda$3$enter
};

/// lambda$4

void lambda$4$__init__(lambda$4 $this, $Cont cont$0) {
    $this->cont$0 = cont$0;
}

void lambda$4$__serialize__(lambda$4 self, $Mapping$dict wit, $WORD* prefix, $int prefix_size, $dict done, struct $ROWLISTHEADER accum) {
    // TBD
}

lambda$4 lambda$4$__deserialize__($Mapping$dict wit, $ROW *row, $dict done) {
    // TBD
    return NULL;
}

$R lambda$4$enter(lambda$4 $this, $WORD _ignore) {
    return $this->cont$0->$class->enter($this->cont$0, $None);
}

struct lambda$4$class lambda$4$methods = {
    "lambda$4",
    NULL,
    lambda$4$__init__,
    lambda$4$__serialize__,
    lambda$4$__deserialize__,
    lambda$4$enter
};


/// Root

$R loop$1(Root self, $Iterator iter$1, $Cont cont$0, $WORD _ignore) {
    $int i = iter$1->$class->__next__(iter$1);
    if (i == $None) {
        return join$1(self, cont$0, $None);
    }
    return $NEWCC(Act, ($Cont)$NEW(lambda$3, self, iter$1, cont$0), i);
}

$R cont$1(Root self, $Iterator iter$1, $Cont cont$0, Act $res) {
    $Sequence$list$witness->$class->append($Sequence$list$witness, self->table, $res);
    return loop$1(self, iter$1, cont$0, $None);
}

$R join$1(Root self, $Cont cont$0, $WORD _ignore) {
    Act tmp$2 = $Sequence$list$witness->$class->__getitem__($Sequence$list$witness, self->table, to$int(0));
    return tmp$2->$class->act(tmp$2, no_actors, self->table, ($Cont)$NEW(lambda$4, cont$0));
}

$R Root$__init__(Root self, $int _ignore, $Cont cont$0) {
    $Actor$methods.__init__(($Actor)self);
    self->table = $NEW($list, $None);
    $Iterator iter$1 = $Iterable$range$witness->$class->__iter__($Iterable$range$witness, $NEW($range, no_actors, $None, $None));
    return loop$1(self, iter$1, cont$0, $None);
}

void Root$__serialize__(Root self, $Mapping$dict wit, $WORD* prefix, $int prefix_size, $dict done, struct $ROWLISTHEADER accum) {
    // TBD
}

Root Root$__deserialize__($Mapping$dict wit, $ROW *row, $dict done) {
    // TBD
    return NULL;
}

struct Root$class Root$methods = {
    "Root",
    NULL,
    Root$__init__,
    Root$__serialize__,
    Root$__deserialize__
};

/// Initialization

$Mapping $Mapping$dict$witness;
$Indexed $Indexed$dict$witness;

$int no_actors;
$int total_msgs;

void $init_module() {
    $Mapping$dict$witness = ($Mapping)$NEW($Mapping$dict, ($Hashable)$Hashable$int$witness);
    $Indexed$dict$witness = $Mapping$dict$witness->w$Indexed$Mapping;

    no_actors = to$int(5);
    total_msgs = to$int(20);
}

$R $ROOT($Env env, $Cont cont$0) {
    $init_module();
    return $NEWCC(Root, cont$0, to$int(env));
}
