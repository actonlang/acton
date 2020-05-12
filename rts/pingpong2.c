#include "rts.h"
#include "pingpong.h"

void lambda$1$__init__(lambda$1 $this, Pingpong self, $int count, $int q) {
    $this->self = self;
    $this->count = count;
    $this->q = q;
}

void lambda$1$__serialize__(lambda$1 self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  int class_id = $get_classid(($Serializable$methods)self->$class);
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-class_id,&prevkey->val,start_no,accum);
  } else {
    $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
    $enqueue(accum,$new_row(class_id,start_no,0,NULL));
    $step_serialize(($Serializable)self->self,wit,start_no,done,accum);
    $step_serialize(($Serializable)self->count,wit,start_no,done,accum);
    $step_serialize(($Serializable)self->q,wit,start_no,done,accum);
  }
}

lambda$1 lambda$1$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  if ((*row)->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)(*row)->blob[0]),NULL);
  } else {
    lambda$1 res = malloc(sizeof(struct lambda$1));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int((*row)->row_no),res);
    *row = (*row)->next;
    res->$class = &lambda$1$methods;
    res->self = (Pingpong)$step_deserialize(wit,row,done);
    res->count = ($int)$step_deserialize(wit,row,done);
    res->q = ($int)$step_deserialize(wit,row,done);
    return res;
  }
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

void lambda$2$__serialize__(lambda$2 self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  int class_id = $get_classid(($Serializable$methods)self->$class);
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-class_id,&prevkey->val,start_no,accum);
  } else {
    $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
    $enqueue(accum,$new_row(class_id,start_no,0,NULL));
    $step_serialize(($Serializable)self->self,wit,start_no,done,accum);
    $step_serialize(($Serializable)self->q,wit,start_no,done,accum);
  }
}

lambda$2 lambda$2$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  if ((*row)->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)(*row)->blob[0]),NULL);
  } else {
    lambda$2 res = malloc(sizeof(struct lambda$2));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int((*row)->row_no),res);
    *row = (*row)->next;
    res->$class = &lambda$2$methods;
    res->self = (Pingpong)$step_deserialize(wit,row,done);
    res->q = ($int)$step_deserialize(wit,row,done);
    return res;
  }
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

void Pingpong$__serialize__(Pingpong self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  int class_id = $get_classid(($Serializable$methods)self->$class);
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-class_id,&prevkey->val,start_no,accum);
  } else {
    $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
    $enqueue(accum,$new_row(class_id,start_no,0,NULL));
    $step_serialize(($Serializable)self->i,wit,start_no,done,accum);
    $step_serialize(($Serializable)self->count,wit,start_no,done,accum);
  }
}

Pingpong Pingpong$__deserialize__($Mapping$dict wit, $ROW* row, $dict done) {
  if ((*row)->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)(*row)->blob[0]),NULL);
  } else {
    Pingpong res = malloc(sizeof(struct Pingpong));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int((*row)->row_no),res);
    *row = (*row)->next;
    res->$class = &Pingpong$methods;
    res->i = ($int)$step_deserialize(wit,row,done);
    res->count = ($int)$step_deserialize(wit,row,done);
    return res;
  }
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
    NULL,
    lambda$1$__init__,
    lambda$1$__serialize__,
    lambda$1$__deserialize__,
    lambda$1$enter
};
struct lambda$2$class lambda$2$methods = {
    "lambda$2",
    NULL,
    lambda$2$__init__,
    lambda$2$__serialize__,
    lambda$2$__deserialize__,
    lambda$2$enter
};
struct Pingpong$class Pingpong$methods = {
    "Pingpong",
    NULL,
    Pingpong$__init__,
    Pingpong$__serialize__,
    Pingpong$__deserialize__,
    Pingpong$ping,
    Pingpong$pong
};

$R $ROOT($Env env, $Cont then) {
     $register_builtin();
     $register_rts();
     $register(($Serializable$methods)&lambda$1$methods);
     $register(($Serializable$methods)&lambda$2$methods);
     $register(($Serializable$methods)&Pingpong$methods);
    return $NEWCC(Pingpong, then, to$int(env));
}

