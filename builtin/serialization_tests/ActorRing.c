/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "ActorRing.h"

/// lambda$1

void lambda$1$__init__(lambda$1 $this, $Cont cont$0) {
    $this->cont$0 = cont$0;
}

$bool lambda$1$__bool__(lambda$1 self) {
  return $True;
}

$str lambda$1$__str__(lambda$1 self) {
  char *s;
  asprintf(&s,"<lambda$1 object at %p>",self);
  return to$str(s);
}

void lambda$1$__serialize__(lambda$1 self, $Serial$state state) {
  $step_serialize(self->cont$0,state); 
}

lambda$1 lambda$1$__deserialize__($Serial$state state) {
  lambda$1 res = $DNEW(lambda$1,state);
  res->cont$0 = $step_deserialize(state);  
  return res;
}

$R lambda$1$__call__(lambda$1 $this, $Msg _ignore) {
    return $this->cont$0->$class->__call__($this->cont$0, $None);
}

struct lambda$1$class lambda$1$methods = {
    "lambda$1",
    UNASSIGNED,
    NULL,
    lambda$1$__init__,
    lambda$1$__serialize__,
    lambda$1$__deserialize__,
    lambda$1$__bool__,
    lambda$1$__str__,
    lambda$1$__call__
};

/// lambda$2

void lambda$2$__init__(lambda$2 $this, Act self, $int from, $list table) {
    $this->self = self;
    $this->from = from;
    $this->table = table;
}

$bool lambda$2$__bool__(lambda$2 self) {
  return $True;
}

$str lambda$2$__str__(lambda$2 self) {
  char *s;
  asprintf(&s,"<lambda$2 object at %p>",self);
  return to$str(s);
}

void lambda$2$__serialize__(lambda$2 self, $Serial$state state) {
  $step_serialize(self->self,state); 
  $step_serialize(self->from,state); 
  $step_serialize(self->table,state);
}


lambda$2 lambda$2$__deserialize__($Serial$state state) {
  lambda$2 res = $DNEW(lambda$2,state);
  res->self = $step_deserialize(state);      
  res->from = $step_deserialize(state);     
  res->table = $step_deserialize(state);   
  return res;
}

$R lambda$2$__call__(lambda$2 $this, $Cont c$1) {
    return $this->self->$class->act$local($this->self, $this->from, $this->table, c$1);
}

struct lambda$2$class lambda$2$methods = {
    "lambda$2",
    UNASSIGNED,
    NULL,
    lambda$2$__init__,
    lambda$2$__serialize__,
    lambda$2$__deserialize__,
    lambda$2$__bool__,
    lambda$2$__str__,
    lambda$2$__call__
};

/// Act

$R Act$__init__(Act self, $int i, $Cont cont$0) {
    $Actor$methods.__init__(($Actor)self);
    self->i = i;
    self->count = to$int(0);
    self->rcv_dict = $dict$new(($Hashable)$Hashable$int$witness, NULL, $None);
    self->snd_dict = $dict$new(($Hashable)$Hashable$int$witness, NULL, $None);
    return $R_CONT(cont$0, $None);
}

$bool Act$__bool__(Act self) {
  return $True;
}

$str Act$__str__(Act self) {
  char *s;
  asprintf(&s,"<Act object at %p>",self);
  return to$str(s);
}

void Act$__serialize__(Act self, $Serial$state state) {
 $step_serialize(self->i,state);        
 $step_serialize(self->count,state);    
 $step_serialize(self->rcv_dict,state); 
 $step_serialize(self->snd_dict,state); 
}

Act Act$__deserialize__($Serial$state state) {
  Act res = $DNEW(Act,state);
  res->i = $step_deserialize(state);         
  res->count = $step_deserialize(state);     
  res->rcv_dict = $step_deserialize(state); 
  res->snd_dict = $step_deserialize(state); 
  return res;
}

$R Act$act$local(Act self, $int from, $list table, $Cont cont$0) {
    if (from$bool($Ord$int$witness->$class->__lt__($Ord$int$witness, self->count, total_msgs))) {
        self->count = $Integral$int$witness->$class->__add__($Integral$int$witness, self->count, to$int(1));
        $int to = $Integral$int$witness->$class->__mod__($Integral$int$witness, $Integral$int$witness->$class->__add__($Integral$int$witness, self->i, to$int(1)), no_actors);
        $Indexed$dict$witness->$class->__setitem__($Indexed$dict$witness, self->rcv_dict, from, $Integral$int$witness->$class->__add__($Integral$int$witness, $Mapping$dict$witness->$class->get($Mapping$dict$witness, self->rcv_dict, from, to$int(0)), to$int(1)));
        $Indexed$dict$witness->$class->__setitem__($Indexed$dict$witness, self->snd_dict, to,   $Integral$int$witness->$class->__add__($Integral$int$witness, $Mapping$dict$witness->$class->get($Mapping$dict$witness, self->snd_dict, to, to$int(0)), to$int(1)));
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
    UNASSIGNED,
    NULL,
    Act$__init__,
    Act$__serialize__,
    Act$__deserialize__,
    Act$__bool__,
    Act$__str__,
    Act$act$local,
    Act$act
};

/// lambda$3

void lambda$3$__init__(lambda$3 $this, Root self, $Iterator iter$1, $Cont cont$0) {
    $this->self = self;
    $this->iter$1 = iter$1;
    $this->cont$0 = cont$0;
}

$bool lambda$3$__bool__(lambda$3 self) {
  return $True;
}

$str lambda$3$__str__(lambda$3 self) {
  char *s;
  asprintf(&s,"<lambda$3 object at %p>",self);
  return to$str(s);
}

void lambda$3$__serialize__(lambda$3 self, $Serial$state state) {
  $step_serialize(self->self,state);  
  $step_serialize(self->iter$1,state);
  $step_serialize(self->cont$0,state);
}

lambda$3 lambda$3$__deserialize__($Serial$state state) {
  lambda$3 res = $DNEW(lambda$3,state);
  res->self = $step_deserialize(state);        
  res->iter$1 = $step_deserialize(state); 
  res->cont$0 = $step_deserialize(state);     
  return res;
}

$R lambda$3$__call__(lambda$3 $this, Act $res) {
    return cont$1($this->self, $this->iter$1, $this->cont$0, $res);
}

struct lambda$3$class lambda$3$methods = {
    "lambda$3",
    UNASSIGNED,
    NULL,
    lambda$3$__init__,
    lambda$3$__serialize__,
    lambda$3$__deserialize__,
    lambda$3$__bool__,
    lambda$3$__str__,
    lambda$3$__call__
};

/// lambda$4

void lambda$4$__init__(lambda$4 $this, $Cont cont$0) {
    $this->cont$0 = cont$0;
}

$bool lambda$4$__bool__(lambda$4 self) {
  return $True;
}

$str lambda$4$__str__(lambda$4 self) {
  char *s;
  asprintf(&s,"<lambda$4 object at %p>",self);
  return to$str(s);
}

void lambda$4$__serialize__(lambda$4 self, $Serial$state state) {
   $step_serialize(self->cont$0,state); 
}

lambda$4 lambda$4$__deserialize__($Serial$state state) {
  lambda$4 res = $DNEW(lambda$4,state);
  res->cont$0 = ($Cont)$step_deserialize(state);  
  return res;
}

$R lambda$4$__call__(lambda$4 $this, $WORD _ignore) {
    return $this->cont$0->$class->__call__($this->cont$0, $None);
}

struct lambda$4$class lambda$4$methods = {
    "lambda$4",
    UNASSIGNED,
    NULL,
    lambda$4$__init__,
    lambda$4$__serialize__,
    lambda$4$__deserialize__,
    lambda$4$__bool__,
    lambda$4$__str__,
    lambda$4$__call__
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

$R Root$__init__(Root self, $Env _ignore, $Cont cont$0) {
    $Actor$methods.__init__(($Actor)self);
    self->table = $list$new(NULL, $None);
    $Iterator iter$1 = $Iterable$range$witness->$class->__iter__($Iterable$range$witness, $NEW($range, no_actors, $None, $None));
    return loop$1(self, iter$1, cont$0, $None);
}

$bool Root$__bool__(Root self) {
  return $True;
}

$str Root$__str__(Root self) {
  char *s;
  asprintf(&s,"<Root object at %p>",self);
  return to$str(s);
}

void Root$__serialize__(Root self, $Serial$state state) {
  $step_serialize(self->table,state); 
}

Root Root$__deserialize__($Serial$state state) {
  Root res = $DNEW(Root,state);
  res->table = $step_deserialize(state);   
  return res;
}

struct Root$class Root$methods = {
    "Root",
    UNASSIGNED,
    NULL,
    Root$__init__,
    Root$__serialize__,
    Root$__deserialize__,
    Root$__bool__,
    Root$__str__
};

$R Root$new($Env env, $Cont cont) {
    Root $tmp = malloc(sizeof(struct Root));
    $tmp->$class = &Root$methods;
    return Root$methods.__init__($tmp, env, $CONSTCONT($tmp, cont));
}

/// Initialization

$Mapping $Mapping$dict$witness;
$Indexed $Indexed$dict$witness;

$int no_actors;
$int total_msgs;

void $init_module() {
    $Mapping$dict$witness = ($Mapping)$NEW($Mapping$dict, ($Hashable)$Hashable$int$witness);
    $Indexed$dict$witness = $Mapping$dict$witness->w$Indexed;
    
    no_actors = to$int(5);
    total_msgs = to$int(20);
    $register_builtin();
    $register(&lambda$1$methods);
    $register(&lambda$2$methods);
    $register(&lambda$3$methods);
    $register(&lambda$4$methods);
    $register(&Root$methods);
    $register(&Act$methods);
}

$R $ROOT ($Env env, $Cont cont) {
    $init_module();
    return Root$new(env, cont);
}
