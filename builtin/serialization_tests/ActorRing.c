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

void lambda$1D___init__(lambda$1 $this, $Cont cont$0) {
    $this->cont$0 = cont$0;
}

B_bool lambda$1D___bool__(lambda$1 self) {
  return B_True;
}

B_str lambda$1D___str__(lambda$1 self) {
  char *s;
  asprintf(&s,"<lambda$1 object at %p>",self);
  return to$str(s);
}

void lambda$1D___serialize__(lambda$1 self, B_NoneType state) {
  $step_serialize(self->cont$0,state); 
}

lambda$1 lambda$1D___deserialize__(B_NoneType state) {
  lambda$1 res = $DNEW(lambda$1,state);
  res->cont$0 = $step_deserialize(state);  
  return res;
}

$R lambda$1D___call__(lambda$1 $this, B_Msg _ignore) {
    return $this->cont$0->$class->__call__($this->cont$0, B_None);
}

struct lambda$1G_class lambda$1G_methods = {
    "lambda$1",
    UNASSIGNED,
    NULL,
    lambda$1D___init__,
    lambda$1D___serialize__,
    lambda$1D___deserialize__,
    lambda$1D___bool__,
    lambda$1D___str__,
    lambda$1D___call__
};

/// lambda$2

void lambda$2D___init__(lambda$2 $this, Act self, B_int from, B_list table) {
    $this->self = self;
    $this->from = from;
    $this->table = table;
}

B_bool lambda$2D___bool__(lambda$2 self) {
  return B_True;
}

B_str lambda$2D___str__(lambda$2 self) {
  char *s;
  asprintf(&s,"<lambda$2 object at %p>",self);
  return to$str(s);
}

void lambda$2D___serialize__(lambda$2 self, B_NoneType state) {
  $step_serialize(self->self,state); 
  $step_serialize(self->from,state); 
  $step_serialize(self->table,state);
}


lambda$2 lambda$2D___deserialize__(B_NoneType state) {
  lambda$2 res = $DNEW(lambda$2,state);
  res->self = $step_deserialize(state);      
  res->from = $step_deserialize(state);     
  res->table = $step_deserialize(state);   
  return res;
}

$R lambda$2D___call__(lambda$2 $this, $Cont c$1) {
    return $this->self->$class->actG_local($this->self, $this->from, $this->table, c$1);
}

struct lambda$2G_class lambda$2G_methods = {
    "lambda$2",
    UNASSIGNED,
    NULL,
    lambda$2D___init__,
    lambda$2D___serialize__,
    lambda$2D___deserialize__,
    lambda$2D___bool__,
    lambda$2D___str__,
    lambda$2D___call__
};

/// Act

$R ActD___init__(Act self, B_int i, $Cont cont$0) {
    $ActorG_methods.__init__(($Actor)self);
    self->i = i;
    self->count = to$int(0);
    self->rcv_dict = B_dictG_new((B_Hashable)B_HashableD_intG_witness, NULL, B_None);
    self->snd_dict = B_dictG_new((B_Hashable)B_HashableD_intG_witness, NULL, B_None);
    return $R_CONT(cont$0, B_None);
}

B_bool ActD___bool__(Act self) {
  return B_True;
}

B_str ActD___str__(Act self) {
  char *s;
  asprintf(&s,"<Act object at %p>",self);
  return to$str(s);
}

void ActD___serialize__(Act self, B_NoneType state) {
 $step_serialize(self->i,state);        
 $step_serialize(self->count,state);    
 $step_serialize(self->rcv_dict,state); 
 $step_serialize(self->snd_dict,state); 
}

Act ActD___deserialize__(B_NoneType state) {
  Act res = $DNEW(Act,state);
  res->i = $step_deserialize(state);         
  res->count = $step_deserialize(state);     
  res->rcv_dict = $step_deserialize(state); 
  res->snd_dict = $step_deserialize(state); 
  return res;
}

$R Act$actG_local(Act self, B_int from, B_list table, $Cont cont$0) {
    if (fromB_bool(B_OrdD_intG_witness->$class->__lt__(B_OrdD_intG_witness, self->count, total_msgs))) {
        self->count = B_IntegralD_intG_witness->$class->__add__(B_IntegralD_intG_witness, self->count, to$int(1));
        B_int to = B_IntegralD_intG_witness->$class->__mod__(B_IntegralD_intG_witness, B_IntegralD_intG_witness->$class->__add__(B_IntegralD_intG_witness, self->i, to$int(1)), no_actors);
        B_IndexedD_MappingD_dictG_witness->$class->__setitem__(B_IndexedD_MappingD_dictG_witness, self->rcv_dict, from, B_IntegralD_intG_witness->$class->__add__(B_IntegralD_intG_witness, B_MappingD_dictG_witness->$class->get(B_MappingD_dictG_witness, self->rcv_dict, from, to$int(0)), to$int(1)));
        B_IndexedD_MappingD_dictG_witness->$class->__setitem__(B_IndexedD_MappingD_dictG_witness, self->snd_dict, to,   B_IntegralD_intG_witness->$class->__add__(B_IntegralD_intG_witness, B_MappingD_dictG_witness->$class->get(B_MappingD_dictG_witness, self->snd_dict, to, to$int(0)), to$int(1)));
        printf("Actor %ld: count=%ld, from=%ld, to=%ld\n", from$int(self->i), from$int(self->count), from$int(from), from$int(to));
        Act tmp$1 = B_SequenceD_listG_witness->$class->__getitem__(B_SequenceD_listG_witness, table, to);
        return tmp$1->$class->act(tmp$1, self->i, table, ($Cont)$NEW(lambda$1, cont$0));
    }
    return $R_CONT(cont$0, B_None);
}
    
$R Act$act(Act self, B_int from, B_list table, $Cont cont$0) {
    return $R_CONT(cont$0, $ASYNC(($Actor)self, ($Cont)$NEW(lambda$2, self, from, table)));
}

struct ActG_class ActG_methods = {
    "Act",
    UNASSIGNED,
    NULL,
    ActD___init__,
    ActD___serialize__,
    ActD___deserialize__,
    ActD___bool__,
    ActD___str__,
    Act$actG_local,
    Act$act
};

/// lambda$3

void lambda$3D___init__(lambda$3 $this, Root self, B_Iterator iter$1, $Cont cont$0) {
    $this->self = self;
    $this->iter$1 = iter$1;
    $this->cont$0 = cont$0;
}

B_bool lambda$3D___bool__(lambda$3 self) {
  return B_True;
}

B_str lambda$3D___str__(lambda$3 self) {
  char *s;
  asprintf(&s,"<lambda$3 object at %p>",self);
  return to$str(s);
}

void lambda$3D___serialize__(lambda$3 self, B_NoneType state) {
  $step_serialize(self->self,state);  
  $step_serialize(self->iter$1,state);
  $step_serialize(self->cont$0,state);
}

lambda$3 lambda$3D___deserialize__(B_NoneType state) {
  lambda$3 res = $DNEW(lambda$3,state);
  res->self = $step_deserialize(state);        
  res->iter$1 = $step_deserialize(state); 
  res->cont$0 = $step_deserialize(state);     
  return res;
}

$R lambda$3D___call__(lambda$3 $this, Act $res) {
    return cont$1($this->self, $this->iter$1, $this->cont$0, $res);
}

struct lambda$3G_class lambda$3G_methods = {
    "lambda$3",
    UNASSIGNED,
    NULL,
    lambda$3D___init__,
    lambda$3D___serialize__,
    lambda$3D___deserialize__,
    lambda$3D___bool__,
    lambda$3D___str__,
    lambda$3D___call__
};

/// lambda$4

void lambda$4D___init__(lambda$4 $this, $Cont cont$0) {
    $this->cont$0 = cont$0;
}

B_bool lambda$4D___bool__(lambda$4 self) {
  return B_True;
}

B_str lambda$4D___str__(lambda$4 self) {
  char *s;
  asprintf(&s,"<lambda$4 object at %p>",self);
  return to$str(s);
}

void lambda$4D___serialize__(lambda$4 self, B_NoneType state) {
   $step_serialize(self->cont$0,state); 
}

lambda$4 lambda$4D___deserialize__(B_NoneType state) {
  lambda$4 res = $DNEW(lambda$4,state);
  res->cont$0 = ($Cont)$step_deserialize(state);  
  return res;
}

$R lambda$4D___call__(lambda$4 $this, $WORD _ignore) {
    return $this->cont$0->$class->__call__($this->cont$0, B_None);
}

struct lambda$4G_class lambda$4G_methods = {
    "lambda$4",
    UNASSIGNED,
    NULL,
    lambda$4D___init__,
    lambda$4D___serialize__,
    lambda$4D___deserialize__,
    lambda$4D___bool__,
    lambda$4D___str__,
    lambda$4D___call__
};


/// Root

$R loop$1(Root self, B_Iterator iter$1, $Cont cont$0, $WORD _ignore) {
    B_int i = iter$1->$class->__next__(iter$1);
    if (i == B_None) {
        return join$1(self, cont$0, B_None);
    }
    return $NEWCC(Act, ($Cont)$NEW(lambda$3, self, iter$1, cont$0), i);
}

$R cont$1(Root self, B_Iterator iter$1, $Cont cont$0, Act $res) {
    B_SequenceD_listG_witness->$class->append(B_SequenceD_listG_witness, self->table, $res);
    return loop$1(self, iter$1, cont$0, B_None);
}

$R join$1(Root self, $Cont cont$0, $WORD _ignore) {
    Act tmp$2 = B_SequenceD_listG_witness->$class->__getitem__(B_SequenceD_listG_witness, self->table, to$int(0));
    return tmp$2->$class->act(tmp$2, no_actors, self->table, ($Cont)$NEW(lambda$4, cont$0));
}

$R RootD___init__(Root self, B_Env _ignore, $Cont cont$0) {
    $ActorG_methods.__init__(($Actor)self);
    self->table = B_listG_new(NULL, B_None);
    B_Iterator iter$1 = B_IterableD_rangeG_witness->$class->__iter__(B_IterableD_rangeG_witness, $NEW(B_range, no_actors, B_None, B_None));
    return loop$1(self, iter$1, cont$0, B_None);
}

B_bool RootD___bool__(Root self) {
  return B_True;
}

B_str RootD___str__(Root self) {
  char *s;
  asprintf(&s,"<Root object at %p>",self);
  return to$str(s);
}

void RootD___serialize__(Root self, B_NoneType state) {
  $step_serialize(self->table,state); 
}

Root RootD___deserialize__(B_NoneType state) {
  Root res = $DNEW(Root,state);
  res->table = $step_deserialize(state);   
  return res;
}

struct RootG_class RootG_methods = {
    "Root",
    UNASSIGNED,
    NULL,
    RootD___init__,
    RootD___serialize__,
    RootD___deserialize__,
    RootD___bool__,
    RootD___str__
};

$R RootG_new(B_Env env, $Cont cont) {
    Root $tmp = malloc(sizeof(struct Root));
    $tmp->$class = &RootG_methods;
    return RootG_methods.__init__($tmp, env, $CONSTCONT($tmp, cont));
}

/// Initialization

B_Mapping B_MappingD_dictG_witness;
B_Indexed B_IndexedD_MappingD_dictG_witness;

B_int no_actors;
B_int total_msgs;

void $init_module() {
    B_MappingD_dictG_witness = (B_Mapping)$NEW(B_MappingD_dict, (B_Hashable)B_HashableD_intG_witness);
    B_IndexedD_MappingD_dictG_witness = B_MappingD_dictG_witness->W_Indexed;
    
    no_actors = to$int(5);
    total_msgs = to$int(20);
    $register_builtin();
    $register(&lambda$1G_methods);
    $register(&lambda$2G_methods);
    $register(&lambda$3G_methods);
    $register(&lambda$4G_methods);
    $register(&RootG_methods);
    $register(&ActG_methods);
}

$R $ROOT (B_Env env, $Cont cont) {
    $init_module();
    return RootG_new(env, cont);
}
