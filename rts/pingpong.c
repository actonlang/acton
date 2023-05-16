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

#include "rts.h"
#include "pingpong.h"
#include "../builtin/env.h"

void lambda$1D___init__(lambda$1 $this, Pingpong self, B_int count, B_int q) {
    $this->self = self;
    $this->count = count;
    $this->q = q;
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
    $step_serialize(self->self,state);
    $step_serialize(self->count,state);
    $step_serialize(self->q,state);
}

lambda$1 lambda$1D___deserialize__(lambda$1 self, B_NoneType state) {
    lambda$1 res = $DNEW(lambda$1,state);
    res->self = $step_deserialize(state);
    res->count = $step_deserialize(state);
    res->q = $step_deserialize(state);
    return res;
}

$R lambda$1D___call__(lambda$1 $this, $Cont then) {
    Pingpong self = $this->self;
    B_int count = $this->count;
    B_int q = $this->q;
    return self->$class->pong(self, count, B_IntegralD_intG_witness->$class->__neg__(B_IntegralD_intG_witness, q), then);
}

lambda$1 lambda$1G_new(Pingpong self, B_int count, B_int q) {
    lambda$1 obj = malloc(sizeof(struct lambda$1));
    obj->$class = &lambda$1G_methods;
    lambda$1G_methods.__init__(obj, self, count, q);
    return obj;
}

////////////////////////////////////////////////////////////////////////////////////////

void lambda$2D___init__(lambda$2 $this, Pingpong self, B_int q) {
    $this->self = self;
    $this->q = q;
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
    $step_serialize(self->q,state);
}


lambda$2 lambda$2D___deserialize__(lambda$2 self, B_NoneType state) {
    lambda$2 res = $DNEW(lambda$2,state);
    res->self = $step_deserialize(state);
    res->q = $step_deserialize(state);
    return res;
}


$R lambda$2D___call__ (lambda$2 $this, $Cont then) {
    Pingpong self = $this->self;
    B_int q = $this->q;
    return self->$class->ping(self, q, then);
}

lambda$2 lambda$2G_new(Pingpong self, B_int q) {
    lambda$2 obj = malloc(sizeof(struct lambda$2));
    obj->$class = &lambda$2G_methods;
    lambda$2G_methods.__init__(obj, self, q);
    return obj;
}

////////////////////////////////////////////////////////////////////////////////////////

void lambda$3D___init__(lambda$3 $this, $Cont cont) {
    $this->cont = cont;
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
    $step_serialize(self->cont,state);
}

lambda$3 lambda$3D___deserialize__(lambda$3 self, B_NoneType state) {
    lambda$3 res = $DNEW(lambda$3,state);
    res->cont = $step_deserialize(state);
    return res;
}

$R lambda$3D___call__ (lambda$3 $this, B_NoneType none) {
    $OLDACT();
    $Cont cont = $this->cont;
    return $R_CONT(cont, none);
}

lambda$3 lambda$3G_new($Cont cont) {
    lambda$3 obj = malloc(sizeof(struct lambda$3));
    obj->$class = &lambda$3G_methods;
    lambda$3G_methods.__init__(obj, cont);
    return obj;
}

////////////////////////////////////////////////////////////////////////////////////////

$R PingpongD___init__(Pingpong self, B_int i, $Cont then) {
    $ActorG_methods.__init__(($Actor)self);
    $NEWACT(($Actor)self);
    self->i = i;
    self->count = i;
    return self->$class->ping(self, i, ($Cont)lambda$3G_new(then));
}

B_bool PingpongD___bool__(Pingpong self) {
    return B_True;
}

B_str PingpongD___str__(Pingpong self) {
    char *s;
    asprintf(&s,"<Pingpong object at %p>",self);
    return to$str(s);
}
void PingpongD___serialize__(Pingpong self, B_NoneType state) {
    $ActorG_methods.__serialize__(($Actor)self, state);
    $step_serialize(self->i,state);
    $step_serialize(self->count,state);
}

Pingpong PingpongD___deserialize__(Pingpong res, B_NoneType state) {
    if (!res)
        res = $DNEW(Pingpong,state);
    $ActorG_methods.__deserialize__(($Actor)res, state);
    res->i = $step_deserialize(state);
    res->count = $step_deserialize(state);
    return res;
}

$R Pingpong$ping(Pingpong self, B_int q, $Cont then) {
    self->count = B_IntegralD_intG_witness->$class->__add__(B_IntegralD_intG_witness, self->count, to$int(1));
    B_int j = B_IntegralD_intG_witness->$class->__mul__(B_IntegralD_intG_witness, self->count, q);
    B_print(1, $FORMAT("%ld Ping %8ld", self->i->val, j->val));
    $AFTER(to$int(1), ($Cont)lambda$1G_new(self, self->count, q));
    return $R_CONT(then, B_None);
}
$R Pingpong$pong(Pingpong self, B_int n, B_int q, $Cont then) {
    B_int j = B_IntegralD_intG_witness->$class->__mul__(B_IntegralD_intG_witness, n, q);
    B_print(1, $FORMAT("%ld       %7ld Pong", self->i->val, j->val));
    $AFTER(to$int(2), ($Cont)lambda$2G_new(self, B_IntegralD_intG_witness->$class->__neg__(B_IntegralD_intG_witness, q)));
    return $R_CONT(then, B_None);
}

$R PingpongG_new(B_int i, $Cont then) {
    Pingpong obj = malloc(sizeof(struct Pingpong));
    obj->$class = &PingpongG_methods;
    return PingpongG_methods.__init__(obj, i, $CONSTCONT(obj,then));
}

////////////////////////////////////////////////////////////////////////////////////////

struct lambda$1G_class lambda$1G_methods = {
    0,
    "lambda$1",
    UNASSIGNED,
    ($SuperG_class)&$ContG_methods,
    lambda$1D___init__,
    lambda$1D___serialize__,
    lambda$1D___deserialize__,
    lambda$1D___bool__,
    lambda$1D___str__,
    lambda$1D___call__
};
struct lambda$2G_class lambda$2G_methods = {
    0,
    "lambda$2",
    UNASSIGNED,
    ($SuperG_class)&$ContG_methods,
    lambda$2D___init__,
    lambda$2D___serialize__,
    lambda$2D___deserialize__,
    lambda$2D___bool__,
    lambda$2D___str__,
    lambda$2D___call__
};
struct lambda$3G_class lambda$3G_methods = {
    0,
    "lambda$3",
    UNASSIGNED,
    ($SuperG_class)&$ContG_methods,
    lambda$3D___init__,
    lambda$3D___serialize__,
    lambda$3D___deserialize__,
    lambda$3D___bool__,
    lambda$3D___str__,
    lambda$3D___call__
};
struct PingpongG_class PingpongG_methods = {
    0,
    "Pingpong",
    UNASSIGNED,
    ($SuperG_class)&$ActorG_methods,
    PingpongD___init__,
    PingpongD___serialize__,
    PingpongD___deserialize__,
    PingpongD___bool__,
    PingpongD___str__,
    Pingpong$ping,
    Pingpong$pong
};

$R $ROOT(B_Env env, $Cont then) {
    $register(&lambda$1G_methods);
    $register(&lambda$2G_methods);
    $register(&PingpongG_methods);
    return PingpongG_new(B_intG_new(B_listD_getitem(env->args,1)), then);
}

