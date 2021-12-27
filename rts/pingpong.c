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

void lambda$1$__init__(lambda$1 $this, Pingpong self, $int count, $int q) {
    $this->self = self;
    $this->count = count;
    $this->q = q;
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
    $step_serialize(self->self,state);
    $step_serialize(self->count,state);
    $step_serialize(self->q,state);
}

lambda$1 lambda$1$__deserialize__(lambda$1 self, $Serial$state state) {
    lambda$1 res = $DNEW(lambda$1,state);
    res->self = $step_deserialize(state);
    res->count = $step_deserialize(state);
    res->q = $step_deserialize(state);
    return res;
}

$R lambda$1$__call__(lambda$1 $this, $Cont then) {
    Pingpong self = $this->self;
    $int count = $this->count;
    $int q = $this->q;
    return self->$class->pong(self, count, $Integral$int$witness->$class->__neg__($Integral$int$witness, q), then);
}

lambda$1 lambda$1$new(Pingpong self, $int count, $int q) {
    lambda$1 obj = malloc(sizeof(struct lambda$1));
    obj->$class = &lambda$1$methods;
    lambda$1$methods.__init__(obj, self, count, q);
    return obj;
}

////////////////////////////////////////////////////////////////////////////////////////

void lambda$2$__init__(lambda$2 $this, Pingpong self, $int q) {
    $this->self = self;
    $this->q = q;
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
    $step_serialize(self->q,state);
}


lambda$2 lambda$2$__deserialize__(lambda$2 self, $Serial$state state) {
    lambda$2 res = $DNEW(lambda$2,state);
    res->self = $step_deserialize(state);
    res->q = $step_deserialize(state);
    return res;
}


$R lambda$2$__call__ (lambda$2 $this, $Cont then) {
    Pingpong self = $this->self;
    $int q = $this->q;
    return self->$class->ping(self, q, then);
}

lambda$2 lambda$2$new(Pingpong self, $int q) {
    lambda$2 obj = malloc(sizeof(struct lambda$2));
    obj->$class = &lambda$2$methods;
    lambda$2$methods.__init__(obj, self, q);
    return obj;
}

////////////////////////////////////////////////////////////////////////////////////////

void lambda$3$__init__(lambda$3 $this, $Cont cont) {
    $this->cont = cont;
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
    $step_serialize(self->cont,state);
}

lambda$3 lambda$3$__deserialize__(lambda$3 self, $Serial$state state) {
    lambda$3 res = $DNEW(lambda$3,state);
    res->cont = $step_deserialize(state);
    return res;
}

$R lambda$3$__call__ (lambda$3 $this, $NoneType none) {
    $OLDACT();
    $Cont cont = $this->cont;
    return $R_CONT(cont, none);
}

lambda$3 lambda$3$new($Cont cont) {
    lambda$3 obj = malloc(sizeof(struct lambda$3));
    obj->$class = &lambda$3$methods;
    lambda$3$methods.__init__(obj, cont);
    return obj;
}

////////////////////////////////////////////////////////////////////////////////////////

$R Pingpong$__init__(Pingpong self, $int i, $Cont then) {
    $Actor$methods.__init__(($Actor)self);
    $NEWACT(($Actor)self);
    self->i = i;
    self->count = i;
    return self->$class->ping(self, i, ($Cont)lambda$3$new(then));
}

$bool Pingpong$__bool__(Pingpong self) {
    return $True;
}

$str Pingpong$__str__(Pingpong self) {
    char *s;
    asprintf(&s,"<Pingpong object at %p>",self);
    return to$str(s);
}
void Pingpong$__serialize__(Pingpong self, $Serial$state state) {
    $Actor$methods.__serialize__(($Actor)self, state);
    $step_serialize(self->i,state);
    $step_serialize(self->count,state);
}

Pingpong Pingpong$__deserialize__(Pingpong res, $Serial$state state) {
    if (!res)
        res = $DNEW(Pingpong,state);
    $Actor$methods.__deserialize__(($Actor)res, state);
    res->i = $step_deserialize(state);
    res->count = $step_deserialize(state);
    return res;
}

$R Pingpong$ping(Pingpong self, $int q, $Cont then) {
    self->count = $Integral$int$witness->$class->__add__($Integral$int$witness, self->count, to$int(1));
    $int j = $Integral$int$witness->$class->__mul__($Integral$int$witness, self->count, q);
    $print(1, $FORMAT("%ld Ping %8ld", self->i->val, j->val));
    $AFTER(to$int(1), ($Cont)lambda$1$new(self, self->count, q));
    return $R_CONT(then, $None);
}
$R Pingpong$pong(Pingpong self, $int n, $int q, $Cont then) {
    $int j = $Integral$int$witness->$class->__mul__($Integral$int$witness, n, q);
    $print(1, $FORMAT("%ld       %7ld Pong", self->i->val, j->val));
    $AFTER(to$int(2), ($Cont)lambda$2$new(self, $Integral$int$witness->$class->__neg__($Integral$int$witness, q)));
    return $R_CONT(then, $None);
}

$R Pingpong$new($int i, $Cont then) {
    Pingpong obj = malloc(sizeof(struct Pingpong));
    obj->$class = &Pingpong$methods;
    return Pingpong$methods.__init__(obj, i, $CONSTCONT(obj,then));
}

////////////////////////////////////////////////////////////////////////////////////////

struct lambda$1$class lambda$1$methods = {
    "lambda$1",
    UNASSIGNED,
    ($Super$class)&$Cont$methods,
    lambda$1$__init__,
    lambda$1$__serialize__,
    lambda$1$__deserialize__,
    lambda$1$__bool__,
    lambda$1$__str__,
    lambda$1$__call__
};
struct lambda$2$class lambda$2$methods = {
    "lambda$2",
    UNASSIGNED,
    ($Super$class)&$Cont$methods,
    lambda$2$__init__,
    lambda$2$__serialize__,
    lambda$2$__deserialize__,
    lambda$2$__bool__,
    lambda$2$__str__,
    lambda$2$__call__
};
struct lambda$3$class lambda$3$methods = {
    "lambda$3",
    UNASSIGNED,
    ($Super$class)&$Cont$methods,
    lambda$3$__init__,
    lambda$3$__serialize__,
    lambda$3$__deserialize__,
    lambda$3$__bool__,
    lambda$3$__str__,
    lambda$3$__call__
};
struct Pingpong$class Pingpong$methods = {
    "Pingpong",
    UNASSIGNED,
    ($Super$class)&$Actor$methods,
    Pingpong$__init__,
    Pingpong$__serialize__,
    Pingpong$__deserialize__,
    Pingpong$__bool__,
    Pingpong$__str__,
    Pingpong$ping,
    Pingpong$pong
};

$R $ROOT($Env env, $Cont then) {
    $register(&lambda$1$methods);
    $register(&lambda$2$methods);
    $register(&Pingpong$methods);
    return Pingpong$new($int$new($list_getitem(env->args,1)), then);
}

