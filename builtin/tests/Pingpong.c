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

#include "Pingpong.h"

void lambda$1D___init__(lambda$1 $this, Pingpong self, B_int count) {
    $this->self = self;
    $this->count = count;
    printf("BBB\n");
}
void lambda$1D___serialize__(lambda$1 $this, B_NoneType state) {
    $step_serialize($this->self,state);
    $step_serialize($this->count,state);
}

lambda$1 lambda$1D___deserialize__(B_NoneType state) {
    lambda$1 res = $DNEW(lambda$1,state);
    res->self = (Pingpong)$step_deserialize(state);
    res->count = (B_int)$step_deserialize(state);
    return res;
}
$R lambda$1D___call__(lambda$1 $this, $Cont then) {
    Pingpong self = $this->self;
    B_int count = $this->count;
    return self->$class->pong(self, B_IntegralD_intG_witness->$class->__neg__(B_IntegralD_intG_witness, count), then);
}

void lambda$2D___init__(lambda$2 $this, Pingpong self) {
    $this->self = self;
}
void lambda$2D___serialize__(lambda$2 $this, B_NoneType state) {
    $step_serialize($this->self,state);
}
lambda$2 lambda$2D___deserialize__(B_NoneType state) {
    lambda$2 res = $DNEW(lambda$2,state);
    res->self = (Pingpong)$step_deserialize(state);
    return res;
}
$R lambda$2D___call__(lambda$2 $this, $Cont then) {
    Pingpong self = $this->self;
    return self->$class->ping(self, then);
}

$R PingpongD___init__(Pingpong self, B_Env env, $Cont then) {
    $ActorG_methods.__init__(($Actor)self);
    self->i = to$int(7);
    self->count = to$int(0);
    return self->$class->ping(self, then);
}
$R Pingpong$ping(Pingpong self, $Cont then) {
    self->count = B_IntegralD_intG_witness->$class->__add__(B_IntegralD_intG_witness, self->count, to$int(1));
    printf("%ld Ping %ld\n", self->i->val, self->count->val);
    $AFTER(to$int(1), ($Cont)$NEW(lambda$1, self, self->count));
    printf("AAA\n");
    return $R_CONT(then, B_None);
}
void PingpongD___serialize__(Pingpong self, B_NoneType state) {
    $step_serialize(self->i,state);
    $step_serialize(self->count,state);
}
Pingpong PingpongD___deserialize__(B_NoneType state) {
    Pingpong res = $DNEW(Pingpong,state);
    res->i = (B_int)$step_deserialize(state);
    res->count = (B_int)$step_deserialize(state);
    return res;
}
$R Pingpong$pong(Pingpong self, B_int q, $Cont then) {
    printf("%ld     %ld Pong\n", self->i->val, q->val);
    $AFTER(to$int(2), ($Cont)$NEW(lambda$2, self));
    return $R_CONT(then, B_None);
}

struct lambda$1G_class lambda$1G_methods = {
    0,
    "lambda$1",
    NULL,
    lambda$1D___init__,
    lambda$1D___serialize__,
    lambda$1D___deserialize__,
    lambda$1D___call__
};
struct lambda$2G_class lambda$2G_methods = {
    0,
    "lambda$2",
    NULL,
    lambda$2D___init__,
    lambda$2D___serialize__,
    lambda$2D___deserialize__,
    lambda$2D___call__
};
struct PingpongG_class PingpongG_methods = {
    0,
    "Pingpong",
    NULL,
    PingpongD___init__,
    PingpongD___serialize__,
    PingpongD___deserialize__,
    Pingpong$ping,
    Pingpong$pong
};

$R PingpongG_new(B_Env env, $Cont cont) {
    Pingpong $tmp = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct Pingpong), PingpongG_methods.$GCdescr);
    $tmp->$class = &PingpongG_methods;
    return PingpongG_methods.__init__($tmp, env, $CONSTCONT($tmp, cont));
}

$R $ROOT (B_Env env, $Cont cont) {
    $register(&lambda$1G_methods);
    $register(&lambda$2G_methods);
    $register(&PingpongG_methods);
    return PingpongG_new(env, cont);
}
