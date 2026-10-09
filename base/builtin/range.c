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

static struct B_ValueError B_range_zero_step_error =
    STATIC_EXCEPTION(B_ValueError, "range() step size must not be zero");

B_range B_rangeG_new(int64_t start, B_int stop, int64_t step) {
    return $NEW(B_range, start, stop, step);
}

void $rangeD_U_init(B_range self, int64_t start, int64_t stop, int64_t step) {
    if (step == 0) {
        RAISE_EXC(&B_range_zero_step_error);
    }

    self->$class = &B_rangeG_methods;
    self->step = step;
    int64_t distance = stop - start;
    self->nxt = start - step; // __next__ will add step
    self->remaining = step > 0
        ? (distance > 0 ? distance / step + (distance % step != 0) : 0)
        : (distance < 0 ? distance / step + (distance % step != 0) : 0);
}

B_range $rangeD_U_new(int64_t start, int64_t stop, int64_t step) {
    B_range self = acton_malloc(sizeof(struct B_range));
    $rangeD_U_init(self, start, stop, step);
    return self;
}

B_NoneType B_rangeD___init__(B_range self, int64_t start, B_int stop, int64_t step) {
    int64_t ustart, ustop, ustep;
    if (stop) {
        ustart = start;
        ustop = stop->val;
    } else {
        ustart = 0;
        ustop = start;
    }
    ustep = step;
    $rangeD_U_init(self, ustart, ustop, ustep);
    return B_None;
}

bool $rangeD_U__next_i64(B_range self, int64_t *out) {
    if (self->remaining-- <= 0)
        return false;
    *out = self->nxt += self->step;
    return true;
}

bool B_rangeD___next__(B_range self, $WORD *out) {
    int64_t value;
    if (!$rangeD_U__next_i64(self, &value))
        return false;
    *out = (B_value)toB_int(value);
    return true;
}

bool B_rangeD___bool__(B_range self) {
    return true;
}

B_str B_rangeD___repr__(B_range self) {
    return $FORMAT("<range object at %p>", self);
}

B_str B_rangeD___str__(B_range self) {
    return $FORMAT("<range object at %p>", self);
}

void B_rangeD___serialize__(B_range self, $Serial$state state) {
    $step_serialize(toB_int(self->nxt),state);
    $step_serialize(toB_int(self->step),state);
    $step_serialize(toB_int(self->remaining),state);
}

B_range B_rangeD___deserialize__(B_range self, $Serial$state state) {
    B_range res = $DNEW(B_range,state);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    res->step = fromB_int((B_int)$step_deserialize(state));
    res->remaining = fromB_int((B_int)$step_deserialize(state));
    return res;
}
