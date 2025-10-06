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

B_range B_rangeG_new(B_int start, B_int stop, B_int step) {
    return $NEW(B_range, start, stop, step);
}


B_NoneType B_rangeD___init__(B_range self, B_int start, B_int stop, B_int step) {
    int64_t ustart, ustop, ustep, stp;
    if (stop) {
        ustart = start->val;
        ustop = stop->val;
    } else {
        ustart = 0;
        ustop = start->val;
    }
    if (step) {
        stp = step->val;
        if (stp == 0) {
            $RAISE((B_BaseException)$NEW(B_ValueError, to$str("range() step size must not be zero")));
        } else {
            ustep = stp;
        }
    } else {
        ustep = 1;
    }
    stp = self->step = ustep;
    int64_t r = ustop - ustart;
    self->nxt = ustart - stp;
    self->remaining = r/stp + (r%stp != 0);
    return B_None;
}

/*
B_bool B_rangeD___bool__(B_range self) {
    return toB_bool ((self->step > 0 && self->stop > self->start) ||
                    (self->start > self->stop));
}

B_str B_rangeD___str__(B_range self) {
    return $FORMAT("range(%ld,%ld,%ld)", self->start, self->stop, self->step);
}

B_str B_rangeD___repr__(B_range self) {
    return $FORMAT("range(%ld,%ld,%ld)", self->start, self->stop, self->step);
}

void B_rangeD___serialize__(B_range self, $Serial$state state) {
    $ROW row = $add_header(RANGE_ID,3,state);
    row->blob[0] = ($WORD)self->start;
    row->blob[1] = ($WORD)self->stop;
    row->blob[2] = ($WORD)self->step;
}

B_range B_rangeD___deserialize__(B_range self, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    B_range res = acton_malloc(sizeof(struct B_range));
    res->$class = &B_rangeG_methods;
    res->start = (int64_t)this->blob[0];
    res->stop = (int64_t)this->blob[1];
    res->step = (int64_t)this->blob[2];
    return res;
}
*/
int64_t rangeD_U__next__(B_range self) {
    if (self->remaining-- <= 0)
        $RAISE ((B_BaseException)$NEW(B_StopIteration, to$str("range iterator terminated")));
    return self->nxt += self->step;
}

B_int B_rangeD___next__(B_range self) {
    return toB_int(rangeD_U__next__(self));
}
/*
void B_IteratorD_rangeD_init(B_IteratorD_range self, B_range rng) {
    int64_t stp = self->step = rng->step;
    int64_t r = rng->stop - rng->start;
    self->nxt = rng->start - stp;
    self->remaining = r/stp + (r%stp != 0);
    self->box = rng->box;
}                                    
*/


B_bool B_rangeD___bool__(B_range self) {
    return B_True;
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
/*
struct B_IteratorD_rangeG_class B_IteratorD_rangeG_methods = {
    "B_IteratorD_range",
    UNASSIGNED,
    ($SuperG_class)&B_IteratorG_methods,
    B_IteratorD_rangeD_init,
    B_IteratorD_rangeD_serialize,
    B_IteratorD_range$_deserialize,
    B_IteratorD_rangeD_bool,
    B_IteratorD_rangeD_str,
    B_IteratorD_rangeD_str,
    B_IteratorD_rangeD_next
};

B_Iterator B_IterableD_rangeD___iter__ (B_IterableD_range wit, B_range rng) {
    return (B_Iterator)$NEW(B_IteratorD_range,rng);
}
*/
