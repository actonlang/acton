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
    if (stop) {
        self->start = from$int(start);
        self->stop = from$int(stop);
    } else {
        self->start = 0;
        self->stop = from$int(start);
    }
    if (step) {
        int stp = from$int(step);
        if (stp==0) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("step size zero in range")));
        }
        else
            self->step = stp;
    } else
        self->step = 1;
    return B_None;
}


B_bool B_rangeD___bool__(B_range self) {
    return toB_bool ((self->step > 0 && self->stop > self->start) ||
                    (self->start > self->stop));
}

B_str B_rangeD___str__(B_range self) {
    char *s;
    asprintf(&s,"range(%ld,%ld,%ld)",self->start,self->stop,self->step);
    return to$str(s);
}

B_str B_rangeD___repr__(B_range self) {
    char *s;
    asprintf(&s,"range(%ld,%ld,%ld)",self->start,self->stop,self->step);
    return to$str(s);
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
    B_range res = malloc(sizeof(struct B_range));
    res->$class = &B_rangeG_methods;
    res->start = (long)this->blob[0];
    res->stop = (long)this->blob[1];
    res->step = (long)this->blob[2];
    return res;
}

static $WORD B_IteratorB_rangeD_next(B_IteratorB_range self) {
    long res = self->src->start + self->nxt++*self->src->step;
    if (self->src->step>0)
        return res < self->src->stop ? to$int(res) : NULL;
    else
        return res > self->src->stop ? to$int(res) : NULL;
}

void B_IteratorB_rangeD_init(B_IteratorB_range self, B_range rng) {
    self->src = rng;
    self->nxt = 0;
}                                    

B_bool B_IteratorB_rangeD_bool(B_IteratorB_range self) {
    return B_True;
}

B_str B_IteratorB_rangeD_str(B_IteratorB_range self) {
    char *s;
    asprintf(&s,"<range iterator object at %p>",self);
    return to$str(s);
}

void B_IteratorB_rangeD_serialize(B_IteratorB_range self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}

B_IteratorB_range B_IteratorB_range$_deserialize(B_IteratorB_range self, $Serial$state state) {
    B_IteratorB_range res = $DNEW(B_IteratorB_range,state);
    res->src = (B_range)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}

struct B_IteratorB_rangeG_class B_IteratorB_rangeG_methods = {
    "B_IteratorB_range",
    UNASSIGNED,
    ($SuperG_class)&B_IteratorG_methods,
    B_IteratorB_rangeD_init,
    B_IteratorB_rangeD_serialize,
    B_IteratorB_range$_deserialize,
    B_IteratorB_rangeD_bool,
    B_IteratorB_rangeD_str,
    B_IteratorB_rangeD_str,
    B_IteratorB_rangeD_next
};

/*
struct B_IterableD_range B_IterableD_rangeG_instance = {&B_IterableD_rangeG_methods};
B_IterableD_range B_IterableD_rangeG_witness = &B_IterableD_rangeG_instance;
*/
//B_Iterator B_rangeD_iter(B_range rng) {
//  return (B_Iterator)$NEW(B_IteratorB_range,rng);
//}

 
B_Iterator B_IterableD_rangeD___iter__ (B_IterableD_range wit, B_range rng) {
    return (B_Iterator)$NEW(B_IteratorB_range,rng);
}
