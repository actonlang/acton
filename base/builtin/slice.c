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


/* Normalize slice notation, so that
   - if step == 0, VALUEERROR is raised

   - Otherwise, 
   - on input, len must be the # of elements in the sequence being sliced
   - on output 
      - 0 <= *start < len is the starting position
      - 0 <= *stop < len is the ending position (*non-inclusive*!)
      - *step is the step size
      - *slen is the # of elements in the slice. 
*/

void normalize_slice(B_slice slc, long len, long *slen, long *start, long *stop, long *step) {
    if (slc->step == NULL)
        *step = 1;
    else
        *step = *slc->step;
    if (*step == 0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("step size 0 in slice")));
    }
    if (slc->start == NULL)
        *start = *step > 0 ? 0 : len-1;
    else {
        *start = *slc->start;
        *start = *start >=0 ? (*start < len  ? *start : len-1 + (*step > 0))
            : (*start > -len ? len+*start : 0);
    }
    if (slc->stop == NULL)
        *stop = *step > 0 ? len : -1;
    else {
        *stop = *slc->stop;
        *stop = *stop >= 0 ? (*stop < len  ? *stop : len-1 + (*step > 0)) 
            : (*stop > -len ? len+*stop : 0);
    }
  
    if ((*step > 0 && *start >= *stop) || (*step < 0 && *start <= *stop))
        *slen = 0;
    else
        *slen = (*stop-*start)/ *step + ((*stop-*start)%*step != 0);
}

B_slice B_sliceG_new(B_int start,B_int stop,B_int step) {
    return $NEW(B_slice,start,stop,step);
}

B_NoneType B_sliceD___init__(B_slice s, B_int start, B_int stop, B_int step) {
    if (start) {
        s->start = acton_malloc(sizeof(long));
        *s->start = from$int(start);
    } else
        s->start = NULL;
    if (stop) {
        s->stop = acton_malloc(sizeof(long));
        *s->stop = from$int(stop);
    } else
        s->stop = NULL;
    if (step) {
        s->step = acton_malloc(sizeof(long));
        *s->step = from$int(step);
    } else
        s->step = NULL;
    return B_None;
}

void B_sliceD___serialize__ (B_slice self, $Serial$state state) {
    $ROW row = $add_header(SLICE_ID,3,state);
    row->blob[0] = ($WORD)*self->start;
    row->blob[1] = ($WORD)*self->stop;
    row->blob[2] = ($WORD)*self->step;
}
B_slice B_sliceD___deserialize__ (B_slice self, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    B_slice res = acton_malloc(sizeof(struct B_slice));
    res->$class = &B_sliceG_methods;
    res->start = acton_malloc(sizeof(long));
    res->stop = acton_malloc(sizeof(long));
    res->step = acton_malloc(sizeof(long));
    *res->start = (long)this->blob[0];
    *res->stop = (long)this->blob[1];
    *res->step = (long)this->blob[2];
    return res;
}

B_bool B_BoolD_sliceD___bool__(B_slice s) {
    return B_True;
}

B_str B_StrD_sliceD___str__(B_slice s) {
    return $FORMAT("Slice [%ld:%ld:%ld]", *s->start, *s->stop, *s->step);
}

B_str B_StrD_sliceD___repr__(B_slice s) {
    return B_StrD_sliceD___str__(s);
}
