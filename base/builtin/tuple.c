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

#include <stdarg.h>

B_NoneType B_tupleD___init__(B_tuple self,int size ,...) {
    va_list args;
    va_start(args,size);
    self->size = size;
    self->components = acton_malloc(size*sizeof($WORD));
    for (int i=0; i<size; i++)
        self->components[i] = va_arg(args,$WORD);
    va_end(args);
    return B_None;
}

void B_tupleD___serialize__(B_tuple self, $Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        $val_serialize(-TUPLE_ID,&prevkey->val,state);
        return;
    }
    B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,to$int(state->row_no));
    long len = (long)self->size;
    $val_serialize(TUPLE_ID,&len,state);
    for (int i=0; i<self->size; i++) {
        $step_serialize(self->components[i],state);
    }
}

B_tuple B_tupleD___deserialize__(B_tuple self, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return (B_tuple)B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,to$int((long)this->blob[0]),NULL);
    } else {
        int len = (int)(long)this->blob[0];
        B_tuple res = acton_malloc(sizeof(struct B_tuple));
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,to$int(state->row_no-1),res);
        res->components = acton_malloc(len * sizeof($WORD));
        res->$class = &B_tupleG_methods;
        res->size = len;
        for (int i = 0; i < len; i++) 
            res->components[i] = $step_deserialize(state);
        return res;
    }
}

struct B_tupleG_class B_tupleG_methods = {
    "tuple",
    UNASSIGNED,
    ($SuperG_class)&B_valueG_methods,
    B_tupleD___init__,
    B_tupleD___serialize__,
    B_tupleD___deserialize__,
};
 
