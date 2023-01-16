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

#include "../../builtin/builtin.h"
#include "../numpy.h"

int main() {
  $register_builtin();
  numpyQ___init__();
  numpyQ_ndarray a = numpyQ_arange(toB_int(0),toB_int(60),toB_int(1));
  B_printobj("a.shape =",a->shape);
  B_printobj("a.strides =",a->strides);
  B_printobj("a=",a);
  B_list newshape = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape,toB_int(3));
  B_listD_append(newshape,toB_int(4));
  B_listD_append(newshape,toB_int(5));
  numpyQ_ndarray b = numpyQ_reshape(a,newshape);
  //B_printobj("b.shape =",b->shape);
  //B_printobj("b.strides =",b->strides);
  B_printobj("b =",b);
  B_list ix = $NEW(B_list,NULL,NULL);
  B_listD_append(ix,numpyQ_ndsliceG_new($NEW(B_slice,toB_int(2),NULL,toB_int(3))));
  numpyQ_ndarray c = numpyQ_ndarrayD___ndgetslice__(a,ix);
  B_printobj("c.shape =",c->shape);
  B_printobj("c.strides =",c->strides);
  B_printobj("c =",c);
  B_list newshape2 = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape2,toB_int(4));
  B_listD_append(newshape2,toB_int(5));
  numpyQ_ndarray d = numpyQ_reshape(c,newshape2);
  B_printobj("d.shape =",d->shape);
  B_printobj("d.strides =",d->strides);
  B_printobj("d =",d);
  B_list newshape3 = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape3,toB_int(20));
  numpyQ_ndarray e = numpyQ_reshape(d,newshape3);
  B_printobj("e.shape =\n",e->shape);
  B_printobj("e.strides =",e->strides);
  B_printobj("e =",e);
  B_list newshape4 = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape4,toB_int(60));
  numpyQ_ndarray f = numpyQ_reshape(b,newshape4);
  B_printobj("f.shape =",f->shape);
  B_printobj("f.strides =",f->strides);
  B_printobj("f =",f);

}
