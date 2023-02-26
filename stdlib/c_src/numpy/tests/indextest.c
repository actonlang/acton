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
  numpyQ_ndarray v = numpyQ_arange(toB_int(0),toB_int(60),toB_int(1));
  B_list newshape = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape,toB_int(3));
  B_listD_append(newshape,toB_int(4));
  B_listD_append(newshape,toB_int(5));
  numpyQ_ndarray a = numpyQ_reshape(v,newshape);
  B_printobj("a=",a);
  B_list ix = $NEW(B_list,NULL,NULL);
  B_listD_append(ix,numpyQ_ndindexG_new(numpy$G_newaxis));
  B_slice s1 = $NEW(B_slice,NULL,NULL,NULL);
  B_listD_append(ix,numpyQ_ndsliceG_new(s1));
  B_listD_append(ix,numpyQ_ndindexG_new(toB_int(2)));
  numpyQ_ndarray b = numpyQ_ndarrayD___ndgetslice__(a,ix);
  B_printobj("b=",b);
  B_list ix1 = $NEW(B_list,NULL,NULL);
  B_listD_append(ix1,numpyQ_ndindexG_new(toB_int(1)));
  B_slice s2 = $NEW(B_slice,toB_int(1),NULL,NULL);
  B_listD_append(ix1,numpyQ_ndsliceG_new(s2));
  B_slice s3 = $NEW(B_slice,toB_int(-1),toB_int(0),toB_int(-1));
  B_listD_append(ix1,numpyQ_ndsliceG_new(s3));
  numpyQ_ndarray c = numpyQ_ndarrayD___ndgetslice__(a,ix1);
  B_printobj("c=",c);
  B_list test = B_listD_copy(c->shape);
  numpyQ_Primitive wit = (numpyQ_Primitive)numpyQ_PrimitiveD_intG_witness;
  numpyQ_ndarray d = numpyQ_abs(wit,c);
  B_printobj("d=",d);
  numpyQ_ndarray e = numpyQ_fromatom(($Super)toB_int(3));
  B_printobj("e.shape =",e->shape);
  B_printobj("e.strides =",e->strides);
  B_printobj("e=",e);
  numpyQ_IntegralD_ndarray wit2 = $NEW(numpyQ_IntegralD_ndarray,wit);
  numpyQ_ndarray f = wit2->$class->__add__(wit2,d,e);
  B_printobj("f=",f);  
  numpyQ_ndarray g = wit2->$class->__add__(wit2,a,e);
  B_printobj("g.shape =",g->shape);
  B_printobj("g.strides",g->strides);
  B_printobj("g=",g);  
}
