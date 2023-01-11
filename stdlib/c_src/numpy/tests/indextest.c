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
  numpy$D___init__();
  numpy$$ndarray v = numpy$$arange(toB_int(0),toB_int(60),toB_int(1));
  B_list newshape = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape,toB_int(3));
  B_listD_append(newshape,toB_int(4));
  B_listD_append(newshape,toB_int(5));
  numpy$$ndarray a = numpy$$reshape(v,newshape);
  $printobj("a=",a);
  B_list ix = $NEW(B_list,NULL,NULL);
  B_listD_append(ix,numpy$$ndindexG_new(numpy$G_newaxis));
  B_slice s1 = $NEW(B_slice,NULL,NULL,NULL);
  B_listD_append(ix,numpy$$ndsliceG_new(s1));
  B_listD_append(ix,numpy$$ndindexG_new(toB_int(2)));
  numpy$$ndarray b = numpy$$ndarrayD___ndgetslice__(a,ix);
  $printobj("b=",b);
  B_list ix1 = $NEW(B_list,NULL,NULL);
  B_listD_append(ix1,numpy$$ndindexG_new(toB_int(1)));
  B_slice s2 = $NEW(B_slice,toB_int(1),NULL,NULL);
  B_listD_append(ix1,numpy$$ndsliceG_new(s2));
  B_slice s3 = $NEW(B_slice,toB_int(-1),toB_int(0),toB_int(-1));
  B_listD_append(ix1,numpy$$ndsliceG_new(s3));
  numpy$$ndarray c = numpy$$ndarrayD___ndgetslice__(a,ix1);
  $printobj("c=",c);
  B_list test = B_listD_copy(c->shape);
  numpy$$Primitive wit = (numpy$$Primitive)numpy$$PrimitiveB_intG_witness;
  numpy$$ndarray d = numpy$$abs(wit,c);
  $printobj("d=",d);
  numpy$$ndarray e = numpy$$fromatom(($Super)toB_int(3));
  $printobj("e.shape =",e->shape);
  $printobj("e.strides =",e->strides);
  $printobj("e=",e);
  numpy$B_IntegralD_ndarray wit2 = $NEW(numpy$B_IntegralD_ndarray,wit);
  numpy$$ndarray f = wit2->$class->__add__(wit2,d,e);
  $printobj("f=",f);  
  numpy$$ndarray g = wit2->$class->__add__(wit2,a,e);
  $printobj("g.shape =",g->shape);
  $printobj("g.strides",g->strides);
  $printobj("g=",g);  
}
