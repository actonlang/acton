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
  numpy$$__init__();
  numpy$$ndarray v = numpy$$arange(to$int(0),to$int(60),to$int(1));
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(4));
  $list_append(newshape,to$int(5));
  numpy$$ndarray a = numpy$$reshape(v,newshape);
  $printobj("a=",a);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,numpy$$ndindex$new(numpy$$newaxis));
  $slice s1 = $NEW($slice,NULL,NULL,NULL);
  $list_append(ix,numpy$$ndslice$new(s1));
  $list_append(ix,numpy$$ndindex$new(to$int(2)));
  numpy$$ndarray b = numpy$$ndarray$__ndgetslice__(a,ix);
  $printobj("b=",b);
  $list ix1 = $NEW($list,NULL,NULL);
  $list_append(ix1,numpy$$ndindex$new(to$int(1)));
  $slice s2 = $NEW($slice,to$int(1),NULL,NULL);
  $list_append(ix1,numpy$$ndslice$new(s2));
  $slice s3 = $NEW($slice,to$int(-1),to$int(0),to$int(-1));
  $list_append(ix1,numpy$$ndslice$new(s3));
  numpy$$ndarray c = numpy$$ndarray$__ndgetslice__(a,ix1);
  $printobj("c=",c);
  $list test = $list_copy(c->shape);
  numpy$$Primitive wit = (numpy$$Primitive)numpy$$Primitive$int$witness;
  numpy$$ndarray d = numpy$$abs(wit,c);
  $printobj("d=",d);
  numpy$$ndarray e = numpy$$fromatom(($Super)to$int(3));
  $printobj("e.shape =",e->shape);
  $printobj("e.strides =",e->strides);
  $printobj("e=",e);
  numpy$$Integral$ndarray wit2 = $NEW(numpy$$Integral$ndarray,wit);
  numpy$$ndarray f = wit2->$class->__add__(wit2,d,e);
  $printobj("f=",f);  
  numpy$$ndarray g = wit2->$class->__add__(wit2,a,e);
  $printobj("g.shape =",g->shape);
  $printobj("g.strides",g->strides);
  $printobj("g=",g);  
}
