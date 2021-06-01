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
  numpy$$ndarray a = numpy$$arange(to$int(0),to$int(60),to$int(1));
  $printobj("a.shape =",a->shape);
  $printobj("a.strides =",a->strides);
  $printobj("a=",a);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(4));
  $list_append(newshape,to$int(5));
  numpy$$ndarray b = numpy$$reshape(a,newshape);
  //$printobj("b.shape =",b->shape);
  //$printobj("b.strides =",b->strides);
  $printobj("b =",b);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,numpy$$ndslice$new($NEW($slice,to$int(2),NULL,to$int(3))));
  numpy$$ndarray c = numpy$$ndarray$__ndgetslice__(a,ix);
  $printobj("c.shape =",c->shape);
  $printobj("c.strides =",c->strides);
  $printobj("c =",c);
  $list newshape2 = $NEW($list,NULL,NULL);
  $list_append(newshape2,to$int(4));
  $list_append(newshape2,to$int(5));
  numpy$$ndarray d = numpy$$reshape(c,newshape2);
  $printobj("d.shape =",d->shape);
  $printobj("d.strides =",d->strides);
  $printobj("d =",d);
  $list newshape3 = $NEW($list,NULL,NULL);
  $list_append(newshape3,to$int(20));
  numpy$$ndarray e = numpy$$reshape(d,newshape3);
  $printobj("e.shape =\n",e->shape);
  $printobj("e.strides =",e->strides);
  $printobj("e =",e);
  $list newshape4 = $NEW($list,NULL,NULL);
  $list_append(newshape4,to$int(60));
  numpy$$ndarray f = numpy$$reshape(b,newshape4);
  $printobj("f.shape =",f->shape);
  $printobj("f.strides =",f->strides);
  $printobj("f =",f);

}
