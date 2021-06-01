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
  $list elems = $NEW($list,NULL,NULL);
  $list_append(elems,to$float(2.0));
  $list_append(elems,to$float(3.0));
  $list_append(elems,to$float(5.0));
  $list_append(elems,to$float(7.0));
  numpy$$ndarray q = numpy$$ndarray_array((numpy$$Primitive)numpy$$Primitive$float$witness,elems);
  $printobj("q =",q);
  numpy$$ndarray a = numpy$$ndarray_arange(to$int(0),to$int(10),to$int(1));
  $printobj("a =",a);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,to$int(2));
  $list_append(ix,to$int(5));
  $printobj("a.reshape(2,5)=",numpy$$ndarray_reshape(a,ix));
  numpy$$ndarray b = numpy$$ndarray_linspace(to$float(0),to$float(1),to$int(5));
  $printobj("b =",b);
  
  $list ix1 = $NEW($list,NULL,NULL);
  $list_append(ix1,numpy$$ndindex$new(to$int(7)));
  numpy$$ndarray c = numpy$$ndarray$__ndgetslice__(a,ix1);
  $printobj("a[7] =",c);
  numpy$$ndarray d = numpy$$ndarray_fromatom(to$float(3.5));
  $printobj("d =",d);
}

