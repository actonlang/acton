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
  B_list elems = $NEW(B_list,NULL,NULL);
  B_listD_append(elems,toB_float(2.0));
  B_listD_append(elems,toB_float(3.0));
  B_listD_append(elems,toB_float(5.0));
  B_listD_append(elems,toB_float(7.0));
  numpy$$ndarray q = numpy$$ndarray_array((numpy$$Primitive)numpy$$PrimitiveB_floatG_witness,elems);
  $printobj("q =",q);
  numpy$$ndarray a = numpy$$ndarray_arange(toB_int(0),toB_int(10),toB_int(1));
  $printobj("a =",a);
  B_list ix = $NEW(B_list,NULL,NULL);
  B_listD_append(ix,toB_int(2));
  B_listD_append(ix,toB_int(5));
  $printobj("a.reshape(2,5)=",numpy$$ndarray_reshape(a,ix));
  numpy$$ndarray b = numpy$$ndarray_linspace(toB_float(0),toB_float(1),toB_int(5));
  $printobj("b =",b);
  
  B_list ix1 = $NEW(B_list,NULL,NULL);
  B_listD_append(ix1,numpy$$ndindexG_new(toB_int(7)));
  numpy$$ndarray c = numpy$$ndarrayD___ndgetslice__(a,ix1);
  $printobj("a[7] =",c);
  numpy$$ndarray d = numpy$$ndarray_fromatom(toB_float(3.5));
  $printobj("d =",d);
}

