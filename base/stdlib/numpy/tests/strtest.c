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
  B_listD_append(elems,to$float(2.0));
  B_listD_append(elems,to$float(3.0));
  B_listD_append(elems,to$float(5.0));
  B_listD_append(elems,to$float(7.0));
  numpyQ_ndarray q = numpyQ_ndarray_array((numpyQ_Primitive)numpyQ_PrimitiveD_floatG_witness,elems);
  B_printobj("q =",q);
  numpyQ_ndarray a = numpyQ_ndarray_arange(to$int(0),to$int(10),to$int(1));
  B_printobj("a =",a);
  B_list ix = $NEW(B_list,NULL,NULL);
  B_listD_append(ix,to$int(2));
  B_listD_append(ix,to$int(5));
  B_printobj("a.reshape(2,5)=",numpyQ_ndarray_reshape(a,ix));
  numpyQ_ndarray b = numpyQ_ndarray_linspace(to$float(0),to$float(1),to$int(5));
  B_printobj("b =",b);
  
  B_list ix1 = $NEW(B_list,NULL,NULL);
  B_listD_append(ix1,numpyQ_ndindexG_new(to$int(7)));
  numpyQ_ndarray c = numpyQ_ndarrayD___ndgetslice__(a,ix1);
  B_printobj("a[7] =",c);
  numpyQ_ndarray d = numpyQ_ndarray_fromatom(to$float(3.5));
  B_printobj("d =",d);
}

