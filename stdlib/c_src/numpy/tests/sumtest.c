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

int main(int argc, char *argv[]) {
  $register_builtin();
  numpy$D___init__();
  long n;
  sscanf(argv[1],"%ld",&n);
  numpyQ_ndarray x = numpyQ_ndarray_linspace(to$float(0.0),to$float(1.0), toB_int(n*n*n*n));
  numpyQ_ndarray y = numpyQ_ndarray_arange(toB_int(0),toB_int(n*n*n*n),toB_int(1));
  //B_printobj("x =",x);
  B_list newshape = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape,toB_int(n));
  B_listD_append(newshape,toB_int(n));
  B_listD_append(newshape,toB_int(n));
  B_listD_append(newshape,toB_int(n));
  numpyQ_ndarray a = numpyQ_ndarray_reshape(x,newshape);
  numpyQ_ndarray b = numpyQ_ndarray_reshape(y,newshape);
  //B_printobj("a =",a);
  //for (int i = 0; i<100; i++) {
    B_value s = (B_value)numpyQ_ndarray_sum((numpyQ_Primitive)numpyQ_PrimitiveB_floatG_witness,a,NULL);
    B_printobj("sum(a) =",s);
    B_value t = (B_value)numpyQ_ndarray_sum((numpyQ_Primitive)numpyQ_PrimitiveB_intG_witness,b,NULL);
    //B_printobj("b =",b);  
    B_printobj("sum(b) =",t);
    //  }
}
