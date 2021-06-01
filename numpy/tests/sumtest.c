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
  numpy$$__init__();
  long n;
  sscanf(argv[1],"%ld",&n);
  numpy$$ndarray x = numpy$$ndarray_linspace(to$float(0.0),to$float(1.0), to$int(n*n*n*n));
  numpy$$ndarray y = numpy$$ndarray_arange(to$int(0),to$int(n*n*n*n),to$int(1));
  //$printobj("x =",x);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  numpy$$ndarray a = numpy$$ndarray_reshape(x,newshape);
  numpy$$ndarray b = numpy$$ndarray_reshape(y,newshape);
  //$printobj("a =",a);
  //for (int i = 0; i<100; i++) {
    $value s = ($value)numpy$$ndarray_sum((numpy$$Primitive)numpy$$Primitive$float$witness,a,NULL);
    $printobj("sum(a) =",s);
    $value t = ($value)numpy$$ndarray_sum((numpy$$Primitive)numpy$$Primitive$int$witness,b,NULL);
    //$printobj("b =",b);  
    $printobj("sum(b) =",t);
    //  }
}
