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
#include "../ndarray.h"

int main(int argc, char *argv[]) {
  long n;
  sscanf(argv[1],"%ld",&n);
  $ndarray x = $ndarray_linspace(to$float(0.0),to$float(1.0), toB_int(n));
  //$ndarray x = $ndarray_arange(toB_int(n*n));
  printf("x=%s\n",x->$class->__str__(x)->str);
  B_list newshape = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape,toB_int(n));
  //B_listD_append(newshape,toB_int(n));
  //B_listD_append(newshape,toB_int(n));
  //B_listD_append(newshape,toB_int(n));
  $ndarray a = $ndarray_reshape(x,newshape);
  //printf("a.shape = %s\n",a->shape->$class->__str__(a->shape)->str);
  //printf("a.strides = %s\n",a->strides->$class->__str__(a->strides)->str);
  //printf("a=%s\n",x->$class->__str__(a)->str);
  B_value s = (B_value)$ndarray_sum(a);
  printf("sum(a) = %s\n",s->$class->__str__(s)->str);
}
