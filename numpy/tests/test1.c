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
  long n;
  long iters;
  sscanf(argv[1],"%ld",&n);
  sscanf(argv[2],"%ld",&iters);
  for (int i=0;i<iters; i++) {
    numpy$$ndarray x = numpy$$linspace(to$float((double)i),to$float((double)i+1), to$int(n));
    //printf("x=%s\n",x->$class->__str__(x)->str);
    $list ix = $NEW($list,NULL,NULL);
    $slice s = $NEW($slice,NULL,NULL,NULL);
    $list_append(ix,numpy$$ndslice$new(s));
    $list_append(ix,numpy$$ndindex$new(numpy$$newaxis));
    numpy$$Integral$ndarray wit = $NEW(numpy$$Integral$ndarray,(numpy$$Primitive)numpy$$Primitive$float$witness);
    numpy$$ndarray r = wit->$class->__add__(wit,numpy$$ndarray$__ndgetslice__(x,ix),x);
    //printf("r->shape=%s\n",r->shape->$class->__str__(r->shape)->str);
    //printf("r->strides=%s\n",r->strides->$class->__str__(r->strides)->str);
    //printf("r=%s\n",r->$class->__str__(r)->str);
    $value sm = ($value)numpy$$sum((numpy$$Primitive)numpy$$Primitive$float$witness,r,NULL);
    $printobj("sum(r) =",sm);
    free(r->data);
    free(r);
  }
}
        
