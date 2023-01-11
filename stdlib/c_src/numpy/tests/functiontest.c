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
  numpy$$ndarray v = numpy$$ndarray_arange(toB_int(0),toB_int(60),toB_int(1));
  B_list newshape = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape,toB_int(3));
  B_listD_append(newshape,toB_int(4));
  B_listD_append(newshape,toB_int(5));
  numpy$$ndarray a = numpy$$ndarray_reshape(v,newshape);
  $printobj("a=\n",a);
  $printobj("a.transpose() =\n",numpy$$ndarray_transpose(a,NULL));

  B_list axes = $NEW(B_list,NULL,NULL);
  B_listD_append(axes,toB_int(1));
  B_listD_append(axes,toB_int(2));
  B_listD_append(axes,toB_int(0));
  $printobj("a.transpose([1,2,0]) =\n",numpy$$ndarray_transpose(a,axes));
  numpy$$ndarray b = numpy$$ndarray_reshape(numpy$$ndarray_arange(toB_int(60),toB_int(0),toB_int(-1)),newshape);
  $printobj("b=\n",b);
  numpy$$Primitive wit = (numpy$$Primitive)numpy$$PrimitiveB_intG_witness;
  numpy$$ndarray c = numpy$$ndarray_sort(wit,b,NULL);
  $printobj("b.sort() =\n",c);
  $printobj("b.sort(-1) =\n",numpy$$ndarray_sort(wit,b,toB_int(-1)));
  $printobj("a.clip(12,40) =\n",numpy$$ndarray_clip(wit,a,toB_int(12),toB_int(40)));
  $printobj("a.clip(17,None) =\n",numpy$$ndarray_clip(wit,a,toB_int(17),NULL));
  /*
  B_Iterable iter = (B_Iterable)$NEW(numpy$B_IterableD_ndarray,wit);
  B_Iterator it = iter->$class->__iter__(iter,c);
  $WORD nxt;
  while ((nxt = it->$class->__next__(it)))
    $printobj("",nxt);
  */
}
