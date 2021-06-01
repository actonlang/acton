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
  numpy$$ndarray v = numpy$$ndarray_arange(to$int(0),to$int(60),to$int(1));
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(4));
  $list_append(newshape,to$int(5));
  numpy$$ndarray a = numpy$$ndarray_reshape(v,newshape);
  $printobj("a=\n",a);
  $printobj("a.transpose() =\n",numpy$$ndarray_transpose(a,NULL));

  $list axes = $NEW($list,NULL,NULL);
  $list_append(axes,to$int(1));
  $list_append(axes,to$int(2));
  $list_append(axes,to$int(0));
  $printobj("a.transpose([1,2,0]) =\n",numpy$$ndarray_transpose(a,axes));
  numpy$$ndarray b = numpy$$ndarray_reshape(numpy$$ndarray_arange(to$int(60),to$int(0),to$int(-1)),newshape);
  $printobj("b=\n",b);
  numpy$$Primitive wit = (numpy$$Primitive)numpy$$Primitive$int$witness;
  numpy$$ndarray c = numpy$$ndarray_sort(wit,b,NULL);
  $printobj("b.sort() =\n",c);
  $printobj("b.sort(-1) =\n",numpy$$ndarray_sort(wit,b,to$int(-1)));
  $printobj("a.clip(12,40) =\n",numpy$$ndarray_clip(wit,a,to$int(12),to$int(40)));
  $printobj("a.clip(17,None) =\n",numpy$$ndarray_clip(wit,a,to$int(17),NULL));
  /*
  $Iterable iter = ($Iterable)$NEW(numpy$$Iterable$ndarray,wit);
  $Iterator it = iter->$class->__iter__(iter,c);
  $WORD nxt;
  while ((nxt = it->$class->__next__(it)))
    $printobj("",nxt);
  */
}
