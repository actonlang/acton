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

int main() {
  $ndarray v = $ndarray_arange(toB_int(60));
  B_list newshape = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape,toB_int(3));
  B_listD_append(newshape,toB_int(4));
  B_listD_append(newshape,toB_int(5));
  $ndarray a = $ndarray_reshape(v,newshape);
  printf("a=%s\n",fromB_str(a->$class->__str__(a)));
  B_list ix = $NEW(B_list,NULL,NULL);
  B_listD_append(ix,NULL);
  $Slice s1 = $NEW($Slice,NULL,NULL,NULL);
  B_listD_append(ix,s1);
  B_listD_append(ix,toB_int(2));
  $ndarray b = $nd_getslice(a,ix);
  printf("b=%s\n",fromB_str(b->$class->__str__(b)));
  B_list ix1 = $NEW(B_list,NULL,NULL);
  B_listD_append(ix1,toB_int(1));
  $Slice s2 = $NEW($Slice,toB_int(1),NULL,NULL);
  B_listD_append(ix1,s2);
  $Slice s3 = $NEW($Slice,toB_int(-1),toB_int(0),toB_int(-1));
  B_listD_append(ix1,s3);
  $ndarray c = $nd_getslice(a,ix1);
  printf("c=%s\n",fromB_str(c->$class->__str__(c)));
  B_list test = B_listD_copy(c->shape);
  $ndarray d = $ndarray_func1(mul2,c);
  printf("d=%s\n",fromB_str(d->$class->__str__(d)));
  $ndarray e = $ndarray_fromatom(($Super)toB_int(3));
  printf("e.shape = %s\n",e->shape->$class->__str__(e->shape)->str);
  printf("e.strides = %s\n",e->strides->$class->__str__(e->strides)->str);
  printf("e=%s\n",fromB_str(e->$class->__str__(e)));
  B_Plus$ndarrayD_int wit = B_Plus$ndarrayD_intG_witness;
  $ndarray f = wit->$class->__add__(wit,d,e);
  printf("f=%s\n",fromB_str(f->$class->__str__(f)));  
  $ndarray g = wit->$class->__add__(wit,a,e);
  printf("g.shape = %s\n",g->shape->$class->__str__(g->shape)->str);
  printf("g.strides = %s\n",g->strides->$class->__str__(g->strides)->str);
  printf("g=%s\n",fromB_str(g->$class->__str__(g)));  
}
