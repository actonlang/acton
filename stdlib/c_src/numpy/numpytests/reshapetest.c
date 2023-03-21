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
  $ndarray a = $ndarray_arange(to$int(60));
  printf("a.shape = %s\n",a->shape->$class->__str__(a->shape)->str);
  printf("a.strides = %s\n",a->strides->$class->__str__(a->strides)->str);
  printf("a=%s\n",a->$class->__str__(a)->str);
  B_list newshape = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape,to$int(3));
  B_listD_append(newshape,to$int(4));
  B_listD_append(newshape,to$int(5));
  $ndarray b = $ndarray_reshape(a,newshape);
  printf("b.shape = %s\n",b->shape->$class->__str__(b->shape)->str);
  printf("b.strides = %s\n",b->strides->$class->__str__(b->strides)->str);
  printf("b=%s\n",b->$class->__str__(b)->str);
  B_list ix = $NEW(B_list,NULL,NULL);
  B_listD_append(ix,$NEW($Slice,to$int(2),NULL,to$int(3)));
  $ndarray c = $nd_getslice(a,ix);
  printf("c.shape = %s\n",c->shape->$class->__str__(c->shape)->str);
  printf("c.strides = %s\n",c->strides->$class->__str__(c->strides)->str);
  printf("c=%s\n",c->$class->__str__(c)->str);
  B_list newshape2 = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape2,to$int(4));
  B_listD_append(newshape2,to$int(5));
  $ndarray d = $ndarray_reshape(c,newshape2);
  printf("d.shape = %s\n",d->shape->$class->__str__(d->shape)->str);
  printf("d.strides = %s\n",d->strides->$class->__str__(d->strides)->str);
  printf("d=%s\n",d->$class->__str__(d)->str);
  B_list newshape3 = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape3,to$int(20));
  $ndarray e = $ndarray_reshape(d,newshape3);
  printf("e.shape = %s\n",e->shape->$class->__str__(e->shape)->str);
  printf("e.strides = %s\n",e->strides->$class->__str__(e->strides)->str);
  printf("e=%s\n",e->$class->__str__(e)->str);
  B_list newshape4 = $NEW(B_list,NULL,NULL);
  B_listD_append(newshape4,to$int(60));
  $ndarray f = $ndarray_reshape(b,newshape4);
  printf("f.shape = %s\n",f->shape->$class->__str__(f->shape)->str);
  printf("f.strides = %s\n",f->strides->$class->__str__(f->strides)->str);
  printf("f=%s\n",f->$class->__str__(f)->str);

}
