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
  $list lst = $NEW($list,NULL,NULL);
  for (int i=0; i<1000; i++)
    $list_append(lst,to$int((long)rand()%200+100));
  $ndarray a = $ndarray_array(lst);
  printf("a=%s\n",a->$class->__str__(a)->str);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(10));
  $list_append(newshape,to$int(100));
  $ndarray c = $ndarray_reshape(a,newshape);
  printf("c=%s\n",c->$class->__str__(c)->str);
  $ndarray d =  $ndarray_partition(c,to$int(4));
  printf("d=%s\n",d->$class->__str__(d)->str);

  $ndarray e = $ndarray_linspace(to$float(0),to$float(1),to$int(101));
  $ndarray f = $ndarray_clip$float(e,to$float(0.2),to$float(0.6));
  printf("f=%s\n",f->$class->__str__(f)->str);
}
