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

#include <stdio.h>
#include "../builtin.h"
 
int main() {
  B_Iterable wit = (B_Iterable)B_IterableD_rangeG_witness;
  B_Iterator it = wit->$class->__iter__(wit,$NEW(B_range,to$int(1),to$int(100),to$int(1)));
  B_list lst = B_listD_fromiter(it);
  B_ContainerD_list wit2 = $NEW(B_ContainerD_list,(B_Eq)B_HashableD_intG_witness);
  B_bool b = wit2->$class->__contains__(wit2,lst,to$int(17));
  B_bool c = wit2->$class->__contains__(wit2,lst,to$int(171));
  B_bool d = wit2->$class->__contains__(wit2,lst,to$int(100));
  printf("results are %s, %s, %s\n",(b->$class->__str__(b))->str,(c->$class->__str__(c))->str,(d->$class->__str__(d))->str);
}
  
