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
#include <stdlib.h>
#include "../builtin.h"



int main() {
  B_Hashable hashwit = (B_Hashable)B_HashableD_intG_witness;
  B_SetD_set wit= $NEW(B_SetD_set,hashwit);
  B_set s = $NEW(B_set,hashwit,NULL,NULL);
  B_set s2 = $NEW(B_set,hashwit,NULL,NULL);
  printf("sets created\n");
  for (long i = 13; i < 1000; i++) {
    wit->$class->add(wit,s,to$int(i*i));
  }
  wit->$class->discard(wit,s,to$int(64));
  wit->$class->discard(wit,s,to$int(225));
  wit->$class->discard(wit,s,to$int(10000));
  int n = 0;
  for (long k = 0; k < 1000; k++)
    if (fromB_bool(wit->$class->__contains__(wit,s,to$int(k)))) {
      n++;
    }
  printf("#elements <1000 is %d (should be 18)\n",n);
  for (long i = 0; i < 500; i++) {
    wit->$class->add(wit,s2,to$int(i*i*i*i));
  }
  printf("size of s is %ld (should be 985)\n",from$int(wit->$class->__len__(wit,s)));
  printf("size of s2 is %ld (should be 500)\n",from$int(wit->$class->__len__(wit,s2)));
  B_set s3 = wit->W_Logical->$class->__and__(wit->W_Logical,s,s2);
  printf("size of intersection is %ld (should be 27)\n",from$int(wit->$class->__len__(wit,s3)));
          
  printf("checking if intersection is lt both operands; returns %ld and %ld\n",fromB_bool(wit->W_Ord->$class->__lt__(wit->W_Ord,s3,s)),fromB_bool(wit->W_Ord->$class->__lt__(wit->W_Ord,s3,s2)));
  B_Iterator iter = wit->$class->__iter__(wit,s3);
  $WORD w;
  printf("Iterating over intersection:\n");
  while((w = iter->$class->__next__(iter))) 
    printf("%ld\n",from$int(w));
  printf("Printing intersection with __str__ : %s\n",(s3->$class->__str__(s3))->str); 
  B_set s4 = wit->W_Logical->$class->__or__(wit->W_Logical,s,s2);
  printf("size of union is %ld (should be 1458)\n",from$int(wit->$class->__len__(wit,s4)));
  printf("checking if union is gt both operands; returns %ld and %ld\n",fromB_bool(wit->W_Ord->$class->__gt__(wit->W_Ord,s4,s)),fromB_bool(wit->W_Ord->$class->__gt__(wit->W_Ord,s4,s2)));
  B_set s5 = wit->W_Logical->$class->__xor__(wit->W_Logical,s,s2);
  printf("size of symmetric difference is %ld (should be 1431)\n",from$int(wit->$class->__len__(wit,s5)));
  printf("popping and printing again elements in intersection\n");
  while((w = wit->$class->pop(wit,s3)))
    printf("popped %ld\n",from$int(w));
  printf("size of intersection is now %ld\n",from$int(wit->$class->__len__(wit,s3)));
}
            
