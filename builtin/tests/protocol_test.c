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

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "../builtin.h"

 
void printSequence(B_Sequence wit, $WORD seq) {
  printf("[");
  long n = fromB_int(wit->W_Collection->$class->__len__(wit->W_Collection, seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",fromB_int(wit->$class->__getitem__(wit,seq,toB_int(i))));
  if (n > 0) 
    printf("%ld",fromB_int(wit->$class->__getitem__(wit,seq,toB_int(n-1))));
  printf("]\n");
}

B_list range(B_Sequence wit, long a, long b) {
  B_list res = $NEW(B_list,NULL,NULL);
  for (long i=a; i<b; i++)
    wit->$class->append(wit,res,toB_int(i));
  return res;
}


$WORD concat(B_Collection wit1, B_Indexed wit2, $Plus wit3, $WORD s, $WORD zero) {
  $WORD res = zero;
  B_int len = wit1->$class->__len__(wit1,s);
  for (long i = 0; i < fromB_int(len); i++) {
    $WORD nxt = wit2->$class->__getitem__(wit2,s,toB_int(i));
    res = wit3->$class->__add__(wit3,res,nxt);
  }
  return res;
}
/*
$WORD B_intD_add(PlusG_class cl, $WORD a, $WORD b) {
  return toB_int(fromB_int(a)+fromB_int(b));
}

struct PlusG_class PlusB_intD_struct = {"GC_Plus",B_intD_add};
PlusG_class PlusB_intD_instance = &PlusB_intD_struct;
*/
int main() {
  B_Sequence wit = (B_Sequence)B_SequenceD_listG_witness;
  // first we use concat for list concatenation
  $WORD lst = $NEW(B_list,NULL,NULL);
  $WORD emptylist = $NEW(B_list,NULL,NULL);
  for (long i = 1; i< 10; i++) {
    wit->$class->append(wit,lst,range(wit,i,2*i));
  }
  printSequence(wit,concat(wit->W_Collection,(B_Indexed)wit,wit->W_Plus,lst,emptylist));
  // and then to sum a list of integers
  $WORD lst2 = range(wit,1,100);
  printf("1+2+...+99 = %ld\n",fromB_int(concat(wit->W_Collection,(B_Indexed)wit,($Plus)B_IntegralD_intG_witness,lst2,toB_int(0))));
  // and finally as a very complicated identity function for strings
  printf("result is '%s'\n",fromB_str(concat((B_Collection)B_ContainerD_strG_witness,(B_Indexed)B_SliceableD_strG_witness,($Plus)$PlusB_strG_witness,to$str("Complicated identity function"),to$str(""))));
}
                 
  
