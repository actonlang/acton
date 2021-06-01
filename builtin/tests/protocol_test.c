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

 
void printSequence($Sequence wit, $WORD seq) {
  printf("[");
  long n = from$int(wit->w$Collection->$class->__len__(wit->w$Collection, seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",from$int(wit->$class->__getitem__(wit,seq,to$int(i))));
  if (n > 0) 
    printf("%ld",from$int(wit->$class->__getitem__(wit,seq,to$int(n-1))));
  printf("]\n");
}

$list range($Sequence wit, long a, long b) {
  $list res = $NEW($list,NULL,NULL);
  for (long i=a; i<b; i++)
    wit->$class->append(wit,res,to$int(i));
  return res;
}


$WORD concat($Collection wit1, $Indexed wit2, $Plus wit3, $WORD s, $WORD zero) {
  $WORD res = zero;
  $int len = wit1->$class->__len__(wit1,s);
  for (long i = 0; i < from$int(len); i++) {
    $WORD nxt = wit2->$class->__getitem__(wit2,s,to$int(i));
    res = wit3->$class->__add__(wit3,res,nxt);
  }
  return res;
}
/*
$WORD $int_add(Plus$class cl, $WORD a, $WORD b) {
  return to$int(from$int(a)+from$int(b));
}

struct Plus$class Plus$int_struct = {"GC_Plus",$int_add};
Plus$class Plus$int_instance = &Plus$int_struct;
*/
int main() {
  $Sequence wit = ($Sequence)$Sequence$list$witness;
  // first we use concat for list concatenation
  $WORD lst = $NEW($list,NULL,NULL);
  $WORD emptylist = $NEW($list,NULL,NULL);
  for (long i = 1; i< 10; i++) {
    wit->$class->append(wit,lst,range(wit,i,2*i));
  }
  printSequence(wit,concat(wit->w$Collection,($Indexed)wit,wit->w$Plus,lst,emptylist));
  // and then to sum a list of integers
  $WORD lst2 = range(wit,1,100);
  printf("1+2+...+99 = %ld\n",from$int(concat(wit->w$Collection,($Indexed)wit,($Plus)$Integral$int$witness,lst2,to$int(0))));
  // and finally as a very complicated identity function for strings
  printf("result is '%s'\n",from$str(concat(($Collection)$Container$str$witness,($Indexed)$Sliceable$str$witness,($Plus)$Plus$str$witness,to$str("Complicated identity function"),to$str(""))));
}
                 
  
