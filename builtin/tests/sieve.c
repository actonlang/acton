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

/* 
  If UNBOXED is defined, both keys and values are unboxed integers. 
  If not, they are boxed.

 #define UNBOXED

#ifdef UNBOXED
$WORD toWord(int i) {
  return ($WORD)i;
}

int fromWord($WORD w) {
  return (int) w;
}

#else

$WORD toWord(int i) {
  $WORD res = malloc(sizeof(int));
  *(int*)res = i;
  return res;
}

int fromWord($WORD w) {
  return *(int*) w;
}

#endif
*/
 
// Version with list methods //////////////////////////////////////////////////////////////////////////////////
/*
// prints a list of ints 
void printlist(B_list lst) {
  $WORD w;
  printf("[");
  for (int i=0; i < *B_listD_len(lst)-1; i++) {
    w = B_listD_getitem(lst,i);
    printf("%d, ",fromWord(w));
  }
  if (*B_listD_len(lst) > 0) {
    w = B_listD_getitem(lst,*B_listD_len(lst)-1);
    printf("%d",fromWord(w));
  }
  printf("]\n");
}
*/

// Sieve of Erathostenes 
B_list sieve(int n) {
  B_list isPrime = $NEW(B_list,NULL,NULL); 
  B_listD_append(isPrime,B_False); 
  B_listD_append(isPrime,B_False);
  for (int i=2; i < n; i++) 
    B_listD_append(isPrime,B_True);
  for (int i=2; i < floor(sqrt(n)); i++) {
    //if (fromB_bool(isPrime->data[i])) {
    if (fromB_bool(B_listD_getitem(isPrime,i))) {
      for (int k=i*i; k<n; k+=i)
        B_listD_setitem(isPrime,k,B_False);
    }
  }
  B_list primes = $NEW(B_list,NULL,NULL);
  for (int i=0; i<n; i++) {
    //if (fromB_bool(isPrime->data[i])) {
    if (fromB_bool(B_listD_getitem(isPrime,i))) {
      B_listD_append(primes,to$int(i));
    }
  }
  return primes;
}

// Version with protocol methods /////////////////////////////////////////////////////////////////////////////////////////

B_list sieveS(B_SequenceD_list wit, int n) {
  $WORD w;
  B_list isPrime = $NEW(B_list,NULL,NULL);
  wit->$class->append(wit,isPrime,B_False); 
  wit->$class->append(wit,isPrime,B_False);
  for (int i=2; i < n; i++) 
    wit->$class->append(wit,isPrime,B_True);
  for (int i=2; i < floor(sqrt(n)); i++) {
    w = wit->$class->__getitem__(wit,isPrime,to$int(i));
    if (from$int(w)) {
      for (int k=i*i; k<n; k+=i)
        wit->$class->__setitem__(wit,isPrime,to$int(k),B_False);
    }
  }
  B_list primes = $NEW(B_list,NULL,NULL);
  for (int i=0; i<n; i++) {
    w = wit->$class->__getitem__(wit,isPrime,to$int(i));
    if (fromB_bool(w)) {
      wit->$class->append(wit,primes,to$int(i));
    }
  }
  return primes;
}

int main() {

  printf("%ld\n",B_listD_len(sieve(10000000)));

  //B_SequenceD_list wit = B_SequenceD_listG_witness;
  //B_list primes = sieveS(wit,10000000);
  //printf("%ld\n",from$int(wit->W_Collection->$class->__len__(wit->W_Collection,primes)));
}
