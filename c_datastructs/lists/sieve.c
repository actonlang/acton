#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "list.h"

/* 
  If UNBOXED is defined, both keys and values are unboxed integers. 
  If not, they are boxed.
*/
// #define UNBOXED

#ifdef UNBOXED
WORD toWord(long i) {
  return (WORD)i;
}

long fromWord(WORD w) {
  return (long) w;
}

#else

WORD toWord(long i) {
  WORD res = malloc(sizeof(long));
  *(long*)res = i;
  return res;
}

long fromWord(WORD w) {
  return *(long*) w;
}

#endif

 
/* prints a list of ints */
void printlist(list_t lst) {
  WORD w;
  printf("[");
  for (int i=0; i < list_len(lst)-1; i++) {
    list_getitem(lst,i,&w);
    printf("%ld, ",fromWord(w));
  }
  if (list_len(lst) > 0) {
    list_getitem(lst,list_len(lst)-1,&w);
    printf("%ld",fromWord(w));
  }
  printf("]\n");
}

/* Sieve of Erathostenes */
list_t sieve(int n) {
  WORD false = toWord(0);
  WORD true = toWord(1);
  WORD w;
  list_t isPrime = list_new(0); // Replacing 0 with n avoids all doublings and copying, 
  list_append(isPrime,false);   // but we wanted to see the effect of these (which is small).
  list_append(isPrime,false);
  for (int i=2; i < n; i++) 
    list_append(isPrime,true);
  for (int i=2; i < floor(sqrt(n)); i++) {
    list_getitem(isPrime,i,&w);
    if (fromWord(w)) {
      for (int k=i*i; k<n; k+=i)
        list_setitem(isPrime,k,false);
    }
  }
  list_t primes = list_new(0);
  for (long i=0; i<n; i++) {
    list_getitem(isPrime,i,&w);
    if (fromWord(w)) 
      list_append(primes,toWord(i));
  }
  return primes;
}

int main() {
  printf("%d\n",list_len(sieve(10000000)));
}
