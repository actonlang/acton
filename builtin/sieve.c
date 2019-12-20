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
$WORD toWord(long i) {
  return ($WORD)i;
}

long fromWord($WORD w) {
  return (long) w;
}

#else

$WORD toWord(long i) {
  $WORD res = malloc(sizeof(long));
  *(long*)res = i;
  return res;
}

long fromWord($WORD w) {
  return *(long*) w;
}

#endif

void RAISE(exception e) {
  fprintf(stderr,"exception raised\n");
  exit(1);
}

 
/* prints a list of ints */
void printlist($list lst) {
  $WORD w;
  printf("[");
  for (int i=0; i < *$list_len(lst)-1; i++) {
    w = $list_getitem(lst,i);
    printf("%ld, ",fromWord(w));
  }
  if (*$list_len(lst) > 0) {
    w = $list_getitem(lst,*$list_len(lst)-1);
    printf("%ld",fromWord(w));
  }
  printf("]\n");
}

/* Sieve of Erathostenes */
$list sieve(int n) {
  $WORD false = toWord(0);
  $WORD true = toWord(1);
  $WORD w;
  $list isPrime = $list_fromiter(NULL); 
  $list_append(isPrime,false); 
  $list_append(isPrime,false);
  for (int i=2; i < n; i++) 
    $list_append(isPrime,true);
  for (int i=2; i < floor(sqrt(n)); i++) {
    w = $list_getitem(isPrime,i);
    if (fromWord(w)) {
      for (int k=i*i; k<n; k+=i)
        $list_setitem(isPrime,k,false);
    }
  }
  $list primes = $list_fromiter(NULL);
  for (long i=0; i<n; i++) {
    w = $list_getitem(isPrime,i);
    if (fromWord(w)) {
      $list_append(primes,toWord(i));
    }
  }
  return primes;
}

int main() {
  printf("%d\n",*$list_len(sieve(10000000)));
}
