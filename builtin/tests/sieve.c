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
void printlist($list lst) {
  $WORD w;
  printf("[");
  for (int i=0; i < *$list_len(lst)-1; i++) {
    w = $list_getitem(lst,i);
    printf("%d, ",fromWord(w));
  }
  if (*$list_len(lst) > 0) {
    w = $list_getitem(lst,*$list_len(lst)-1);
    printf("%d",fromWord(w));
  }
  printf("]\n");
}
*/

// Sieve of Erathostenes 
$list sieve(int n) {
  $list isPrime = $NEW($list,NULL,NULL); 
  $list_append(isPrime,$False); 
  $list_append(isPrime,$False);
  for (int i=2; i < n; i++) 
    $list_append(isPrime,$True);
  for (int i=2; i < floor(sqrt(n)); i++) {
    //if (from$bool(isPrime->data[i])) {
    if (from$bool($list_getitem(isPrime,i))) {
      for (int k=i*i; k<n; k+=i)
        $list_setitem(isPrime,k,$False);
    }
  }
  $list primes = $NEW($list,NULL,NULL);
  for (int i=0; i<n; i++) {
    //if (from$bool(isPrime->data[i])) {
    if (from$bool($list_getitem(isPrime,i))) {
      $list_append(primes,to$int(i));
    }
  }
  return primes;
}

// Version with protocol methods /////////////////////////////////////////////////////////////////////////////////////////

$list sieveS($Sequence$list wit, int n) {
  $WORD w;
  $list isPrime = $NEW($list,NULL,NULL);
  wit->$class->append(wit,isPrime,$False); 
  wit->$class->append(wit,isPrime,$False);
  for (int i=2; i < n; i++) 
    wit->$class->append(wit,isPrime,$True);
  for (int i=2; i < floor(sqrt(n)); i++) {
    w = wit->$class->__getitem__(wit,isPrime,to$int(i));
    if (from$int(w)) {
      for (int k=i*i; k<n; k+=i)
        wit->$class->__setitem__(wit,isPrime,to$int(k),$False);
    }
  }
  $list primes = $NEW($list,NULL,NULL);
  for (int i=0; i<n; i++) {
    w = wit->$class->__getitem__(wit,isPrime,to$int(i));
    if (from$bool(w)) {
      wit->$class->append(wit,primes,to$int(i));
    }
  }
  return primes;
}

int main() {

  printf("%ld\n",$list_len(sieve(10000000)));

  //$Sequence$list wit = $Sequence$list$witness;
  //$list primes = sieveS(wit,10000000);
  //printf("%ld\n",from$int(wit->w$Collection->$class->__len__(wit->w$Collection,primes)));
}
