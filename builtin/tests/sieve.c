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


// Sieve of Erathostenes 
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
  for (int i=0; i<n; i++) {
    w = $list_getitem(isPrime,i);
    if (fromWord(w)) {
      $list_append(primes,toWord(i));
    }
  }
  return primes;
}
*/
// Version with protocol methods /////////////////////////////////////////////////////////////////////////////////////////

/* prints a Sequence of ints */
void printSequence(Sequence$list wit, $WORD seq) {
  printf("[");
  long n = from$int(wit->_Collection->__class__->__len__(wit->_Collection, seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",from$int(wit->__class__->__getitem__(wit,seq,to$int(i))));
  if (n > 0) 
    printf("%ld",from$int(wit->__class__->__getitem__(wit,seq,to$int(n-1))));
  printf("]\n");
}


$list sieveS(Sequence$list wit, int n) {
  $int false = to$int(0);
  $int true = to$int(1);
  $WORD w;
  $list isPrime = wit->_Collection->__class__->__fromiter__(wit->_Collection,NULL);
  wit->__class__->append(wit,isPrime,false); 
  wit->__class__->append(wit,isPrime,false);
  for (int i=2; i < n; i++) 
    wit->__class__->append(wit,isPrime,true);
  for (int i=2; i < floor(sqrt(n)); i++) {
    w = wit->__class__->__getitem__(wit,isPrime,to$int(i));
    if (from$int(w)) {
      for (int k=i*i; k<n; k+=i)
        wit->__class__->__setitem__(wit,isPrime,to$int(k),false);
    }
  }
  $list primes = wit->_Collection->__class__->__fromiter__(wit->_Collection,NULL);
  for (int i=0; i<n; i++) {
    w = wit->__class__->__getitem__(wit,isPrime,to$int(i));
    if (from$int(w)) {
      wit->__class__->append(wit,primes,to$int(i));
    }
  }
  return primes;
}
 
int main() {

  // printf("%d\n",*$list_len(sieve(10000000)));

  Sequence$list wit = Sequence$list_new();
  $list primes = sieveS(wit,10000000);
  printf("%ld\n",from$int(wit->_Collection->__class__->__len__(wit->_Collection,primes)));
}
