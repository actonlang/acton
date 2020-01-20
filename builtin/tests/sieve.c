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
void printSequence(Sequence$__class__ cl, $WORD seq) {
  Indexed$__class__ cl1 = cl->Sliceable$__methods__->Indexed$__methods__;
  printf("[");
  long n = from$int(cl->Collection$__methods__->__len__(cl->Collection$__methods__,
                                                        seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",from$int(cl1->__getitem__(cl1,seq,to$int(i))));
  if (n > 0) 
    printf("%ld",from$int(cl1->__getitem__(cl1,seq,to$int(n-1))));
  printf("]\n");
}


$WORD sieveS(int n) {
  $WORD false = to$int(0);
  $WORD true = to$int(1);
  $WORD w;
  Sequence$__class__ cl = Sequence$list_instance;
  Indexed$__class__ cl1 = cl->Sliceable$__methods__->Indexed$__methods__;
  $WORD isPrime = cl->Collection$__methods__->__fromiter__(cl->Collection$__methods__,NULL)->__impl__;
  cl->append(cl,isPrime,false); 
  cl->append(cl,isPrime,false);
  for (int i=2; i < n; i++) 
    cl->append(cl,isPrime,true);
  for (int i=2; i < floor(sqrt(n)); i++) {
    w = cl1->__getitem__(cl1,isPrime,to$int(i));
    if (from$int(w)) {
      for (int k=i*i; k<n; k+=i)
        cl1->__setitem__(cl1,isPrime,to$int(k),false);
    }
  }
  $WORD primes = cl->Collection$__methods__->__fromiter__(cl->Collection$__methods__,NULL)->__impl__;
  for (int i=0; i<n; i++) {
    w = cl1->__getitem__(cl1,isPrime,to$int(i));
    if (from$int(w)) {
      cl->append(cl,primes,to$int(i));
    }
  }
  return primes;
}
 
int main() {

  // printf("%d\n",*$list_len(sieve(10000000)));

  $WORD primes = sieveS(10000000);
  printf("%ld\n",*Collection$list_instance->__len__(Collection$list_instance,primes));
}
