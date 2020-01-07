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

void RAISE(exception e) {
  fprintf(stderr,"exception raised\n");
  exit(1);
}

// Version with list methods //////////////////////////////////////////////////////////////////////////////////
 
/* prints a list of ints */
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
  for (int i=0; i<n; i++) {
    w = $list_getitem(isPrime,i);
    if (fromWord(w)) {
      $list_append(primes,toWord(i));
    }
  }
  return primes;
}

// Version with protocol methods /////////////////////////////////////////////////////////////////////////////////////////

/* prints a Sequence of ints */
void printSequence(Sequence s) {
  $WORD w;
  printf("[");
  for (int i=0; i < *s->__class__->Collection$__methods__->__len__((Collection)s)-1; i++) {
    w = s->__class__->Sliceable$__methods__->Indexed$__methods__->__getitem__((Indexed)s,toWord(i));
    printf("%d, ",fromWord(w));
  }
  if (*s->__class__->Collection$__methods__->__len__((Collection)s) > 0) {//should use '>' for $int...
    w = s->__class__->Sliceable$__methods__->Indexed$__methods__->__getitem__((Indexed)s,toWord(*s->__class__->Collection$__methods__->__len__((Collection)s)-1));
    printf("%d",fromWord(w));
  }
  printf("]\n");
}
 
Sequence sieveS(int n) {
  $WORD false = toWord(0);
  $WORD true = toWord(1);
  $WORD w;
  Sequence isPrime = Sequence$__pack__(Sequence_$list_instance,Sequence_$list_instance->Collection$__methods__->__fromiter__(NULL)->__impl__);
  isPrime->__class__->append(isPrime,false); 
  isPrime->__class__->append(isPrime,false);
  for (int i=2; i < n; i++) 
    isPrime->__class__->append(isPrime,true);
  for (int i=2; i < floor(sqrt(n)); i++) {
    w = isPrime->__class__->Sliceable$__methods__->Indexed$__methods__->__getitem__((Indexed)isPrime,toWord(i));
    if (fromWord(w)) {
      for (int k=i*i; k<n; k+=i)
        isPrime->__class__->Sliceable$__methods__->Indexed$__methods__->__setitem__((Indexed)isPrime,toWord(k),false);
    }
  }
  Sequence primes = Sequence$__pack__(Sequence_$list_instance,Sequence_$list_instance->Collection$__methods__->__fromiter__(NULL)->__impl__);
  for (int i=0; i<n; i++) {
    w = isPrime->__class__->Sliceable$__methods__->Indexed$__methods__->__getitem__((Indexed)isPrime,toWord(i));
    if (fromWord(w)) {
      primes->__class__->append(primes,toWord(i));
    }
  }
  return primes;
}


int main() {

  // printf("%d\n",*$list_len(sieve(10000000)));

  list_instance_init();
  Sequence primes = sieveS(10000000);
  printf("%d\n",*primes->__class__->Collection$__methods__->__len__((Collection)primes));
}
