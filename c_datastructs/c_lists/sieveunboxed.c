#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "list.h"

 
/* prints a list of ints */
void printlist(list_t lst) {
  printf("[");
  for (int i=0; i < lst->length-1; i++)
    printf("%d, ",(int)(lst->data[i]));
  if (lst->length > 0) printf("%d",(int)lst->data[lst->length-1]);
  printf("]\n");
}

/* Sieve of Erathostenes */
list_t sieve(int n) {
  WORD false = (WORD)0;
  WORD true = (WORD)1;
  list_t isPrime = list_new(n);
  // for (int i=0; i < n; i++)  //   Replaced by next three lines
  int i;
  range_iterator_t iter = range(0,n-1,1);
  while(!range_iterator_next_p(iter,&i))
    list_append(isPrime,true);
  list_setitem(isPrime,0,false);
  list_setitem(isPrime,1,false);
   
  for (int i=2; i < floor(sqrt(n)); i++)
    if (list_getitem(isPrime,i)) {
      for (int k=i*i; k<n; k+=i)
        list_setitem(isPrime,k,false);
    }
  list_t primes = list_new(0);
  for (int i=0; i<n; i++)
    if (list_getitem(isPrime,i)) 
      list_append(primes,(WORD)(long)i);
  return primes;
}

int main() {
  printf("%d\n",list_len(sieve(10000000)));
}
  
