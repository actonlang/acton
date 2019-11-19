#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "list.h"
#include "iterator.h"

/* prints a list of ints */
void printlist(list_t lst) {
  printf("[");
  for (int i=0; i < lst->length-1; i++)
    printf("%d, ",(int)(lst->data[i]));
  if (lst->length > 0) printf("%d",(int)lst->data[lst->length-1]);
  printf("]\n");
}

list_t list_range(int start, int stop) {
  iterator_t iter = range(start,stop,1);
  WORD i;
  list_t res = list_new(0);
  while(!iterator_next(iter,&i))
    list_append(res,i);
  return res;
}
  
int main() {
  long sum = 0;
  WORD nxt;
  list_t t = list_new(10);
  for (long i = 0; i<20; i++)
    list_append(t,(WORD)i);
  list_t t1 =list_copy(t);
  list_reverse(t1);
  printlist(t1);
  iterator_t it = list_reversed(t);
  while (!iterator_next(it,&nxt)) {
    sum+=(long)nxt;
    printf("%ld, ",(long)nxt);
  }
  printf("\nsum=%ld\n",sum);
  list_insert(t,5,(WORD)100);
  WORD w;
  list_pop(t,10,&w);
  printlist(t);

  iterator_t iter = range(1,5,1);
  list_t lst = list_new(5);
  WORD n;
  while(!iterator_next(iter,&n))
    list_append(lst,list_range(1,(int)n));
  // lst2 = [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
  
  list_t flat = list_new(5);
  iterator_t iter2 = list_iter(lst);
  WORD tmp;
  while (!iterator_next(iter2,&tmp)) {
      iterator_t iter3 = list_iter((list_t)tmp);
    WORD k;
    while(!iterator_next(iter3,&k))
      list_append(flat,k);
  }
  printlist(flat);

    // flat = [1,1,2,1,2,3,1,2,3,4,1,2,3,4,5]

}


        
