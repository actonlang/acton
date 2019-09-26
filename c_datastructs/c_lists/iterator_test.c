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

list_t list_range(int start, int stop) {
  range_iterator_t iter = range(start,stop,1);
  int i;
  list_t res = list_new(0);
  while(!range_iterator_next_p(iter,&i))
    list_append(res,(WORD)(long)i);
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
  list_iterator_t it = list_reversed(t);
  while (!list_iterator_next_p(it,&nxt)) {
    sum+=(long)nxt;
    printf("%ld, ",(long)nxt);
  }
  printf("\nsum=%ld\n",sum);
  list_insert(t,5,(WORD)100);
  list_pop(t,10);
  printlist(t);

  range_iterator_t iter = range(1,5,1);
  list_t lst = list_new(5);
  int n;
  while(!range_iterator_next_p(iter,&n))
    list_append(lst,list_range(1,n));

  // lst2 = [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
  
  list_t flat = list_new(5);
  list_iterator_t iter2 = list_iter(lst);
  list_t tmp = (list_t)(list_iterator_next(iter2));
  while (tmp) {
    list_iterator_t iter3 = list_iter(tmp);
    WORD k;
    while(!list_iterator_next_p(iter3,&k))
      list_append(flat,k);
    tmp = (list_t)(list_iterator_next(iter2));
  }
  printlist(flat);

    // flat = [1,1,2,1,2,3,1,2,3,4,1,2,3,4,5]

}


        
