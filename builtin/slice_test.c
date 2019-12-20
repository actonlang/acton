#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "list.h"

void
RAISE(exception e) {
  fprintf(stderr,"exception raised\n");
  exit(1);
}

$WORD toWord(long i) {
  $WORD res = malloc(sizeof(long));
  *(long*)res = i;
  return res;
}

long fromWord($WORD w) {
  return *(long*) w;
}

void printlist($list lst) {
  $WORD w;
  printf("[");
  for (int i=0; i < *$list_len(lst)-1; i++) {
    w = $list_getitem(lst,i);
    printf("%ld, ",fromWord(w));
  }
  if ($list_len(lst) > 0) {
    w = $list_getitem(lst,*$list_len(lst)-1);
    printf("%ld",fromWord(w));
  }
  printf("]\n");
}

int main() {
  $list lst = $list_fromiter(NULL);
  for (long i=0; i<100; i++)
    $list_append(lst,toWord(i));
  int start = -1;
  int stop = 0;
  int step = -2;
  struct Slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  $list lst2 = $list_getslice(lst,&slc);
  $list lst3 = $list_fromiter(NULL);
  for (long i=100; i<110; i++)
    $list_append(lst3,toWord(i));
  start = 10;
  stop = 30;
  step = 2;
  $list_setslice(lst2,&slc,lst3);
  printlist(lst2);
  
}
