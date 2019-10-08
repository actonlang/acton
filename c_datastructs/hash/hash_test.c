#include <stdio.h>
#include <stdlib.h>

#include "hash.h"

WORD mkWord(double x) {
  WORD res = malloc(sizeof(double));
  *(double*)res = x;
  return res;
}


int main() {
  WORD e[3];
  e[0] = mkWord(5.0);
  e[1] = mkWord(17.0);
  e[2] = mkWord(56.0);
  size_t (*h[])(WORD) = {double_hash, double_hash, double_hash};
  /*
  printf("hash of %d is %ld\n",543,long_hash(543));
  printf("hash of %d is %ld\n",-1,long_hash(-1));
  printf("hash of %d is %ld\n",-5,long_hash(-5));
  printf("hash of %f is %ld\n",-2.0,double_hash(-2.0));
  printf("hash of %f is %ld\n",-1.0,double_hash(-1.0));
  printf("hash of %f is %ld\n",0.75,double_hash(0.75));
  printf("hash of %s is %ld\n","test",bytes_hash("test",4));*/
  tuple_t t = malloc(sizeof(tuple_t));
  t->item = (WORD)e;
  t->length=3;
  tuple_t ht = malloc(sizeof(tuple_t));
  ht->item = (WORD)h;
  ht->length = 3;
  printf("hash of t is %ld\n",tuple_hash(ht,t));
}
