#ifndef HASH_H
#define HASH_H
#include <stddef.h>

typedef void *WORD;

typedef struct tuple_struct {
  WORD item;
  int length;
} *tuple_t;

/* Equality defined here for the moment */

int long_eq(WORD a, WORD b);
int double_eq(WORD a, WORD b);

size_t long_hash(WORD v);
size_t double_hash(WORD v);
size_t tuple_hash(WORD ht,WORD v);


typedef struct Hashable_struct {
  int (*eq)(WORD a, WORD b);
  size_t (*hash)(WORD v);
} *Hashable;

Hashable long_Hashable;
Hashable double_Hashable;

#endif
