#pragma once

#include "common.h"

struct iterator_internal_t;
typedef struct iterator_internal_t *iterator_internal_t;

struct $iterator;
typedef struct $iterator *$iterator;

struct iterator_internal_t {
  char *$GCINFO;
  $WORD state;
  $WORD (*next) (iterator_internal_t iter);
};

typedef struct $iterator$__methods__ {
  $WORD (*next)($iterator self);
} *$iterator$__methods__;

struct $iterator {
  $iterator$__methods__ __class__;
  iterator_internal_t __internal__;
};

/*
iterable_t range(int start, int stop, int step);

iterator_t iterable_iter(iterable_t it);
 
int iterator_next(iterator_t iter, WORD *res);
*/
