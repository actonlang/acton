#pragma once

typedef void *WORD;

typedef struct iterator_struct *iterator_t;

typedef struct iterable_struct *iterable_t;

struct iterator_struct {
  WORD state;
  int (*next) (iterator_t iter,WORD *res);
};

struct iterable_struct {
  iterator_t itr;
  iterator_t (*iter) (iterable_t it);
};
 
iterable_t range(int start, int stop, int step);

iterator_t iterable_iter(iterable_t it);
 
int iterator_next(iterator_t iter, WORD *res);
