#ifndef ITERATOR_H
#define ITERATOR_H

typedef void *WORD;

typedef struct iterator_struct *iterator_t;

struct iterator_struct {
  WORD state;
  int (*next) (iterator_t iter,WORD *res);
};

typedef struct range_iterator_struct {
  int nxt,stop,step;
} *range_iterator_state_t; 

// Creating a range iterator
iterator_t range(int start, int stop, int step);

// Iterator methods
int iterator_next(iterator_t iter, WORD *res);

#endif
