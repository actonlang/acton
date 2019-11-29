#include <stdlib.h>
#include "acterror.h"
#include "iterator.h"

typedef struct range_iterator_struct {
  int nxt,stop,step;
} *range_iterator_state_t; 

static int range_iterator_next(iterator_t iter, WORD *res) {
  range_iterator_state_t state = iter->state;
  if((state->step>0 && state->nxt>state->stop) || (state->step<0 && state->nxt<state->stop)) {
    return STOPITERATION;
  }
  *res = (WORD)(long)state->nxt;
  state->nxt += state->step;
  return 0;
}

// Fast version when step=1
static int range_iterator_next_fast(iterator_t iter, WORD *res) {
  range_iterator_state_t state = iter->state;
  if(state->nxt > state->stop) {
    return STOPITERATION;
  }
  *res = (WORD)(long)state->nxt++;
  return 0;
}

// Creating a range iterator
static iterator_t range_iter(int start, int stop, int step) {
  range_iterator_state_t state = malloc(sizeof(struct range_iterator_struct));
  state->nxt = start;
  state->stop = stop;
  state->step = step;
  iterator_t iter = malloc(sizeof(struct iterator_struct));
  iter->state = state;
  if (step==1)
    iter->next = range_iterator_next_fast;
  else
    iter->next = range_iterator_next;    
  return iter;
}

iterator_t iterable_iter(iterable_t it) {
  return it->itr;
}

int iterator_next(iterator_t iter, WORD *res) {
  return iter->next(iter,res);
}


iterable_t range(int start, int stop, int step) {
  iterable_t r = malloc(sizeof(struct iterable_struct));
  r->itr = range_iter(start,stop,step);
  r->iter = iterable_iter;
  return r;
}

                      
