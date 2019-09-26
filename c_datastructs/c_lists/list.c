#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "list.h"

static inline int min(int a, int b) {
    if (a > b)
        return b;
    return a;
}

static inline int max(int a, int b) {
    if (a > b)
        return a;
    return b;
}

// For now, expansion doubles capacity. 
static int expand(list_t lst) {
  if (lst->capacity == lst->length) {
    int newcapacity = lst->capacity==0 ? 1 : lst->capacity << 1;
    WORD* newptr = lst->data==NULL ? malloc(newcapacity*sizeof(WORD)) : realloc(lst->data,newcapacity*sizeof(WORD));
    if (newptr == NULL) {
       return -1;
    }
    lst->data = newptr;
    lst->capacity = newcapacity;
  }
  return 0;
}  

list_t list_new(int capacity) {
  if (capacity < 0) {
    errno = EINVAL;
    return NULL;
  } 
  list_t lst = malloc(sizeof(struct list_struct));
  if (lst == NULL) {
     return NULL;
  }
  if (capacity>0) {
    lst->data = malloc(capacity*sizeof(WORD));
    if (lst->data == NULL) {
      return NULL;
    }
  } else {
    lst->data = NULL;
  }
  lst->length = 0;
  lst->capacity = capacity;
  return lst;
}

// Container method
int list_contains(list_t lst, WORD elem, int (*eq)(WORD,WORD)) {
  for (int i=0; i < lst->length; i++)
    if (eq(elem,lst->data[i]))
      return 1;
  return 0;
}

// Iterable method
list_iterator_t list_iter(list_t lst) {
  list_iterator_t it = malloc(sizeof(struct list_iterator_struct));
  it->src = lst;
  it->nxt = 0;
  return it;
}

// Sized method
int list_len(list_t lst) {
  return lst->length;
}

// Reversible method
list_iterator_t list_reversed(list_t lst){
  list_iterator_t it = malloc(sizeof(struct list_iterator_struct));
  it->src = list_copy(lst);
  list_reverse(it->src);
  it->nxt = 0;
  return it;
}

// Sequence methods
WORD list_getitem(list_t lst, int ix) {
  int ix0 = ix < 0 ? lst->length + ix : ix;
  if (ix0 < 0 || ix0 >= lst->length) {
    errno = EINVAL;
    return NULL;
  }
  return lst->data[ix0];
}

int list_index(list_t lst, WORD elem, int startix, int endix,int (*eq)(WORD,WORD)) {
  int start = startix < 0 ? lst->length + startix : startix;
  int end = endix < 0 ? lst->length + endix : (endix < lst->length ? endix : lst->length);
  while (start < end) {
    if (eq(lst->data[start++],elem))
      return start-1;
  }
  errno = EINVAL;
  return -1;
}

int list_count(list_t lst, WORD elem, int (*eq)(WORD,WORD)) {
  int res = 0;
  for (int i = 0; i < lst->length; i++)
    if(eq(lst->data[i],elem)) res++;
  return res;
}

// MutableSequence methods
int list_setitem(list_t lst, int ix, WORD elem) {
  int ix0 = ix < 0 ? lst->length + ix : ix;
  if (ix0 < 0 || ix0 >= lst->length) {
    errno = EINVAL;
    return -1;
  }
  lst->data[ix0] = elem;
  return 0;
}

int list_append(list_t lst, WORD elem) {
  int err = expand(lst);
  if (err == 0)
    lst->data[lst->length++] = elem;
  return err;
}

// Simplified version; 2nd argument should be an iterable.
// So, for now we implement this by iterating append.
int list_extend(list_t lst, list_t other) {
  int n = other->length; //other and lst may be the same!
  for (int i=0; i<n; i++) {
    int err = list_append(lst,other->data[i]);
    if (err<0)
      return err;
  }
  return 0;
}
 
// Any int is acceptable as ix.
int list_insert(list_t lst, int ix, WORD elem) {
  int err = expand(lst);
  int ix0 = ix < 0 ? max(lst->length+ix,0) : min(ix,lst->length);
  if (err == 0) {
    memmove(lst->data + (ix0 + 1),
            lst->data + ix0 ,
            (lst->length - ix0) * sizeof(WORD));
    lst->data[ix0] = elem;
    lst->length++;
  }
  return err;
}

WORD list_pop(list_t lst, int ix) {
  int ix0 = ix < 0 ? lst->length + ix : ix;
  if (ix0 < 0 || ix0 >= lst->length) {
    errno = EINVAL;
    return NULL;
  }
  WORD elem = lst->data[ix0];
  memmove(lst->data + ix0,
          lst->data + (ix0 + 1),
          (lst->length-(ix0+1))*sizeof(WORD));
  lst->length--;
  return elem;
}
        
int list_remove(list_t lst, WORD elem, int (*eq)(WORD,WORD)) {
  int ix = list_index(lst,elem,0,lst->length,eq);
  if (ix < 0) return ix;
  memmove(lst->data + ix,
          lst->data + (ix + 1),
          (lst->length-(ix+1))*sizeof(WORD));
  lst->length--;
  return 0;
}

// In place reversal
void list_reverse(list_t lst) {
  for (int i = 0; i < lst->length/2; i++) {
    WORD tmp = lst->data[i];
    lst->data[i] = lst->data[lst->length-1-i];
    lst->data[lst->length-1-i] = tmp;
  }
}

void list_clear(list_t lst){
  lst->length = 0;   // what about capacity and data?
}

// Other list methods
list_t list_copy(list_t lst) {
  list_t res = list_new(lst->length);
  res->length=lst->length;
  memcpy(res->data,lst->data,lst->length*sizeof(WORD));
  return res;
}
                      
int list_sort(list_t lst, int (*cmp)(WORD,WORD)) {
  return heapsort(lst->data, lst->length, sizeof(WORD), cmp);
}

// Iterator methods
WORD list_iterator_next(list_iterator_t iter) {
  if(iter->nxt >= list_len(iter->src)) {
    errno = EINVAL;
    return NULL;
  }
  return iter->src->data[iter->nxt++];
}

/*
int range_iterator_next(range_iterator_t iter) {
  if((iter->step>0 && iter->nxt>iter->stop) || (iter->step<0 && iter->nxt<iter->stop)) {
    errno = EINVAL;
    return NULL;
  }
  int res = iter->nxt;
  iter->nxt += iter->step;
  return res;
}
*/

// Creating a range iterator

range_iterator_t range(int start, int stop, int step) {
  range_iterator_t iter = malloc(sizeof(struct range_iterator_struct));
  iter->nxt = start;
  iter->stop = stop;
  iter->step = step;
  return iter;
}

// Variants for non-pointer elem type
int list_getitem_p(list_t lst, int ix, WORD *res) {
  int ix0 = ix < 0 ? lst->length + ix : ix;
  if (ix0 < 0 || ix0 >= lst->length) {
    errno = EINVAL;
    return -1;
  }
  *res = lst->data[ix0];
  return 0;
}

int list_pop_p(list_t lst,int ix, WORD *res) {
  int ix0 = ix < 0 ? lst->length + ix : ix;
  if(ix0 < 0 || ix0 >= lst->length) {
    errno = EINVAL;
    return -1;
  }
  *res = lst->data[ix0];
  memmove(lst->data + ix0,
          lst->data + (ix0 + 1),
          (lst->length-(ix0+1))*sizeof(WORD));
  lst->length--;
  return 0;
}

int list_iterator_next_p(list_iterator_t iter, WORD *res) {
  if(iter->nxt >=list_len(iter->src)) {
    errno = EINVAL;
    return -1;
  }
  *res = iter->src->data[iter->nxt++];
  return 0;
}

int range_iterator_next_p(range_iterator_t iter, int *res) {
  if((iter->step>0 && iter->nxt>iter->stop) || (iter->step<0 && iter->nxt<iter->stop)) {
    errno = EINVAL;
    return -1;
  }
  *res = iter->nxt;
  iter->nxt += iter->step;
  return 0;
}
  
