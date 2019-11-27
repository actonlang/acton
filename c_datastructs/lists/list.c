#include <stdlib.h>
#include <string.h>

#include "list.h"
#include "iterator.h"
#include "acterror.h"
#include "slice.h"

typedef struct list_iterator_struct {
  list_t src;
  int nxt;
} *list_iterator_state_t; 

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
static int expand(list_t lst,int n) {
   if (lst->capacity >= lst->length + n)
     return 0;
   int newcapacity = lst->capacity==0 ? 1 : lst->capacity;
   while (newcapacity < lst->length+n)
     newcapacity <<= 1;
   WORD* newptr = lst->data==NULL ? malloc(newcapacity*sizeof(WORD)) : realloc(lst->data,newcapacity*sizeof(WORD));
   if (newptr == NULL) {
     return MEMORYERROR;
   }
   lst->data = newptr;
   lst->capacity = newcapacity;
   return 0;
}  

list_t list_new(int capacity) {
  if (capacity < 0) {
    return NULL; // VALUEERROR
  } 
  list_t lst = malloc(sizeof(struct list_struct));
  if (lst == NULL) {
    return NULL; // MEMORYERROR
  }
  if (capacity>0) {
    lst->data = malloc(capacity*sizeof(WORD));
    if (lst->data == NULL) {
      return NULL;  // MEMORYERROR
    }
  } else {
    lst->data = NULL;
  }
  lst->length = 0;
  lst->capacity = capacity;
  return lst;
}

list_t list_init(WORD elems[], int length) {
  list_t res = list_new(0);
  res->data = (WORD*)elems;
  res->length = length;
  res->capacity = length;
  return res;
}

// Container method
int list_contains(list_t lst, WORD elem, int (*eq)(WORD,WORD)) {
  for (int i=0; i < lst->length; i++)
    if (eq(elem,lst->data[i]))
      return 1;
  return 0;
}

// Iterable method
int list_iterator_next(iterator_t iter, WORD *res) {
  list_iterator_state_t state = iter->state;
  if(state->nxt >=list_len(state->src)) {
    return STOPITERATION;
  }
  *res = state->src->data[state->nxt++];
  return 0;
}

iterator_t list_iter(list_t lst) {
  list_iterator_state_t state = malloc(sizeof(struct list_iterator_struct));
  state->src = lst;
  state->nxt = 0;
  iterator_t iter = malloc(sizeof(struct iterator_struct));
  iter->state = state;
  iter->next = list_iterator_next;
  return iter;
}

// Sized method
int list_len(list_t lst) {
  return lst->length;
}

// Reversible method
iterator_t list_reversed(list_t lst){
  list_iterator_state_t state = malloc(sizeof(struct list_iterator_struct));
  state->src = list_copy(lst);
  list_reverse(state->src);
  state->nxt = 0;
  iterator_t iter = malloc(sizeof(struct iterator_struct));
  iter->state = state;
  iter->next = list_iterator_next;
  return iter;
}

// Sequence methods
int list_getitem(list_t lst, int ix, WORD *res) {
  int ix0 = ix < 0 ? lst->length + ix : ix;
  if (ix0 < 0 || ix0 >= lst->length) {
    return INDEXERROR;
  }
  *res = lst->data[ix0];
  return 0;
}

int list_getslice(list_t lst, slice_t slc, WORD *res) {
  int len = lst->length;
  int start, stop, step;
  int r = normalize_slice(slc, &len, &start, &stop, &step);
  if (r<0) return r;
  //slice notation have been eliminated and default values applied.
  // len now is the length of the slice
  list_t rlst = list_new(len);
  int t = start;
  for (int i=0; i<len; i++) {
    WORD w;
    list_getitem(lst,t,&w);
    list_append(rlst,w);
    t += step;
  }
  *res = (WORD)rlst;
  return 0;
}

      
int list_index(list_t lst, WORD elem, int startix, int endix,int (*eq)(WORD,WORD)) {
  int start = startix < 0 ? lst->length + startix : startix;
  int end = endix < 0 ? lst->length + endix : (endix < lst->length ? endix : lst->length);
  while (start < end) {
    if (eq(lst->data[start++],elem))
      return start-1;
  }
  return VALUEERROR;
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
    return INDEXERROR;
  }
  lst->data[ix0] = elem;
  return 0;
}

int list_setslice(list_t lst, slice_t slc, list_t other) {
  int len = lst->length;
  int start, stop, step;
  int r = normalize_slice(slc, &len, &start, &stop, &step);
  if (r<0) return r;
  if (step != 1 && other->length != len)
    return VALUEERROR;
  int copy = other->length <= len ? other->length : len;
  int t = start;
  for (int i= 0; i<copy; i++) {
    lst->data[t] = other->data[i];
    t += step;
  }
  if (other->length == len)
    return 0;
  if (other->length < len) {
    memmove(lst->data + start + copy,
            lst->data + start + len,
            (lst->length-(start+len))*sizeof(WORD));
     lst->length-=len-other->length;
     return 0;
  } else {
    expand(lst,other->length-len);
    int rest = lst->length -(start+copy);
    int incr = other->length - len;
    memmove(lst->data + start + copy + incr,
            lst->data + start + copy,
            rest*sizeof(WORD));
    for (int i = copy; i < other->length; i++)
      lst->data[start+i] = other->data[i];
    lst->length = lst->length+incr;
    return 0;
  }
}

int list_delitem(list_t lst,int ix) {
  WORD dummy;
  return list_pop(lst,ix,&dummy);
}

int list_delslice(list_t lst, slice_t slc) {
  int len = lst->length;
  int start, stop, step;
  int r = normalize_slice(slc, &len, &start, &stop, &step);
  if (r<0) return r;
  if (len==0) return 0;
  for (int ix = start+step*(len-1); ix>= start; ix -= step)
    list_delitem(lst,ix);
  return 0;
}

int list_append(list_t lst, WORD elem) {
  int err = expand(lst,1);
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
  int err = expand(lst,1);
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

int list_pop(list_t lst,int ix, WORD *res) {
  int ix0 = ix < 0 ? lst->length + ix : ix;
  if(ix0 < 0 || ix0 >= lst->length) {
    return INDEXERROR;
  }
  *res = lst->data[ix0];
  memmove(lst->data + ix0,
          lst->data + (ix0 + 1),
          (lst->length-(ix0+1))*sizeof(WORD));
  lst->length--;
  return 0;
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
