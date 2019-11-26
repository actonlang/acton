#pragma once

#include "iterator.h"
#include "slice.h"

typedef void *WORD;

typedef struct list_struct {
  WORD *data;
  int length;
  int capacity;
} *list_t;

list_t list_new(int capacity);
list_t list_init(WORD elems[], int length);

// Container method
int list_contains(list_t lst, WORD elem, int (*eq)(WORD,WORD));

// Iterable method
iterator_t list_iter(list_t lst);  

// Sized method
int list_len(list_t lst);

// Reversible method
iterator_t list_reversed(list_t lst);

// Sequence methods 
int list_getitem(list_t lst, int ix, WORD *res);
int list_getslice(list_t lst, slice_t slc, WORD *res);
int list_index(list_t lst, WORD elem, int startix, int endix,int (*eq)(WORD,WORD));
int list_count(list_t lst, WORD elem, int (*eq)(WORD,WORD));

// MutableSequence methods
int list_setitem(list_t lst, int ix, WORD elem);
int list_setslice(list_t lst, slice_t slc, list_t other);
int list_delitem(list_t lst,int ix);
int list_delslice(list_t lst, slice_t slc);
int list_append(list_t lst, WORD elem);
int list_insert(list_t lst, int ix, WORD elem);
int list_remove(list_t lst, WORD elem, int (*eq)(WORD,WORD));
int list_pop(list_t lst,int ix, WORD *res);
void list_clear(list_t lst);
void list_reverse(list_t lst);
int list_extend(list_t lst, list_t other); // 2nd par should be an Iterable

// Other list methods
list_t list_copy(list_t lst);
int list_sort(list_t lst, int (*cmp)(WORD,WORD)); // Python function has additional param bool reversed.

