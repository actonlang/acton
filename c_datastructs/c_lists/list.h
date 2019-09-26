#ifndef LIST_H
#define LIST_H

typedef void *WORD;

typedef struct list_struct {
  WORD *data;
  int length;
  int capacity;
} *list_t;

typedef struct list_iterator_struct {
  list_t src;
  int nxt;
} *list_iterator_t; 

typedef struct range_iterator_struct {
  int nxt,stop,step;
} *range_iterator_t; 


list_t list_new(int capacity);

// Container method
int list_contains(list_t lst, WORD elem, int (*eq)(WORD,WORD));

// Iterable method
list_iterator_t list_iter(list_t lst);  

// Sized method
int list_len(list_t lst);

// Reversible method
list_iterator_t list_reversed(list_t lst);

// Sequence methods 
WORD list_getitem(list_t lst, int ix);
int list_index(list_t lst, WORD elem, int startix, int endix,int (*eq)(WORD,WORD));
int list_count(list_t lst, WORD elem, int (*eq)(WORD,WORD));

// MutableSequence methods
int list_setitem(list_t lst, int ix, WORD elem);
int list_append(list_t lst, WORD elem);
int list_insert(list_t lst, int ix, WORD elem);
int list_remove(list_t lst, WORD elem, int (*eq)(WORD,WORD));
WORD list_pop(list_t lst,int ix);
void list_clear(list_t lst);
void list_reverse(list_t lst);
int list_extend(list_t lst, list_t other); // 2nd par should be an Iterable

// Other list methods
list_t list_copy(list_t lst);
int list_sort(list_t lst, int (*cmp)(WORD,WORD)); // Python function has additional param bool reversed.

// Iterator methods
WORD list_iterator_next(list_iterator_t iter);
int range_iterator_next(range_iterator_t iter);

// Creating a range iterator

range_iterator_t range(int start, int stop, int step);

// Variants when using non-pointer elems
int list_getitem_p(list_t lst, int ix, WORD *res);
int list_pop_p(list_t lst,int ix, WORD *res);
int list_iterator_next_p(list_iterator_t iter, WORD *res);
int range_iterator_next_p(range_iterator_t iter, int *res);

#endif
