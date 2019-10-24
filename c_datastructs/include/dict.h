#ifndef DICT_H
#define DICT_H

#include "hash.h"
#include "iterator.h"

typedef void *WORD;

struct dict_struct;

typedef  struct dict_struct *dict_t;

typedef struct item_struct {
  WORD key;
  WORD value;
} *item_t;

dict_t dict_new(Hashable h);
int dict_setitem(dict_t dict, WORD key, WORD value);
int dict_getitem(dict_t dict, WORD key, WORD *res);
int dict_contains(dict_t dict, WORD key);
iterator_t dict_iter(dict_t dict); // same as dict_keys
iterator_t dict_keys(dict_t dict);
iterator_t dict_values(dict_t dict);
iterator_t dict_items(dict_t dict);     
iterator_t dict_reversed(dict_t t);
int dict_pop(dict_t t, WORD key, WORD *res);
int dict_len(dict_t dict);
int dict_get(dict_t dict, WORD key, WORD *res); // Never fails; res unchanged if key not present.
int dict_popitem(dict_t dict, WORD *res); // returns an item_t
void dict_update(dict_t dict, dict_t other);
#endif
