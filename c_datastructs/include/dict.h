#ifndef DICT_H
#define DICT_H

#include "hash.h"
#include "iterator.h"

typedef void *WORD;

typedef struct entry_struct {
  size_t hash;
  WORD key;
  WORD value;  // deleted entry has value NULL
} *entry_t;

typedef struct table_struct {
  long tb_size;        // size of dk_indices array; power of 2
  long tb_usable;      // nr of unused entries in dk_entries (deleted entries are counted as used)
  long tb_nentries;    // nr of used entries in dk_entries
  int  tb_indices[];   // array of indices
                       // after this follows tb_entries array;
} *table_t;
  
typedef struct dict_struct {
  long numelements;    // nr of elements in dictionary
  Hashable h;          // eq and hash function used in this dictionary
  table_t table;       // the hashtable
} *dict_t;

#define DKIX_EMPTY (-1)
#define DKIX_DUMMY (-2)  /* Used internally */
// #define DKIX_ERROR (-3)

dict_t dict_new(Hashable h);
int dict_setitem(dict_t dict, WORD key, WORD value);
int dict_getitem(dict_t dict, WORD key, WORD *res);
int dict_contains(dict_t dict, WORD key);
iterator_t dict_iter(dict_t t);
int dict_pop(dict_t t, WORD key, WORD *res);

#endif
