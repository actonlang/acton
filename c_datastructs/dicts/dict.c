#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h> /*memset*/
#include <assert.h>
#include <errno.h>

#include "dict.h"
#include "hash.h"
#include "iterator.h"

#define TB_ENTRIES(tb) \
  ((entry_t)(&((int*)((tb)->tb_indices))[(tb)->tb_size]))

#define PERTURB_SHIFT 5

/*
Internal routine used by dictresize() to build a hashtable of entries.
*/
static void build_indices(table_t tbl, entry_t ep, size_t n) {
  size_t mask = (size_t)(tbl->tb_size - 1);
  for (int ix = 0; ix != n; ix++, ep++) {
    size_t hash = ep->hash;
    size_t i = hash & mask;
    for (size_t perturb = hash; tbl->tb_indices[i] != DKIX_EMPTY;) {
      perturb >>= PERTURB_SHIFT;
      i = mask & (i*5 + perturb + 1);
    }
    tbl->tb_indices[i] = ix;
  }
}

/*
Restructure the table by allocating a new table and reinserting all
items again.  When entries have been deleted, the new table may
actually be smaller than the old one.
If a table is split (its keys and hashes are shared, its values are not),
then the values are temporarily copied into the table, it is resized as
a combined table, then the me_value slots in the old table are NULLed out.
After resizing a table is always combined,
but can be resplit by make_keys_shared().
*/

static int dictresize(dict_t dict) {
  table_t oldtable = dict->table;
  long numelements = dict->numelements;
  long newsize, minsize = 3*numelements;
  entry_t oldentries, newentries;

  for (newsize = 8; newsize < minsize; //&& newsize > 0; // ignore case when minsize is so large that newsize overflows
       newsize <<= 1)
    ;
  /*
  Again, for the moment, ignore enormous dictionary size request.
  if (newsize <= 0) {
        PyErr_NoMemory();
        return -1;
    }
  */
  /* Allocate a new table. */
  table_t newtable =  malloc(3*sizeof(long) + newsize*sizeof(int) + (2*newsize/3)*sizeof(struct entry_struct));
  newtable->tb_size = newsize;
  newtable->tb_usable = 2*newsize/3-numelements;
  newtable->tb_nentries = numelements;
  memset(&(newtable->tb_indices[0]), 0xff, newsize*sizeof(int));
  oldentries = TB_ENTRIES(oldtable);
  newentries = TB_ENTRIES(newtable);

  if (oldtable->tb_nentries == numelements) {
    memcpy(newentries, oldentries, numelements*sizeof(struct entry_struct));
  }
  else {
    entry_t ep = oldentries;
    for (int i = 0; i < numelements; i++) {
      while (ep->value == NULL) ep++;
      newentries[i] = *ep++;
    }
  }
  dict->table = newtable;
  free(oldtable);
  
  build_indices(newtable, newentries, numelements);
  return 0;
}

  
dict_t dict_new(Hashable h) {
  dict_t dict =  malloc(sizeof(long) + 2*sizeof(WORD));
  dict->numelements = 0;
  dict->h = h;
  dict->table = malloc(3*sizeof(long) + 8*sizeof(int) + 5*sizeof(struct entry_struct));
  dict->table->tb_size = 8;
  dict->table->tb_usable = 5;
  dict->table->tb_nentries = 0;
  memset(&(dict->table->tb_indices[0]), 0xff, 8*sizeof(int));
  return dict;
}

// Returns index into compact array where hash/key is found or DKIX_EMPTY
// if no such entry exists
static int lookdict(dict_t dict, size_t hash, WORD key, WORD *valueaddr) {
  table_t table = dict->table;
  size_t mask = (table->tb_size)-1, i = (size_t)hash & mask, perturb = hash;
  entry_t entry0 = TB_ENTRIES(table);
  for(;;) {
    int ix = table->tb_indices[i];
    if (ix == DKIX_EMPTY) {
      // Unused slot
      *valueaddr = NULL;
      return ix;
    }
    if (ix >= 0) {
      entry_t entry = &entry0[ix];
      if (entry->key == key) {
        // found an entry with the same key object
        *valueaddr = entry->value;
        return ix;
      }
      if (entry->hash == hash && dict->h->eq(key,entry->key)) { 
        *valueaddr = entry->value;
        return ix;
      }
      // collision; probe another location
    }
    perturb >>= PERTURB_SHIFT;
    i = (i*5 + perturb + 1) & mask;
  }
  // this should be unreachable
}
/* 
   Internal function to find slot in index array for an item from its hash
   when it is known that the key is not present in the dict.
   
*/

static size_t find_empty_slot(table_t table, size_t hash) {
  const size_t mask = (table->tb_size)-1;
  size_t i = hash & mask;
  int ix = table->tb_indices[i]; //dictkeys_get_index(dict, i);
    for (size_t perturb = hash; ix >= 0;) {
        perturb >>= PERTURB_SHIFT;
        i = (i*5 + perturb + 1) & mask;
        ix = table->tb_indices[i];
    }
    return i;
}

static int insertdict(dict_t dict, size_t hash, WORD key, WORD value) {
  WORD old_value;
  table_t table;
  entry_t ep;
  int ix = lookdict(dict,hash,key,&old_value);
  if (ix == DKIX_EMPTY) {
    if (dict->table->tb_usable <= 0)
      if (dictresize(dict) < 0)
        return -1;
    table = dict->table;
    size_t hashpos = find_empty_slot(table,hash);
    ep = &TB_ENTRIES(table)[table->tb_nentries];
    table->tb_indices[hashpos] = table->tb_nentries;
    ep->key = key;
    ep->hash = hash;
    ep->value = value;
    table->tb_usable--;
    table->tb_nentries++;
    dict->numelements++;
    return 0;
  }
  if (old_value != value)  //eq ??
    TB_ENTRIES(table)[ix].value = value;
  return 0;
}

int dict_setitem(dict_t dict, WORD key, WORD value) {
  size_t hash = dict->h->hash(key);
  if (hash==-1)
    return -1;
  return insertdict(dict, hash, key, value);
}
 
int dict_getitem(dict_t dict, WORD key, WORD *res) {
  size_t hash = dict->h->hash(key);
  long ix = lookdict(dict,hash,key,res); 
  if (ix >= 0) {
    table_t table = dict->table;
    entry_t entry = &TB_ENTRIES(table)[table->tb_indices[ix]];
  }
  return ix >= 0 ? 0 : -1;
}


int dict_contains(dict_t dict, WORD key) {
  WORD res;
  return lookdict(dict, dict->h->hash(key),key,&res) >= 0;
}

typedef struct dict_iterator_struct {
  dict_t src;
  int nxt;
} *dict_iterator_state_t; 

int dict_iterator_next(iterator_t iter, WORD *res) {
  dict_iterator_state_t state = iter->state;
  int i = state->nxt;
  table_t table = state->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      *res = entry->key;
      state->nxt = i+1;
      return 0;
    }
    i++;
  }
  *res = NULL;
  errno = EINVAL;
  return -1;
}

iterator_t dict_iter(dict_t dict) {
  table_t table = dict->table;
  dict_iterator_state_t state = malloc(sizeof(struct dict_iterator_struct));
  state->src = dict;
  state->nxt = 0;
  iterator_t iter =  malloc(sizeof(struct dict_iterator_struct));
  iter->state = state;
  iter->next = dict_iterator_next;
  return iter;
}
  
int dict_pop(dict_t dict, WORD key, WORD *res) {
  long ix = lookdict(dict, dict->h->hash(key),key,res);
  if (ix >= 0) {
    entry_t entry = &TB_ENTRIES(dict->table)[ix];
    *res = entry->value;
    entry->value = NULL;
    dict->numelements--;
    return 0;
  } else {
    if (res == NULL) {
      errno = EINVAL;
      return -1;
    } else { // *res already contains return value
      return 0;
    }
  }
}
