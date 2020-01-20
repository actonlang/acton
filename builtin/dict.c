#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h> /*memset*/
#include <assert.h>

#include "builtin.h"

// Method tables /////////////////////////////////////////////////

Iterator $dict_iter_instance(Iterable$__class__ cl,$WORD self);

$WORD $dict_next_instance(Iterator$__class__ cl, $WORD self);

Collection $dict_fromiter_instance(Collection$__class__ cl, Iterable it);
$int $dict_len_instance(Collection$__class__ cl, $WORD self);

$WORD $dict_getitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix); 
void $dict_setitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix, $WORD val);
void $dict_delitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix);

$WORD $dict_get_instance(Mapping$__class__ cl, $WORD self, $WORD key, $WORD deflt);
Iterator $dict_keys_instance(Mapping$__class__ cl,$WORD self);
Iterator $dict_values_instance(Mapping$__class__ cl,$WORD self);
Iterator $dict_items_instance(Mapping$__class__ cl,$WORD self);
void $dict_update_instance(Mapping$__class__ cl,$WORD self, Mapping other);
$WORD $dict_popitem_instance(Mapping$__class__ cl,$WORD self);
$WORD $dict_setdefault_instance(Mapping$__class__ cl,$WORD self, $WORD key, $WORD deflt);

$bool $dict_contains_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem);
$bool $dict_containsnot_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem);


static struct Iterator$__class__ Iterator$dict_struct = {"GC_Iterator",$dict_next_instance};
Iterator$__class__ Iterator$dict_instance = &Iterator$dict_struct;

static struct Iterable$__class__ Iterable$dict_struct = {"GC_Iterable", $dict_iter_instance};
Iterable$__class__ Iterable$dict_instance = &Iterable$dict_struct;

static struct Collection$__class__ Collection$dict_struct = {"GC_Collection",&Iterable$dict_struct,$dict_fromiter_instance,$dict_len_instance};
Collection$__class__ Collection$dict_instance = &Collection$dict_struct;

static struct Indexed$__class__ Indexed$dict_struct = {"GC_Indexed", $dict_getitem_instance, $dict_setitem_instance, $dict_delitem_instance};
Indexed$__class__ Indexed$dict_instance = &Indexed$dict_struct;

static struct Mapping$__class__ Mapping$dict_struct = {"GC_Mapping", &Indexed$dict_struct, $dict_get_instance, $dict_keys_instance,
                                                       $dict_values_instance,  $dict_items_instance, $dict_update_instance, $dict_popitem_instance,
                                                       $dict_setdefault_instance};

Mapping$__class__ Mapping$dict_instance = &Mapping$dict_struct;

static struct Container_Eq$__class__ Container_Eq$dict_struct = {"GC_Container_Eq",&Collection$dict_struct, $dict_contains_instance,$dict_containsnot_instance,NULL};

Container_Eq$__class__ Container_Eq$dict_instance = &Container_Eq$dict_struct;

#define DKIX_EMPTY (-1)
#define DKIX_DUMMY (-2)  /* Used internally */
// #define DKIX_ERROR (-3)
#define TB_ENTRIES(tb) \
  (($entry_t)(&((int*)((tb)->tb_indices))[(tb)->tb_size]))

#define PERTURB_SHIFT 5

/*
Internal routine used by dictresize() to build a hashtable of entries.
*/
static void build_indices($table tbl, $entry_t ep, long n) {
  long mask = tbl->tb_size - 1;
  for (int ix = 0; ix != n; ix++, ep++) {
    long hash = ep->hash;

    long i = hash & mask;
    for (long perturb = hash; tbl->tb_indices[i] != DKIX_EMPTY;) {
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
*/

static int dictresize($dict_internal_t d) {
  $table oldtable = d->table;
  long numelements = d->numelements;
  long newsize, minsize = 3*numelements;
  $entry_t oldentries, newentries;

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
  $table newtable =  malloc(3*sizeof(long) + newsize*sizeof(int) + (2*newsize/3)*sizeof(struct $entry_struct));
  newtable->tb_size = newsize;
  newtable->tb_usable = 2*newsize/3-numelements;
  newtable->tb_nentries = numelements;
  memset(&(newtable->tb_indices[0]), 0xff, newsize*sizeof(int));
  oldentries = TB_ENTRIES(oldtable);
  newentries = TB_ENTRIES(newtable);

  if (oldtable->tb_nentries == numelements) {
    memcpy(newentries, oldentries, numelements*sizeof(struct $entry_struct));
  }
  else {
    $entry_t ep = oldentries;
    for (int i = 0; i < numelements; i++) {
      while (ep->value == NULL) ep++;
      newentries[i] = *ep++;
    }
  }
  d->table = newtable;
  free(oldtable);
  
  build_indices(newtable, newentries, numelements);

  return 0;
}


$dict $dict_new(Eq_Hashable$__class__ h) {
  $dict_internal_t dict =  malloc(sizeof(long) + 2*sizeof($WORD));
  dict->numelements = 0;
  dict->h = h;
  dict->table = malloc(3*sizeof(long) + 8*sizeof(int) + 5*sizeof(struct $entry_struct));
  dict->table->tb_size = 8;
  dict->table->tb_usable = 5;
  dict->table->tb_nentries = 0;
  memset(&(dict->table->tb_indices[0]), 0xff, 8*sizeof(int));
  $dict res = malloc(sizeof(struct $dict));
  res->__class__ = NULL;
  res->__internal__ = dict;
  return res;
}

// Search index of hash table from offset of entry table 
static int lookdict_index($table table, long hash, int index) {
    long mask =  (table->tb_size)-1;
    long perturb = hash;
    long i = hash & mask;

    for (;;) {
        int ix = table->tb_indices[i];
        if (ix == index) {
            return i;
        }
        if (ix == DKIX_EMPTY) {
            return DKIX_EMPTY;
        }
        perturb >>= PERTURB_SHIFT;
        i = mask & (i*5 + perturb + 1);
    }
    // unreachable
}

// Returns index into compact array where hash/key is found
// (and returns corresponding value in *res)
// or DKIX_EMPTY if no such entry exists
static int lookdict($dict_internal_t dict, long hash, $WORD key, $WORD *res) {
  $table table = dict->table;
  long mask = (table->tb_size)-1, i = hash & mask, perturb = hash;
  int ix;
  //  entry_t entry0 = TB_ENTRIES(table);
  for(;;) {
    ix = table->tb_indices[i];
    if (ix == DKIX_EMPTY) {
      // Unused slot
      *res = NULL;
      return ix;
    }
    if (ix >= 0) {
      $entry_t entry = &TB_ENTRIES(table)[ix];
      if (entry->value != NULL && (entry->key == key || (entry->hash == hash && dict->h->__eq__((Eq$__class__)dict->h,key,entry->key)))) {
        // found an entry with the same or equal key
        *res = entry->value;
        return ix;
      }
      // collision; probe another location
    }
    perturb >>= PERTURB_SHIFT;
    i = (i*5 + perturb + 1) & mask;
  }
  // this should be unreachable
}

 //  Internal function to find slot in index array for an item from its hash
 //  when it is known that the key is not present in the dict.
   


static long find_empty_slot($table table, long hash) {
  const long mask = (table->tb_size)-1;

  long i = hash & mask;
  int ix = table->tb_indices[i];
    for (long perturb = hash; ix >= 0;) {
        perturb >>= PERTURB_SHIFT;
        i = (i*5 + perturb + 1) & mask;
        ix = table->tb_indices[i];
    }
    return i;
}

static int insertdict($dict_internal_t dict, long hash, $WORD key, $WORD value) {
  $WORD old_value;
  $table table;
  $entry_t ep;
  int ix = lookdict(dict,hash,key,&old_value);
  if (ix == DKIX_EMPTY) {
    if (dict->table->tb_usable <= 0 && dictresize(dict) < 0)
        return -1;
    table = dict->table;
    long hashpos = find_empty_slot(table,hash);
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
// Iterable //////////////////////////////////////////////////////////////////////////////


dict_iterator_state_t $dict_state_of($dict dict) {
  dict_iterator_state_t state = malloc(sizeof(struct dict_iterator_struct));
  state->$GCINFO = "GC_State";
  state->src = dict->__internal__;
  state->nxt = 0;
  return state;
}

$WORD $dict_iterator_next(dict_iterator_state_t state) {
  int i = state->nxt;
  $table table = state->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      state->nxt = i+1;
      return entry->key;
    }
    i++;
  }
  exception e;
  MKEXCEPTION(e,KEYERROR);
  RAISE(e);
  return NULL;
}

// instance methods

Iterator $dict_iter_instance(Iterable$__class__ cl, $WORD self) {
  return Iterator$__pack__(Iterator$dict_instance,$dict_state_of(($dict)self));
}

$WORD $dict_next_instance(Iterator$__class__ cl, $WORD self) {
  return  $dict_iterator_next(self);
}

// Indexed ///////////////////////////////////////////////////////////////////////////////

void $dict_setitem($dict dict, $WORD key, $WORD value) {
  long hash = *dict->__internal__->h->__hash__(dict->__internal__->h,key);
  if (insertdict(dict->__internal__, hash, key, value)<0) {
    exception e;
    MKEXCEPTION(e,MEMORYERROR);
    RAISE(e);
  }      
}

$WORD $dict_getitem($dict dict, $WORD key) {
  long hash = *dict->__internal__->h->__hash__(dict->__internal__->h,key);
  $WORD res;
  int ix = lookdict(dict->__internal__,hash,key,&res);
  if (ix < 0)  {
    exception e;
    MKEXCEPTION(e,KEYERROR);
    RAISE(e);
  }      
  return res;
}


void $dict_delitem($dict dict,  $WORD key) {
  long hash = *dict->__internal__->h->__hash__(dict->__internal__->h,key);
  $WORD res;
  int ix = lookdict(dict->__internal__,hash,key,&res);
  $table table = dict->__internal__->table;
  if (ix >= 0) {
    $entry_t entry = &TB_ENTRIES(table)[ix];
    int i = lookdict_index(table,hash,ix);
    table->tb_indices[i] = DKIX_DUMMY;
    res = entry->value;
    if (res == NULL) {
      exception e;
      MKEXCEPTION(e,KEYERROR);
      RAISE(e);
    }
    entry->value = NULL;
    dict->__internal__->numelements--;
    /*
    downsizing does not guarantee LIFO order in popitem
    if (10*dict->__internal__->numelements < dict->__internal__->table->tb_size) {
      dictresize(dict->__internal__);
    */
  }
}

// instance methods

$WORD $dict_getitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix) {
  $WORD w = $dict_getitem(($dict)self, ix);
  return w;
}

void $dict_setitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix, $WORD val){
  $dict_setitem(($dict)self,ix,val);
}

void $dict_delitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix) {
  $dict_delitem(($dict)self,ix);
}


// Collection ///////////////////////////////////////////////////////////////////////////////


$int $dict_len($dict dict) {
  $int res = malloc(sizeof(long));
  *res = dict->__internal__->numelements;
  return res;
}

// What to do with from_iter? We need to have a Eq_Hashable instance to get started.

// instance methods

Collection $dict_fromiter_instance(Collection$__class__ cl, Iterable it) {
  exception e;
  MKEXCEPTION(e,NOTIMPLEMENTED);
  RAISE(e);
  return NULL;
}

$int $dict_len_instance(Collection$__class__ cl, $WORD self) {
  return $dict_len(($dict)self);
}

// Container_Eq /////////////////////////////////////////////////////////////////////////////

$bool $dict_contains($dict dict, $WORD key) {
  $WORD res;
  return lookdict(dict->__internal__,*dict->__internal__->h->__hash__(dict->__internal__->h,key),key,&res) >= 0;
}

$bool $dict_contains_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem) {
  return $dict_contains(($dict)self,elem);
}

$bool $dict_containsnot_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem) {
  return !$dict_contains(($dict)self,elem);
}

// Mapping /////////////////////////////////////////////////////////////////////////////


static $WORD $dict_values_iterator_next(dict_iterator_state_t state) {
  int i = state->nxt;
  $table table = state->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      state->nxt = i+1;
      return entry->value;
    }
    i++;
  }
  exception e;
  MKEXCEPTION(e,STOPITERATION);
  RAISE(e);
  return NULL;
}

static $WORD $dict_items_iterator_next(dict_iterator_state_t state) {
  int i = state->nxt;
  $table table = state->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      $item_t res = malloc(sizeof(struct $item_struct));
      res->key = entry->key;
      res->value = entry->value;
      state->nxt = i+1;
      return res;
    }
    i++;
  }
  exception e;
  MKEXCEPTION(e,STOPITERATION);
  RAISE(e);
  return NULL;
}


/*

// This is part of Sequence protocol.

static $WORD $dict_iterator_reversed_next(iterator_internal_t iter) {
  dict_iterator_state_t state = iter->state;
  int i = state->nxt;
  $table table = state->src->table;
  while (i >= 0) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      state->nxt = i-1;
      return entry->key;
    }
    i--;
  }
  exception e;
  MKEXCEPTION(e,STOPITERATION);
  RAISE(e);
  return NULL;
}

*/
// instance methods
 
// TODO: Handle KEYERROR and return deflt
$WORD $dict_get($dict dict, $WORD key, $WORD deflt) {
  return $dict_getitem(dict,key);
}

// Mapping methods

/*
pop is now a method in protocol Set

int dict_pop(dict_t dict, WORD key, WORD *res) {
  size_t hash = dict->h->hash(key);
  long ix = lookdict(dict,hash,key,res);
  table_t table = dict->table;
  if (ix >= 0) {
    entry_t entry = &TB_ENTRIES(table)[ix];
    int i = lookdict_index(table,hash,ix);
    table->tb_indices[i] = DKIX_DUMMY;
    // *res = entry->value;
    entry->value = NULL;
    dict->numelements--;
    return 0;
  } else {
    if (res == NULL) {
      return KEYERROR;
    } else { // *res already contains default value
      return 0;
    }
  }
}
*/
$WORD $dict_popitem($dict dict) {
  $dict_internal_t d = dict->__internal__;
  $table table = d->table;
  int ix = table->tb_nentries;
  while (ix >= 0) {
    $entry_t entry =  &TB_ENTRIES(table)[ix];
    if (entry->value != NULL) {
      $item_t res = malloc(sizeof(struct $item_struct));
      res->key = entry->key;
      res->value = entry->value;
      entry->value = NULL;
      long hash = *d->h->__hash__(d->h,entry->key);
      int i = lookdict_index(table,hash,ix);
      table->tb_indices[i] = DKIX_DUMMY;
      d->numelements--;
      table->tb_nentries = ix;
      return res;
    }
    ix--;
  }
  exception e;
  MKEXCEPTION(e,KEYERROR);
  RAISE(e);
  return NULL;
}

void $dict_update($dict dict, $dict other) {
  Iterator items = Mapping$dict_instance->items(Mapping$dict_instance,other);
  $WORD item;
  while((item = items->__class__->__next__( items->__class__,items->__impl__)))
    $dict_setitem(dict,(($item_t)item)->key,(($item_t)item)->value);
}

$WORD $dict_setdefault($dict dict, $WORD key, $WORD deflt) {
  // if (!deflt) deflt = None; what i the name of None here?...
  $dict_internal_t d = dict->__internal__;
  long hash = *d->h->__hash__(d->h,key);
  $WORD value;
  int ix = lookdict(d,hash,key,&value);
  if (ix >= 0)
    return value;
  TB_ENTRIES(d->table)[ix].value = deflt;
  return deflt;
}

// instance methods

$WORD $dict_next_values_instance(Iterator$__class__ cl, $WORD self) {
  return  $dict_values_iterator_next(self);
}

$WORD $dict_next_items_instance(Iterator$__class__ cl, $WORD self) {
  return  $dict_items_iterator_next(self);
}

Iterator $dict_keys_instance(Mapping$__class__ cl, $WORD self) {
  return Iterator$__pack__(Iterator$dict_instance,$dict_state_of(($dict)self));
}
Iterator $dict_values_instance(Mapping$__class__ cl,$WORD self) {
  Iterator$__class__ cl1 = malloc(sizeof(struct Iterator$__class__));
  cl1->$GCINFO = "GC_Iterator$__class__";
  cl1->__next__ = $dict_next_values_instance;
  return Iterator$__pack__(cl1,$dict_state_of(self));
}

Iterator $dict_items_instance(Mapping$__class__ cl,$WORD self) {
  Iterator$__class__ cl1 = malloc(sizeof(struct Iterator$__class__));
  cl1->$GCINFO = "GC_Iterator$__class__";
  cl1->__next__ = $dict_next_items_instance;
  return Iterator$__pack__(cl1,$dict_state_of(self));
}


$WORD $dict_get_instance(Mapping$__class__ cl, $WORD self, $WORD key, $WORD deflt) {
  return $dict_get(self,key,deflt);
}

$WORD $dict_popitem_instance(Mapping$__class__ cl, $WORD self) {
  return $dict_popitem(self);
}

void $dict_update_instance(Mapping$__class__ cl, $WORD self, Mapping other) {
  // Note:  This assumes other is a dict!!
    $dict_update(self, other->__impl__);
}

$WORD $dict_setdefault_instance(Mapping$__class__ cl, $WORD self, $WORD key, $WORD deflt) {
  return $dict_setdefault(self, key, deflt);
}
