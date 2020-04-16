#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h> /*memset*/
#include <assert.h>

#include "builtin.h"

// types //////////////////////////////////////////////////////////////////////////////////////

typedef struct $entry_struct {
  long hash;
  $WORD key;
  $WORD value;  // deleted entry has value NULL
} *$entry_t;

struct $table_struct {
  char *GCINFO;
  long tb_size;        // size of dk_indices array; must be power of 2
  long tb_usable;      // nr of unused entries in tb_entries (deleted entries are counted as used)
  long tb_nentries;    // nr of used entries in tb_entries
  int  tb_indices[];   // array of indices
                       // after this follows tb_entries array;
};

struct $dict$__methods__ $dict_struct = {$dict_serialize,$dict_deserialize,$dict_hashwitness}; 

$dict$__methods__ $dict_methods = &$dict_struct;

#define DKIX_EMPTY (-1)
#define DKIX_DUMMY (-2)  /* Used internally */
 #define TB_ENTRIES(tb) \
  (($entry_t)(&((int*)((tb)->tb_indices))[(tb)->tb_size]))

#define PERTURB_SHIFT 5

// Serialisation /////////////////////////////////////////////////////////////////////////

None $dict_serialize($dict self, $WORD *prefix, int prefix_size, $dict done, $ROWLISTHEADER accum) {
  $WORD deflt = NULL;
  $PREFIX prevkey = ($PREFIX)$dict_get(done,self,deflt);
  int this_blobsize = 5 + (self->table->tb_size + 1) * sizeof(int)/sizeof($WORD);
  int blob_size = prevkey ? prevkey->prefix_size : this_blobsize;
  $ROW row = new_row(DICT_ID,prefix_size,blob_size,prefix);
  if (prevkey) {
    row->class_id = -DICT_ID;
    memcpy(row->data + prefix_size,prevkey->prefix,prevkey->prefix_size*sizeof($WORD));
    enqueue(accum,row);
    return;
  }
  $PREFIX pref = malloc(sizeof(int) + prefix_size*sizeof($WORD));
  pref->prefix_size = prefix_size;
  memcpy(pref->prefix, prefix, prefix_size*sizeof($WORD));
  $dict_setitem(done,self,pref);
  row->class_id = DICT_ID;
  row->data[prefix_size] = ($WORD)self->numelements;
  row->data[prefix_size+1] = ($WORD)from$int(self->hashwit->__class__->__keyinfo__(self->hashwit));
  row->data[prefix_size+2] = ($WORD)self->table->tb_size;
  row->data[prefix_size+3] = ($WORD)self->table->tb_usable;
  row->data[prefix_size+4] = ($WORD)self->table->tb_nentries;
  memcpy(&row->data[prefix_size+5],self->table->tb_indices,self->table->tb_size*sizeof(int));
  enqueue(accum,row);
  int extprefix_size = prefix_size + 1;
  for (long i=0; i<self->table->tb_nentries; i++) {
    $WORD extprefix[extprefix_size];
    memcpy(extprefix, prefix, prefix_size*sizeof($WORD));
    extprefix[extprefix_size-1] = ($WORD)i;
    $ROW row2 = new_row(ITEM_ID,extprefix_size,1,extprefix);
    $entry_t entry = &TB_ENTRIES(self->table)[i];
    row2->data[extprefix_size] = ($WORD)entry->hash;
    enqueue(accum,row2);
    int extprefix2_size = extprefix_size + 1;
    $WORD extprefix2[extprefix2_size];
    memcpy(extprefix2, extprefix, extprefix_size*sizeof($WORD));
    extprefix2[extprefix2_size-1] = ($WORD)0;
    Serializable key = (Serializable)entry->key;
    key->__class__->__serialize__(key,extprefix2,extprefix2_size,done,accum);
    extprefix2[extprefix2_size-1] = ($WORD)1;
    Serializable val =  (Serializable)entry->value;
    if (val==NULL) 
      enqueue(accum,new_row(DUMMY_ID,extprefix2_size,0,extprefix2));
    else 
      val->__class__->__serialize__(val,extprefix2,extprefix2_size,done,accum);
  }
}

$dict $dict_deserialize($ROW *row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    $PREFIX pref = malloc(sizeof(int) + this->blob_size*sizeof($WORD));
    pref->prefix_size = this->blob_size;
    memcpy(pref->prefix, this->data+this->prefix_size, this->blob_size*sizeof($WORD));
    return $dict_get(done,pref,NULL);
  } else {
    $dict res = malloc(sizeof(struct $dict));
    res->__class__ = $dict_methods;
    res->numelements = (long)this->data[this->prefix_size];
    res->hashwit = Hashable_instance((long)this->data[this->prefix_size+1]);
    long tb_size = (long)this->data[this->prefix_size+2];
    res->table = malloc(sizeof(char*) + 3*sizeof(long) + tb_size*sizeof(int) + (2*tb_size/3)*sizeof(struct $entry_struct));
    res->table->tb_size = tb_size;
    res->table->tb_usable = (long)this->data[this->prefix_size+3];
    res->table->tb_nentries = (long)this->data[this->prefix_size+4];
    memcpy(res->table->tb_indices,&this->data[this->prefix_size+5],tb_size*sizeof(int));
    for (int i=0; i<res->table->tb_nentries; i++) {
      $entry_t entry = &TB_ENTRIES(res->table)[i];
      entry->hash = (long)(*row)->data[(*row)->prefix_size];
      *row = (*row)->next;
      entry->key = (serial$_methods[labs((*row)->class_id)])->__deserialize__(row,done);
      if ((*row)->class_id == DUMMY_ID)
        entry->value = NULL;
      else {
        entry->value = (serial$_methods[labs((*row)->class_id)])->__deserialize__(row,done);
      }
    }
    $PREFIX pref = malloc(sizeof(int) + this->prefix_size*sizeof($WORD));
    pref->prefix_size = this->prefix_size;
    memcpy(pref->prefix, this->data, this->prefix_size*sizeof($WORD));
    $dict_setitem(done,pref,res);
    return res;
  }
}


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

static int dictresize($dict d) {
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
  $table newtable =  malloc(sizeof(char*) + 3*sizeof(long) + newsize*sizeof(int) + (2*newsize/3)*sizeof(struct $entry_struct));
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


$dict $new_dict(Hashable hashwit) { 
  $dict dict =  malloc(sizeof(struct $dict));
  dict->__class__ = $dict_methods;
  dict->numelements = 0;
  dict->hashwit = hashwit;
  dict->table = malloc(sizeof(char*)+3*sizeof(long) + 8*sizeof(int) + 5*sizeof(struct $entry_struct));
  dict->table->tb_size = 8;
  dict->table->tb_usable = 5;
  dict->table->tb_nentries = 0;
  memset(&(dict->table->tb_indices[0]), 0xff, 8*sizeof(int));
  return dict;
}

Hashable $dict_hashwitness($dict dict) {
  return dict->hashwit;
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
static int lookdict($dict dict, long hash, $WORD key, $WORD *res) {
  $table table = dict->table;
  long mask = (table->tb_size)-1, i = hash & mask, perturb = hash;
  int ix;
  for(;;) {
    ix = table->tb_indices[i];
    if (ix == DKIX_EMPTY) {
      // Unused slot
      *res = NULL;
      return ix;
    }
    if (ix >= 0) {
      $entry_t entry = &TB_ENTRIES(table)[ix];
      if (entry->value != NULL && (entry->key == key || (entry->hash == hash && dict->hashwit->__class__->__eq__(dict->hashwit,key,entry->key)))) {
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

static int insertdict($dict dict, long hash, $WORD key, $WORD value) {
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

typedef struct Iterator$dict {
  char *$GCINFO;
  $WORD(*__next__)($WORD self);
  $dict src;
  int nxt;
} *Iterator$dict; 



static $WORD $dict_iterator_next($WORD self) {
  Iterator$dict state = (Iterator$dict) ((Iterator)self)->__class__;
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
 
Iterator $dict_iter($dict dict) {
  Iterator$dict iter = malloc(sizeof(struct Iterator$dict));
  iter->__next__ = $dict_iterator_next;
  iter->src = dict;
  iter->nxt = 0;
  Iterator res = malloc(sizeof(struct Iterator));
  res->__class__ = (Iterator$__class__)iter;
  return res;
}
 
// Indexed ///////////////////////////////////////////////////////////////////////////////

void $dict_setitem($dict dict, $WORD key, $WORD value) {
  long hash = from$int(dict->hashwit->__class__->__hash__(dict->hashwit,key));
  if (insertdict(dict, hash, key, value)<0) {
    exception e;
    MKEXCEPTION(e,MEMORYERROR);
    RAISE(e);
  }      
}

$WORD $dict_getitem($dict dict, $WORD key) {
  long hash = from$int(dict->hashwit->__class__->__hash__(dict->hashwit,key));
  $WORD res;
  int ix = lookdict(dict,hash,key,&res);
  if (ix < 0)  {
    exception e;
    MKEXCEPTION(e,KEYERROR);
    RAISE(e);
  }      
  return res;
}


void $dict_delitem($dict dict,  $WORD key) {
  long hash = from$int(dict->hashwit->__class__->__hash__(dict->hashwit,key));
  $WORD res;
  int ix = lookdict(dict,hash,key,&res);
  $table table = dict->table;
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
    dict->numelements--;
    if (10*dict->numelements < dict->table->tb_size) 
      dictresize(dict);
    }
}

// Collection ///////////////////////////////////////////////////////////////////////////////


long $dict_len($dict dict) {
  return dict->numelements;
}


$dict $dict_fromiter(Hashable hashwit, Iterator it) {
  $dict res = $new_dict(hashwit);
  if (it==NULL)
    return res;
  $dict_update(res, it);
  return res;
}


// Container_Eq /////////////////////////////////////////////////////////////////////////////

int $dict_contains($dict dict, $WORD key) {
  $WORD res;
  return lookdict(dict,from$int(dict->hashwit->__class__->__hash__(dict->hashwit,key)),key,&res) >= 0;
}


// Mapping /////////////////////////////////////////////////////////////////////////////

static $WORD $dict_values_iterator_next($WORD self) {
  Iterator$dict state = (Iterator$dict) ((Iterator)self)->__class__;
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

static $WORD $dict_items_iterator_next($WORD self) {
  Iterator$dict state = (Iterator$dict) ((Iterator)self)->__class__;
  int i = state->nxt;
  $table table = state->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      state->nxt = i+1;
      $tup2_t res = malloc(sizeof(struct $tup2_t));
      res->a = entry->key;
      res->b = entry->value;
      return res;
    }
    i++;
  }
  exception e;
  MKEXCEPTION(e,STOPITERATION);
  RAISE(e);
  return NULL;
}


Iterator $dict_keys($dict dict) {
  return $dict_iter(dict);
}

Iterator $dict_values($dict dict) {
  Iterator$dict iter = malloc(sizeof(struct Iterator$dict));
  iter->__next__ = $dict_values_iterator_next;
  iter->src = dict;
  iter->nxt = 0;
  Iterator res = malloc(sizeof(struct Iterator));
  res->__class__ = (Iterator$__class__)iter;
  return res;
}

Iterator $dict_items($dict dict) {
  Iterator$dict iter = malloc(sizeof(struct Iterator$dict));
  iter->__next__ = $dict_items_iterator_next;
  iter->src = dict;
  iter->nxt = 0;
  Iterator res = malloc(sizeof(struct Iterator));
  res->__class__ = (Iterator$__class__)iter;
  return res;
}
 
$WORD $dict_get($dict dict, $WORD key, $WORD deflt) {
  long hash = from$int(dict->hashwit->__class__->__hash__(dict->hashwit,key));
  $WORD res;
  int ix = lookdict(dict,hash,key,&res);
  if (ix < 0) 
    return deflt;
  else
    return res;
}

$WORD $dict_popitem($dict dict) {
  $table table = dict->table;
  int ix = table->tb_nentries-1;
  while (ix >= 0) {
    $entry_t entry =  &TB_ENTRIES(table)[ix];
    if (entry->value != NULL) {
      $tup2_t res = malloc(sizeof(struct $tup2_t));
      res->a = entry->key;
      res->b = entry->value;
      entry->value = NULL;
      long hash = from$int(dict->hashwit->__class__->__hash__(dict->hashwit,entry->key));
      int i = lookdict_index(table,hash,ix);
      table->tb_indices[i] = DKIX_DUMMY;
      dict->numelements--;
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

void $dict_update($dict dict, Iterator it) {
  $WORD item;
  while((item = it->__class__->__next__(it)))
    $dict_setitem(dict,(($tup2_t)item)->a,(($tup2_t)item)->b);
}

$WORD $dict_setdefault($dict dict, $WORD key, $WORD deflt) {
  // if (!deflt) deflt = None; what is the name of None here?...
  long hash = from$int(dict->hashwit->__class__->__hash__(dict->hashwit,key));
  $WORD value;
  int ix = lookdict(dict,hash,key,&value);
  if (ix >= 0)
    return value;
  TB_ENTRIES(dict->table)[ix].value = deflt;
  return deflt;
}
