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
  char *$GCINFO;
  long tb_size;        // size of dk_indices array; must be power of 2
  long tb_usable;      // nr of unused entries in tb_entries (deleted entries are counted as used)
  long tb_nentries;    // nr of used entries in tb_entries
  int  tb_indices[];   // array of indices
                       // after this follows tb_entries array;
};

struct $dict$class $dict$methods = {"", NULL, $dict_init, $dict_serialize,$dict_deserialize}; 


#define DKIX_EMPTY (-1)
#define DKIX_DUMMY (-2)  /* Used internally */
 #define TB_ENTRIES(tb) \
  (($entry_t)(&((int*)((tb)->tb_indices))[(tb)->tb_size]))

#define PERTURB_SHIFT 5

// Serialisation /////////////////////////////////////////////////////////////////////////

void $dict_serialize($dict self, $Mapping$dict wit, long *start_no, $dict done, struct $ROWLISTHEADER *accum) {
  $int prevkey = ($int)$dict_get(done,wit->w$Hashable$Mapping,self,NULL);
  if (prevkey) {
    $val_serialize(-DICT_ID,&prevkey->val,start_no,accum);
    return;
  }
  $dict_setitem(done,wit->w$Hashable$Mapping,self,to$int(*start_no));
  int blobsize = 4 + (self->table->tb_size + 1) * sizeof(int)/sizeof($WORD);
  $ROW row = $new_row(DICT_ID,start_no,blobsize,NULL);
  row->blob[0] = ($WORD)self->numelements;
  row->blob[1] = ($WORD)self->table->tb_size;
  row->blob[2] = ($WORD)self->table->tb_usable;
  row->blob[3] = ($WORD)self->table->tb_nentries;
  memcpy(&row->blob[4],self->table->tb_indices,self->table->tb_size*sizeof(int));
  $enqueue(accum,row);
  for (int i=0; i<self->table->tb_nentries; i++) {
    $entry_t entry = &TB_ENTRIES(self->table)[i];
    $step_serialize(($Serializable)to$int(entry->hash),wit,start_no,done,accum);
    $step_serialize(entry->key,wit,start_no,done,accum);
    $step_serialize(entry->value,wit,start_no,done,accum);
  }
}

$dict $dict_deserialize($Mapping$dict wit, $ROW *row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    return $dict_get(done,wit->w$Hashable$Mapping,to$int((long)this->blob[0]),NULL);
  } else {
    $dict res = malloc(sizeof(struct $dict));
    $dict_setitem(done,wit->w$Hashable$Mapping,to$int(this->row_no),res);
    res->$class = &$dict$methods;
    res->numelements = (long)this->blob[0];
    long tb_size = (long)this->blob[1];
    res->table = malloc(sizeof(char*) + 3*sizeof(long) + tb_size*sizeof(int) + (2*tb_size/3)*sizeof(struct $entry_struct));
    res->table->tb_size = tb_size;
    res->table->tb_usable = (long)this->blob[2];
    res->table->tb_nentries = (long)this->blob[3];
    memcpy(res->table->tb_indices,&this->blob[4],tb_size*sizeof(int));
    for (int i=0; i<res->table->tb_nentries; i++) {
      $entry_t entry = &TB_ENTRIES(res->table)[i];
      entry->hash = from$int(($int)$step_deserialize(wit,row,done));
      entry->key =  $step_deserialize(wit,row,done);
      entry->value = $step_deserialize(wit,row,done);
    }
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


void $dict_init($dict dict, $Hashable hashwit, $Iterable$opaque it) { 
  dict->numelements = 0;
  dict->table = malloc(sizeof(char*)+3*sizeof(long) + 8*sizeof(int) + 5*sizeof(struct $entry_struct));
  dict->table->tb_size = 8;
  dict->table->tb_usable = 5;
  dict->table->tb_nentries = 0;
  memset(&(dict->table->tb_indices[0]), 0xff, 8*sizeof(int));
  if (it) {
    $Iterator iter = it->proto->$class->__iter__(it->proto,it->impl);
    
    //try iterate and insert into dict, catching StopIteration
  }
}

//$Hashable $dict_hashwitness($dict dict) {
//  return dict->hashwit;
//}

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
static int lookdict($dict dict, $Hashable hashwit, long hash, $WORD key, $WORD *res) {
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
      if (entry->value != NULL && (entry->key == key || (entry->hash == hash && hashwit->$class->__eq__(hashwit,key,entry->key)))) {
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

static int insertdict($dict dict, $Hashable hashwit, long hash, $WORD key, $WORD value) {
  $WORD old_value;
  $table table;
  $entry_t ep;
  int ix = lookdict(dict,hashwit,hash,key,&old_value);
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

typedef struct $Iterator$dict {
  char *$GCINFO;
  $Super$class $superclass;
  $WORD(*__next__)($WORD self);
  $dict src;
  int nxt;
} *$Iterator$dict; 



static $WORD $dict_iterator_next($WORD self) {
  $Iterator$dict state = ($Iterator$dict) (($Iterator)self)->$class;
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
 
$Iterator $dict_iter($dict dict) {
  $Iterator$dict iter = malloc(sizeof(struct $Iterator$dict));
  iter->__next__ = $dict_iterator_next;
  iter->$superclass = ($Super$class)$Iterator$witness;
  iter->src = dict;
  iter->nxt = 0;
  $Iterator res = malloc(sizeof(struct $Iterator));
  res->$class = ($Iterator$class)iter;
  return res;
}
 
// Indexed ///////////////////////////////////////////////////////////////////////////////

void $dict_setitem($dict dict, $Hashable hashwit, $WORD key, $WORD value) {
  long hash = from$int(hashwit->$class->__hash__(hashwit,key));
  if (insertdict(dict, hashwit, hash, key, value)<0) {
    exception e;
    MKEXCEPTION(e,MEMORYERROR);
    RAISE(e);
  }      
}

$WORD $dict_getitem($dict dict, $Hashable hashwit, $WORD key) {
  long hash = from$int(hashwit->$class->__hash__(hashwit,key));
  $WORD res;
  int ix = lookdict(dict,hashwit,hash,key,&res);
  if (ix < 0)  {
    exception e;
    MKEXCEPTION(e,KEYERROR);
    RAISE(e);
  }      
  return res;
}


void $dict_delitem($dict dict, $Hashable hashwit, $WORD key) {
  long hash = from$int(hashwit->$class->__hash__(hashwit,key));
  $WORD res;
  int ix = lookdict(dict,hashwit,hash,key,&res);
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


$dict $dict_fromiter($Hashable hashwit, $Iterable$opaque it) {
  return $NEW($dict,hashwit,it);
}


// Container_Eq /////////////////////////////////////////////////////////////////////////////

int $dict_contains($dict dict, $Hashable hashwit, $WORD key) {
  $WORD res;
  return lookdict(dict,hashwit,from$int(hashwit->$class->__hash__(hashwit,key)),key,&res) >= 0;
}


// Mapping /////////////////////////////////////////////////////////////////////////////

static $WORD $dict_values_iterator_next($WORD self) {
  $Iterator$dict state = ($Iterator$dict) (($Iterator)self)->$class;
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
  $Iterator$dict state = ($Iterator$dict) (($Iterator)self)->$class;
  int i = state->nxt;
  $table table = state->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      state->nxt = i+1;
      $tup2_t res = malloc(sizeof(struct $tup2_t));
      res->$class = &$tup2_t$methods;
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


$Iterator $dict_keys($dict dict) {
  return $dict_iter(dict);
}

$Iterator $dict_values($dict dict) {
  $Iterator$dict iter = malloc(sizeof(struct $Iterator$dict));
  iter->$superclass = ($Super$class)$Iterator$witness;
  iter->__next__ = $dict_values_iterator_next;
  iter->src = dict;
  iter->nxt = 0;
  $Iterator res = malloc(sizeof(struct $Iterator));
  res->$class = ($Iterator$class)iter;
  return res;
}

$Iterator $dict_items($dict dict) {
  $Iterator$dict iter = malloc(sizeof(struct $Iterator$dict));
  iter->$superclass = ($Super$class)$Iterator$witness;
  iter->__next__ = $dict_items_iterator_next;
  iter->src = dict;
  iter->nxt = 0;
  $Iterator res = malloc(sizeof(struct $Iterator));
  res->$class = ($Iterator$class)iter;
  return res;
}
 
$WORD $dict_get($dict dict, $Hashable hashwit, $WORD key, $WORD deflt) {
  long hash = from$int(hashwit->$class->__hash__(hashwit,key));
  $WORD res;
  int ix = lookdict(dict,hashwit,hash,key,&res);
  if (ix < 0) 
    return deflt;
  else
    return res;
}

$WORD $dict_popitem($dict dict, $Hashable hashwit) {
  $table table = dict->table;
  int ix = table->tb_nentries-1;
  while (ix >= 0) {
    $entry_t entry =  &TB_ENTRIES(table)[ix];
    if (entry->value != NULL) {
      $tup2_t res = malloc(sizeof(struct $tup2_t));
      res->$class = &$tup2_t$methods;
      res->a = entry->key;
      res->b = entry->value;
      entry->value = NULL;
      long hash = from$int(hashwit->$class->__hash__(hashwit,entry->key));
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

void $dict_update($dict dict,  $Hashable hashwit, $Iterable$opaque it) {
  $Iterator iter = it->proto->$class->__iter__(it->proto,it->impl);
  $WORD item;
  while((item = iter->$class->__next__(iter)))
    $dict_setitem(dict,hashwit,(($tup2_t)item)->a,(($tup2_t)item)->b);
}

$WORD $dict_setdefault($dict dict, $Hashable hashwit, $WORD key, $WORD deflt) {
  // if (!deflt) deflt = void; what is the name of void here?...
  long hash = from$int(hashwit->$class->__hash__(hashwit,key));
  $WORD value;
  int ix = lookdict(dict,hashwit,hash,key,&value);
  if (ix >= 0)
    return value;
  TB_ENTRIES(dict->table)[ix].value = deflt;
  return deflt;
}
