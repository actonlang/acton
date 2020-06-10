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



#define DKIX_EMPTY (-1)
#define DKIX_DUMMY (-2)  /* Used internally */
#define TB_ENTRIES(tb) \
  (($entry_t)(&((int*)((tb)->tb_indices))[(tb)->tb_size]))

#define PERTURB_SHIFT 5

// General methods /////////////////////////////////////////////////////////////////////////


void $dict_init($dict dict, $Hashable hashwit, $Mapping$opaque m) { 
  dict->numelements = 0;
  dict->table = malloc(sizeof(char*)+3*sizeof(long) + 8*sizeof(int) + 5*sizeof(struct $entry_struct));
  dict->table->tb_size = 8;
  dict->table->tb_usable = 5;
  dict->table->tb_nentries = 0;
  memset(&(dict->table->tb_indices[0]), 0xff, 8*sizeof(int));
  if (m) {
    $Iterator iter = m->proto->$class->__iter__(m->proto,m->impl);
    $tuple nxt;
    while((nxt = ($tuple)iter->$class->__next__(iter))) {
      $dict_setitem(dict,hashwit,nxt->components[0],nxt->components[1]);
    }
  }
}

$bool $dict_bool($dict self) {
  return to$bool(self->numelements>0);
}

$str $dict_str($dict self) {
  $list s2 = $list_new(self->numelements);
  $Iterator$dict$items iter = $NEW($Iterator$dict$items,self);
  $tuple item;
  for (int i=0; i<self->numelements; i++) {
    item = ($tuple)iter->$class->__next__(iter);
    $struct key = (($struct)item->components[0]);
    $struct value = (($struct)item->components[1]);
    $str keystr = key->$class->__str__(key);
    $str valuestr = value->$class->__str__(value);
    $str elem = malloc(sizeof(struct $str));
    elem->$class = &$str$methods;
    elem->nbytes = keystr->nbytes+valuestr->nbytes+1;
    elem->nchars = keystr->nchars+valuestr->nchars+1;
    elem->str = malloc(elem->nbytes+1);
    memcpy(elem->str,keystr->str,keystr->nbytes);
    elem->str[keystr->nbytes] = ':';
    memcpy(&elem->str[keystr->nbytes+1],valuestr->str,valuestr->nbytes);
    elem->str[elem->nbytes] = '\0';    
    $list_append(s2,elem);
  }
  return $str_join_par('{',s2,'}');
}

void $dict_serialize($dict self,$Serial$state state) {
  $int prevkey = ($int)$dict_get(state->done,($Hashable)$Hashable$WORD$witness,self,NULL);
  if (prevkey) {
    $val_serialize(-DICT_ID,&prevkey->val,state);
    return;
  }
  $dict_setitem(state->done,($Hashable)$Hashable$WORD$witness,self,to$int(state->row_no));
  int blobsize = 4 + (self->table->tb_size + 1) * sizeof(int)/sizeof($WORD);
  $ROW row = $add_header(DICT_ID,blobsize,state);
  row->blob[0] = ($WORD)self->numelements;
  row->blob[1] = ($WORD)self->table->tb_size;
  row->blob[2] = ($WORD)self->table->tb_usable;
  row->blob[3] = ($WORD)self->table->tb_nentries;
  memcpy(&row->blob[4],self->table->tb_indices,self->table->tb_size*sizeof(int));
  for (int i=0; i<self->table->tb_nentries; i++) {
    $entry_t entry = &TB_ENTRIES(self->table)[i];
    $step_serialize(to$int(entry->hash),state);
    $step_serialize(entry->key,state);
    $step_serialize(entry->value,state);
  }
}

$dict $dict_deserialize($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row_no++;
  if (this->class_id < 0) {
    return $dict_get(state->done,($Hashable)$Hashable$int$witness,to$int((long)this->blob[0]),NULL);
  } else {
    $dict res = malloc(sizeof(struct $dict));
    $dict_setitem(state->done,($Hashable)$Hashable$int$witness,to$int(state->row_no-1),res);
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
      entry->hash = from$int(($int)$step_deserialize(state));
      entry->key =  $step_deserialize(state);
      entry->value = $step_deserialize(state);
    }
    return res;
  }
}

struct $dict$class $dict$methods = {"", UNASSIGNED,($Super$class)&$object$methods, $dict_init, $dict_serialize,$dict_deserialize, $dict_bool, $dict_str}; 

// Internal routines //////////////////////////////////////////////////////////////////

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
    TB_ENTRIES(dict->table)[ix].value = value;
  return 0;
}
// Iterable //////////////////////////////////////////////////////////////////////////////
 
static $WORD $Iterator$dict_next($Iterator$dict self) {
  int i = self->nxt;
  $table table = self->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      self->nxt = i+1;
      return entry->key;
    }
    i++;
  }
  return NULL;
}
 
void $Iterator$dict_init($Iterator$dict self, $dict dict) {
  self->src = dict;
  self->nxt = 0;
}


$bool $Iterator$dict_bool($Iterator$dict self) {
  return $True;
}

$str $Iterator$dict_str($Iterator$dict self) {
  char *s;
  asprintf(&s,"<dict keys iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$dict_serialize($Iterator$dict self, $Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}


$Iterator$dict $Iterator$dict$_deserialize($Serial$state state) {
   $Iterator$dict res = $DNEW( $Iterator$dict,state);
   res->src = ($dict)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}


struct $Iterator$dict$class $Iterator$dict$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$dict_init,
                                                      $Iterator$dict_serialize, $Iterator$dict$_deserialize, $Iterator$dict_bool,$Iterator$dict_str, $Iterator$dict_next};

$Iterator $dict_iter($dict dict) {
  return ($Iterator)$NEW($Iterator$dict,dict);
}

// Indexed ///////////////////////////////////////////////////////////////////////////////

void $dict_setitem($dict dict, $Hashable hashwit, $WORD key, $WORD value) {
  long hash = from$int(hashwit->$class->__hash__(hashwit,key));
  if (insertdict(dict, hashwit, hash, key, value)<0) {
    RAISE(($BaseException)$NEW($IndexError,to$str("getitem: key not in dictionary")));
  }      
}

$WORD $dict_getitem($dict dict, $Hashable hashwit, $WORD key) {
  long hash = from$int(hashwit->$class->__hash__(hashwit,key));
  $WORD res;
  int ix = lookdict(dict,hashwit,hash,key,&res);
  if (ix < 0)  {
    RAISE(($BaseException)$NEW($IndexError,to$str("setitem: key not in dictionary")));
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
      RAISE(($BaseException)$NEW($IndexError,to$str("setitem: key not in dictionary")));
    }
    entry->value = NULL;
    dict->numelements--;
    if (10*dict->numelements < dict->table->tb_size) 
      dictresize(dict);
    }
}

// Collection ///////////////////////////////////////////////////////////////////////////////

$dict $dict_fromiter($Hashable hashwit, $Iterable$opaque iter) {
  $dict dict = $NEW($dict,hashwit,NULL);
  dict->numelements = 0;
  dict->table = malloc(sizeof(char*)+3*sizeof(long) + 8*sizeof(int) + 5*sizeof(struct $entry_struct));
  dict->table->tb_size = 8;
  dict->table->tb_usable = 5;
  dict->table->tb_nentries = 0;
  memset(&(dict->table->tb_indices[0]), 0xff, 8*sizeof(int));
  if (iter) {
    $Iterator it = iter->proto->$class->__iter__(iter->proto,iter->impl);
    $tuple nxt;
    while((nxt = ($tuple)it->$class->__next__(it))) {
      $dict_setitem(dict,hashwit,nxt->components[0],nxt->components[1]);
    }
  }
  return dict;
}

long $dict_len($dict dict) {
  return dict->numelements;
}

// Container_Eq /////////////////////////////////////////////////////////////////////////////

int $dict_contains($dict dict, $Hashable hashwit, $WORD key) {
  $WORD res;
  return lookdict(dict,hashwit,from$int(hashwit->$class->__hash__(hashwit,key)),key,&res) >= 0;
}

// Mapping /////////////////////////////////////////////////////////////////////////////

// values iterator

static $WORD $Iterator$dict$values_next($Iterator$dict$values self) {
  int i = self->nxt;
  $table table = self->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      self->nxt = i+1;
      return entry->value;
    }
    i++;
  }
  return NULL;
}
 
void $Iterator$dict$values_init($Iterator$dict$values self, $dict dict) {
  self->src = dict;
  self->nxt = 0;
}


$bool $Iterator$dict$values_bool($Iterator$dict$values self) {
  return $True;
}

$str $Iterator$dict$values_str($Iterator$dict$values self) {
  char *s;
  asprintf(&s,"<dict values iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$dict$values_serialize($Iterator$dict$values self, $Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}

$Iterator$dict$values $Iterator$dict$values_deserialize($Serial$state state) {
   $Iterator$dict$values res = $DNEW($Iterator$dict$values,state);
   res->src = ($dict)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}



struct $Iterator$dict$values$class $Iterator$dict$values$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$dict$values_init,
                                                                    $Iterator$dict$values_serialize, $Iterator$dict$values_deserialize, $Iterator$dict$values_bool, $Iterator$dict$values_str,
                                                                    $Iterator$dict$values_next};

// items iterator

static $WORD $Iterator$dict$items_next($Iterator$dict$items self) {
  int i = self->nxt;
  $table table = self->src->table;
  int n = table->tb_nentries;
  while (i < n) {
    $entry_t entry =  &TB_ENTRIES(table)[i];
    if (entry->value != NULL) {
      self->nxt = i+1;
      $WORD *comps = malloc(2*sizeof($WORD));
      comps[0] = entry->key;
      comps[1] = entry->value;
      return $NEW($tuple,2,comps);
    }
    i++;
  }
  return NULL;
}
 
void $Iterator$dict$items_init($Iterator$dict$items self, $dict dict) {
  self->src = dict;
  self->nxt = 0;
}


$bool $Iterator$dict$items_bool($Iterator$dict$items self) {
  return $True;
}

$str $Iterator$dict$items_str($Iterator$dict$items self) {
  char *s;
  asprintf(&s,"<dict items iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$dict$items_serialize($Iterator$dict$items self, $Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}

$Iterator$dict$items $Iterator$dict$items_deserialize($Serial$state state) {
   $Iterator$dict$items res = $DNEW($Iterator$dict$items,state);
   res->src = ($dict)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}



struct $Iterator$dict$items$class $Iterator$dict$items$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$dict$items_init,
                                                                  $Iterator$dict$items_serialize, $Iterator$dict$items_deserialize,$Iterator$dict$items_bool, $Iterator$dict$items_str, $Iterator$dict$items_next};


$Iterator $dict_keys($dict dict) {
  return ($Iterator)$NEW($Iterator$dict,dict);
}

$Iterator $dict_values($dict dict) {
  return ($Iterator)$NEW($Iterator$dict$values, dict);
}

$Iterator $dict_items($dict dict) {
  return  ($Iterator)$NEW($Iterator$dict$items, dict);
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

$tuple $dict_popitem($dict dict, $Hashable hashwit) {
  $table table = dict->table;
  int ix = table->tb_nentries-1;
  while (ix >= 0) {
    $entry_t entry =  &TB_ENTRIES(table)[ix];
    if (entry->value != NULL) {
      $WORD *comps = malloc(2*sizeof($WORD));
      comps[0] = entry->key;
      comps[1] = entry->value;
      long hash = from$int(hashwit->$class->__hash__(hashwit,entry->key));
      int i = lookdict_index(table,hash,ix);
      table->tb_indices[i] = DKIX_DUMMY;
      dict->numelements--;
      table->tb_nentries = ix;
      return $NEW($tuple,2,comps);
    }
    ix--;
  }
  return NULL;
}

void $dict_update($dict dict,  $Hashable hashwit, $Iterable$opaque it) {
  $Iterator iter = it->proto->$class->__iter__(it->proto,it->impl);
  $tuple item;
  while((item = ($tuple)iter->$class->__next__(iter)))
    $dict_setitem(dict,hashwit,item->components[0],item->components[1]);
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
