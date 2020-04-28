#include <string.h> /*memset*/

#define DISCARD_NOTFOUND 0
#define DISCARD_FOUND 1
#include "builtin.h"

/* 
This implementation of sets is an adaptation of CPython's set implementation.

*/

// Types ////////////////////////////////////////////////////////////////////////////////////////////////////////////////



// Maybe we should  offer union, intersection and symmetric difference under those names.
struct $set$class $set$methods = {"",(void (*)($set))$default__init__,$set_serialize,$set_deserialize,$set_copy}; 


#define PERTURB_SHIFT 5
#define MIN_SIZE 8

static $WORD _dummy;
#define dummy (&_dummy)

static void $set_insert_clean($setentry *table, long mask, $WORD *key, long hash) {
    $setentry *entry;
    long perturb = hash;
    long i = hash & mask;
    long j;

    while (1) {
        entry = &table[i];
        if (entry->key == NULL)
            goto found_null;
        
        perturb >>= PERTURB_SHIFT;
        i = (i * 5 + 1 + perturb) & mask;
    }
  found_null:
    entry->key = key;
    entry->hash = hash;
}

static int $set_table_resize($set so, int minsize) {
    $setentry *oldtable, *newtable, *entry;
    long oldmask = so->mask;
    long newmask;

    /* Find the smallest table size > minsize. */
    long newsize = MIN_SIZE;
    while (newsize <= (long)minsize) {
        newsize <<= 1; // The largest possible value is PY_SSIZE_T_MAX + 1.
    }
    /* Get space for a new table. */
    oldtable = so->table;

    newtable = malloc(sizeof($setentry) * newsize);
    if (newtable == NULL) {
      return -1;
    }

    /* Make the $set empty, using the new table. */
    memset(newtable, 0, sizeof($setentry) * newsize);
    so->mask = newsize - 1;
    so->table = newtable;

    /* Copy the data over; 
       dummy entries aren't copied over, of course */
    newmask = (long)so->mask;
    if (so->fill == so->numelements) {
        for (entry = oldtable; entry <= oldtable + oldmask; entry++) {
            if (entry->key != NULL) {
                $set_insert_clean(newtable, newmask, entry->key, entry->hash);
            }
        }
    } else {
        so->fill = so->numelements;
        for (entry = oldtable; entry <= oldtable + oldmask; entry++) {
            if (entry->key != NULL && entry->key != dummy) {
                $set_insert_clean(newtable, newmask, entry->key, entry->hash);
            }
        }
    }

    free(oldtable);
    return 0;
}

static $setentry *$set_lookkey($set set, $Hashable hashwit, $WORD key, long hash) {
    $setentry *entry;
    long perturb;
    long mask = set->mask;
    long i = hash & mask;

    entry = &set->table[i];
    if (entry->key == NULL)
        return entry;

    perturb = hash;

    while (1) {
        if (entry->hash == hash) {
            $WORD *startkey = entry->key;
            // startkey cannot be a dummy because the dummy hash field is -1 
            // assert(startkey != dummy);
            if (startkey == key || hashwit->$class->__eq__(hashwit,startkey,key))
              return entry;
        }
        perturb >>= PERTURB_SHIFT;
        i = (i * 5 + 1 + perturb) & mask;

        entry = &set->table[i];
        if (entry->key == NULL)
            return entry;
    }
}

static int $set_contains_entry($set set,  $Hashable hashwit, $WORD elem, long hash) {
  return $set_lookkey(set, hashwit, elem, hash)->key != NULL;
}

$set $set_new() {
  $set res = malloc(sizeof(struct $set));
  // $set res = malloc(sizeof(char*) + 4*sizeof(long)+sizeof(Hashable$class)+sizeof($setentry*));
  res->numelements = 0;
  res->fill = 0;
  res->mask = MIN_SIZE-1;
  res->finger = 0;
  res->table = malloc(MIN_SIZE*sizeof($setentry));
  memset(res->table,0,MIN_SIZE*sizeof($setentry));
  res->$class = &$set$methods;
  return res; 
}


static void $set_add_entry($set set, $Hashable hashwit, $WORD key, long hash) {
  $setentry *freeslot;
  $setentry *entry;
  long perturb;
  long mask;
  long i;  
  mask = set->mask;
  i = hash & mask;

  entry = &set->table[i];
  if (entry->key == NULL)
    goto found_unused;

  freeslot = NULL;
  perturb = hash;

  while (1) {
    if (entry->hash == hash) {
      $WORD startkey = entry->key;
      // startkey cannot be a dummy because the dummy hash field is -1 
      if (startkey == key || hashwit->$class->__eq__(hashwit,startkey,key))
          goto found_active;
          }
      else if (entry->hash == -1)
        freeslot = entry;

      perturb >>= PERTURB_SHIFT;
      i = (i * 5 + 1 + perturb) & mask;

      entry = &set->table[i];
      if (entry->key == NULL)
        goto found_unused_or_dummy;
    }

  found_unused_or_dummy:
    if (freeslot == NULL)
      goto found_unused;
    set->numelements++;
    freeslot->key = key;
    freeslot->hash = hash;
    return;

  found_unused:
    set->fill++;
    set->numelements++;
    entry->key = key;
    entry->hash = hash;
    if ((size_t)set->fill*5 < mask*3)
      return;
    $set_table_resize(set, set->numelements>50000 ? set->numelements*2 : set->numelements*4);
    return;
  found_active:
    return;
}


$set $set_copy($set set) {
  $set res = malloc(sizeof(*set));
  memcpy(res,set,sizeof(*set));
  return res;
}

static int $set_discard_entry($set set, $Hashable hashwit, $WORD elem, long hash) {
  $setentry *entry = $set_lookkey(set,hashwit,elem, hash);
  if (entry->key != NULL) {
    entry->key = dummy;
    entry->hash = -1;
    set->numelements--;
    return DISCARD_FOUND;
  } else
    return DISCARD_NOTFOUND;
}

// Eq //////////////////////////////////////////////////////////////////////////////////////////////


int $set_eq($Hashable hashwit, $set set, $set other) {
  if (set == other) 
    return 1;
  if (set->numelements != other->numelements)
    return 0;
  $Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = $next(iter))){
    if(!$set_contains_entry(set, hashwit, (($setentry*)w)->key, (($setentry*)w)->hash))
      return 0;
  }
  return 1;
}

// Ord /////////////////////////////////////////////////////////////////////////////////////////////

int $set_ge($Hashable hashwit, $set set, $set other) {
  if (set == other) 
    return 1;    
  if (set->numelements < other->numelements)
    return 0;
  $Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = $next(iter))){
    if(!$set_contains_entry(set, hashwit, (($setentry*)w)->key, (($setentry*)w)->hash))
      return 0;
  }
  return 1;
}

int $set_gt($Hashable hashwit, $set set, $set other) {
  if (set == other) 
    return 0;    
  if (set->numelements <= other->numelements)
    return 0;
  $Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = $next(iter))){
    if(!$set_contains_entry(set, hashwit, (($setentry*)w)->key, (($setentry*)w)->hash))
      return 0;
  }
  return 1;
}

int $set_le($Hashable hashwit, $set set, $set other) {
  return $set_ge(hashwit,other,set);
}

int $set_lt($Hashable hashwit, $set set, $set other) {
  return $set_gt(hashwit,other,set);
}

// Logical /////////////////////////////////////////////////////////////////////////////////////////////


$set $set_intersection($Hashable hashwit, $set set, $set other) {
  if ($set_len(other) > $set_len(set))
    return $set_intersection(hashwit,other,set);
  $set res = $set_new();
  $Iterator iter = $set_iter_entry(set);
  $WORD w;
  while((w = $next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    if ($set_contains_entry(other,hashwit,key,hash))
      $set_add_entry(res,hashwit,key,hash);
  }
  return res;
}


$set $set_union($Hashable hashwit, $set set, $set other) {
  if ($set_len(other) > $set_len(set))
    return $set_union(hashwit,other,set);
  $set res = $set_copy(set);
  $Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = $next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    $set_add_entry(res,hashwit,key,hash);
  }
  return res;
}

$set $set_symmetric_difference($Hashable hashwit, $set set, $set other) {
  $set res = $set_copy(set);
  $Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = $next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    if(!$set_discard_entry(res,hashwit,key,hash))
      $set_add_entry(res,hashwit,key,hash);
  }
  return res;
}

// Set /////////////////////////////////////////////////////////////////////////////////////////////

void $set_add($set set, $Hashable hashwit,  $WORD elem) {
  $set_add_entry(set,hashwit,elem,from$int(hashwit->$class->__hash__(hashwit,elem)));
}


void $set_discard($set set, $Hashable hashwit, $WORD elem) {
  $set_discard_entry(set,hashwit,elem,from$int(hashwit->$class->__hash__(hashwit,elem)));
}

void $set_remove($set set, $Hashable hashwit, $WORD elem) {
  long hash = from$int(hashwit->$class->__hash__(hashwit,elem));
  if($set_discard_entry(set,hashwit,elem,hash))
    return;
  else {
      exception e;
      MKEXCEPTION(e,KEYERROR);
      RAISE(e);
  }
}

$WORD $set_pop($set set) {
  $WORD res;
  // Make sure the search finger is in bounds 
  $setentry *entry = set->table + (set->finger & set->mask);
  $setentry *limit = set->table + set->mask;

  if (set->numelements == 0) {
    exception e;
    MKEXCEPTION(e,KEYERROR);
    RAISE(e);
    return NULL;
  }
  while (entry->key == NULL || entry->key==dummy) {
    entry++;
    if (entry > limit)
      entry = set->table;
  }
  res = entry->key;
  entry->key = dummy;
  entry->hash = -1;
  set->numelements--;
  set->finger = entry - set->table + 1;   // next place to start 
  return res;
}

int $set_isdisjoint($Hashable hashwit, $set set, $set other) {
  if (set == other) 
    return set->numelements == 0;
  if (other->numelements > set->numelements)
    return $set_isdisjoint(hashwit,other,set);
  $Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = $next(iter))){
    if($set_contains_entry(set, hashwit,(($setentry*)w)->key, (($setentry*)w)->hash))
      return 0;
  }
  return 1;
}

// Collection /////////////////////////////////////////////////////////////////////////////////////////////

$set $set_fromiter($Hashable wit, $Iterator it) {
  $set res = $set_new();
  if (it==NULL)
    return res;
  $WORD elem;
  while((elem = $next(it)))
    $set_add(res,wit,elem);
  return res;
}

long $set_len($set set) {
  return set->numelements;
}

// Container_Eq ///////////////////////////////////////////////////////////////////////////////////////

int $set_contains($set set, $Hashable hashwit, $WORD elem) {
  return $set_contains_entry(set,hashwit,elem,from$int(hashwit->$class->__hash__(hashwit,elem)));
}
 
// Iterable ///////////////////////////////////////////////////////////////////////////////////////

typedef struct $Iterator$set {
  char *$GCINFO;
  $WORD(*__next__)($WORD self);
  $set src;
  int nxt;
} *$Iterator$set; 
 
static $WORD $set_iterator_next_entry($WORD self) {
  $Iterator$set state = ($Iterator$set) (($Iterator)self)->$class;
  $setentry *table = state->src->table;
  long n = state->src->mask;
  long i = state->nxt;
  while (i <= n) {
    $setentry *entry = &table[i];
    if (entry->key != NULL && entry->key != dummy) {
      state->nxt = i+1;
      return entry;
    }
    i++;
  }
  exception e;
  MKEXCEPTION(e,STOPITERATION);
  RAISE(e);
  return NULL;
}

$WORD $set_iterator_next($WORD self) {
  $WORD res;
  if((res = $set_iterator_next_entry(self))) {
    return (($setentry*)res)->key;
  } 
  exception e;
  MKEXCEPTION(e,STOPITERATION);
  RAISE(e);
  return NULL;
}

$Iterator $set_iter($set set) {
  $Iterator$set iter = malloc(sizeof(struct $Iterator$set));
  iter->__next__ = $set_iterator_next;
  iter->src = set;
  iter->nxt = 0;
  $Iterator res = malloc(sizeof(struct $Iterator));
  res->$class = ($Iterator$class)iter;
  return res;
}

$Iterator $set_iter_entry($set set) {
  $Iterator$set iter = malloc(sizeof(struct $Iterator$set));
  iter->__next__ = $set_iterator_next_entry;
  iter->src = set;
  iter->nxt = 0;
  $Iterator res = malloc(sizeof(struct $Iterator));
  res->$class = ($Iterator$class)iter;
  return res;
}

 
// Minus ///////////////////////////////////////////////////////////////////////////////////////////

$set $set_difference($Hashable hashwit, $set set, $set other) {
  $set res = $set_copy(set);
  $Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = $next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    $set_discard_entry(res,hashwit,key,hash);
  }
  return res;
}
 
// Serialization ///////////////////////////////////////////////////////////////////////////////////

void $set_serialize($set self, $Mapping$dict wit, long *start_no, $dict done, $ROWLISTHEADER accum) {
  $int prevkey = ($int)$dict_get(done,wit->_Hashable,self,NULL);
  if (prevkey) {
    $enqueue(accum,$new_row(-LIST_ID,start_no,1,($WORD)&prevkey->val));
    return;
  }
  $dict_setitem(done,wit->_Hashable,self,to$int(*start_no));
  $ROW row = $new_row(SET_ID,start_no,4,NULL);
  row->blob[0] = ($WORD)self->numelements;
  row->blob[1] = ($WORD)self->fill;
  row->blob[2] = ($WORD)self->mask;
  row->blob[3] = ($WORD)self->finger;
  $enqueue(accum,row);
  for (long i=0; i<=self->mask; i++) {
    $setentry *entry = &self->table[i];
    if (entry->key == NULL) {
        $enqueue(accum, $new_row(DUMMY_ID,start_no,0,NULL));
    } else {
      // When key is NULL we store a DUMMY_ID row.
      // Otherwise we serialize the hash followed by a serialization of the key.
      $Serializable hash = ($Serializable)to$int(entry->hash);
      hash->$class->__serialize__(hash,wit,start_no,done,accum);
      $Serializable key = ($Serializable)entry->key;
      key->$class->__serialize__(key,wit,start_no,done,accum);
    }
  }
}
 
$set $set_deserialize($Mapping$dict wit, $ROW *row, $dict done) {
  $ROW this = *row;
  *row = this->next;
  if (this->class_id < 0) {
    return $dict_get(done,wit->_Hashable,to$int((long)this->blob[0]),NULL);
  } else {
    $set res = malloc(sizeof(struct $set));
    $dict_setitem(done,wit->_Hashable,to$int(this->row_no),res);
    res->$class = &$set$methods;
    res->numelements = (long)this->blob[0];
    res->fill = (long)this->blob[1];
    res->mask = (long)this->blob[2];
    res->finger = (long)this->blob[3];
    res->table = malloc((res->mask+1)*sizeof($setentry));
    memset(res->table,0,(res->mask+1)*sizeof($setentry));
    for (int i=0; i<=res->mask;i++) {
      $setentry *entry = &res->table[i];
      if ((*row)->class_id == DUMMY_ID) {
         *row = (*row)->next;
      } else { 
        entry->hash = (long)(*row)->blob[0];
        *row = (*row)->next;
        entry->key = $get_methods(abs((*row)->class_id))->__deserialize__(wit,row,done);
        if (entry->hash==-1)
          entry->key = dummy;
      }
    }
    return res;
  }
}
