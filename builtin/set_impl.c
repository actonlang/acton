#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <string.h> /*memset*/

#include "builtin.h"

#define DISCARD_NOTFOUND 0
#define DISCARD_FOUND 1

/* 
This implementation of sets is an adaptation of CPython's set implementation.
*/

// Types ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
  char *$GCINFO;
  $WORD key;
  long hash;    
} $setentry;

typedef void *$set$__methods__;       // All set methods are from protocols
// Maybe we should  offer union, intersection and symmetric difference under those names.

typedef struct $set {
  char *$GCINFO;
  $set$__methods__ __class__;
  long numelements;    // nr of elements in $set
  long fill;           // numelements + #dummy entries
    /* The table contains mask + 1 slots, and that's a power of 2.
     * We store the mask instead of the size because the mask is more
     * frequently needed.
     */
  long mask;
  long finger;                       // Search finger for pop() 
  Hashable hashwit;             // eq and hash function used in this $set
  $setentry *table;                  // the hashtable
} *$set;

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

    /* Find the smallest table size > minused. */
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

static $setentry *$set_lookkey($set set, $WORD key, long hash) {
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
            if (startkey == key || set->hashwit->__class__->__eq__(set->hashwit,startkey,key))
              return entry;
        }
        perturb >>= PERTURB_SHIFT;
        i = (i * 5 + 1 + perturb) & mask;

        entry = &set->table[i];
        if (entry->key == NULL)
            return entry;
    }
}

static int $set_contains_entry($set set, $WORD elem, long hash) {
  return $set_lookkey(set, elem, hash)->key != NULL;
}

$set $set_new(Hashable h) {
  $set res = malloc(sizeof(struct $set));
  // $set res = malloc(sizeof(char*) + 4*sizeof(long)+sizeof(Hashable$__class__)+sizeof($setentry*));
  res->numelements = 0;
  res->fill = 0;
  res->mask = MIN_SIZE-1;
  res->finger = 0;
  res->hashwit = h;
  res->table = malloc(MIN_SIZE*sizeof($setentry));
  memset(res->table,0,MIN_SIZE*sizeof($setentry));
  res->__class__ = NULL;
  return res; 
}


static void $set_add_entry($set set, $WORD key, long hash) {
  $setentry *freeslot;
  $setentry *entry;
  long perturb;
  long mask;
  long i;                       // Unsigned for defined overflow behavior 
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
      if (startkey == key || set->hashwit->__class__->__eq__(set->hashwit,startkey,key))
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


static $set $set_copy($set set) {
  $set res = $set_new(set->hashwit);
  Iterator iter = $set_iter_entry(set);
  $WORD w;
  while((w = next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    $set_add_entry(res,key,hash);
  }
  return res;
}

static int $set_discard_entry($set set, $WORD elem, long hash) {
  $setentry *entry = $set_lookkey(set, elem, hash);
  if (entry->key != NULL) {
    entry->key = dummy;
    entry->hash = -1;
    set->numelements--;
    return DISCARD_FOUND;
  } else
    return DISCARD_NOTFOUND;
}


// Eq //////////////////////////////////////////////////////////////////////////////////////////////


int $set_eq($set set, $set other) {
  if (set == other) 
    return 1;
  if (set->numelements != other->numelements)
    return 0;
  Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = next(iter))){
    if(!$set_contains_entry(set, (($setentry*)w)->key, (($setentry*)w)->hash))
      return 0;
  }
  return 1;
}

// Ord /////////////////////////////////////////////////////////////////////////////////////////////

int $set_ge($set set, $set other) {
  if (set == other) 
    return 1;    
  if (set->numelements < other->numelements)
    return 0;
  Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = next(iter))){
    if(!$set_contains_entry(set, (($setentry*)w)->key, (($setentry*)w)->hash))
      return 0;
  }
  return 1;
}

int $set_gt($set set, $set other) {
  if (set == other) 
    return 0;    
  if (set->numelements <= other->numelements)
    return 0;
  Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = next(iter))){
    if(!$set_contains_entry(set, (($setentry*)w)->key, (($setentry*)w)->hash))
      return 0;
  }
  return 1;
}

int $set_le($set set, $set other) {
  return $set_ge(other,set);
}

int $set_lt($set set, $set other) {
  return $set_gt(other,set);
}

// Logical /////////////////////////////////////////////////////////////////////////////////////////////


$set $set_intersection($set set, $set other) {
  if ($set_len(other) > $set_len(set))
    return $set_intersection(other,set);
  $set res = $set_new(set->hashwit);
  Iterator iter = $set_iter_entry(set);
  $WORD w;
  while((w = next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    if ($set_contains_entry(other,key,hash))
      $set_add_entry(res,key,hash);
  }
  return res;
}


$set $set_union($set set, $set other) {
  if ($set_len(other) > $set_len(set))
    return $set_union(other,set);
  $set res = $set_copy(set);
  Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    $set_add_entry(res,key,hash);
  }
  return res;
}

$set $set_symmetric_difference($set set, $set other) {
  $set res = $set_copy(set);
  Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    if(!$set_discard_entry(res,key,hash))
      $set_add_entry(res,key,hash);
  }
  return res;
}

// Set /////////////////////////////////////////////////////////////////////////////////////////////

void $set_add($set set, $WORD elem) {
  $set_add_entry(set,elem,from$int(set->hashwit->__class__->__hash__(set->hashwit,elem)));
}


void $set_discard($set set, $WORD elem) {
  $set_discard_entry(set,elem,from$int(set->hashwit->__class__->__hash__(set->hashwit,elem)));
}

void $set_remove($set set, $WORD elem) {
  long hash = from$int(set->hashwit->__class__->__hash__(set->hashwit,elem));
  if($set_discard_entry(set,elem,hash))
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

int $set_isdisjoint($set set, $set other) {
  if (set == other) 
    return set->numelements == 0;
  if (other->numelements > set->numelements)
    return $set_isdisjoint(other,set);
  Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = next(iter))){
    if($set_contains_entry(set, (($setentry*)w)->key, (($setentry*)w)->hash))
      return 0;
  }
  return 1;
}

// Collection /////////////////////////////////////////////////////////////////////////////////////////////

$set $set_fromiter(Hashable wit, Iterator it) {
  $set res = $set_new(wit);
  if (it==NULL)
    return res;
  $WORD elem;
  while((elem = it->__class__->__next__(it)))
    $set_add(res,elem);
  return res;
}

long $set_len($set set) {
  return set->numelements;
}

// Container_Eq ///////////////////////////////////////////////////////////////////////////////////////

int $set_contains($set set, $WORD elem) {
  return $set_contains_entry(set,elem,from$int(set->hashwit->__class__->__hash__(set->hashwit,elem)));
}
 
// Iterable ///////////////////////////////////////////////////////////////////////////////////////

typedef struct Iterator$set {
  char *$GCINFO;
  $WORD(*__next__)($WORD self);
  $set src;
  int nxt;
} *Iterator$set; 
 
static $WORD $set_iterator_next_entry($WORD self) {
  Iterator$set state = (Iterator$set) ((Iterator)self)->__class__;
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

Iterator $set_iter($set set) {
  Iterator$set iter = malloc(sizeof(struct Iterator$set));
  iter->__next__ = $set_iterator_next;
  iter->src = set;
  iter->nxt = 0;
  Iterator res = malloc(sizeof(struct Iterator));
  res->__class__ = (Iterator$__class__)iter;
  return res;
}

Iterator $set_iter_entry($set set) {
  Iterator$set iter = malloc(sizeof(struct Iterator$set));
  iter->__next__ = $set_iterator_next_entry;
  iter->src = set;
  iter->nxt = 0;
  Iterator res = malloc(sizeof(struct Iterator));
  res->__class__ = (Iterator$__class__)iter;
  return res;
}

 
// Minus ///////////////////////////////////////////////////////////////////////////////////////////

$set $set_difference($set set, $set other) {
  $set res = $set_copy(set);
  Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    $set_discard_entry(res,key,hash);
  }
  return res;
}
 
