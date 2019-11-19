#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <string.h> /*memset*/
#include "set.h"
#include "hash.h"
#include "iterator.h"
#include "acterror.h"

typedef struct {
    WORD key;
    size_t hash;    
} setentry;

struct set_struct {
  long numelements;    // nr of elements in set
  long fill;           // numelements + #dummy entries
    /* The table contains mask + 1 slots, and that's a power of 2.
     * We store the mask instead of the size because the mask is more
     * frequently needed.
     */
  long mask;
  long finger;         // Search finger for pop() 
  Hashable h;          // eq and hash function used in this set
  setentry *table;      // the hashtable
};

#define PERTURB_SHIFT 5
#define MIN_SIZE 8

static WORD _dummy;
#define dummy (&_dummy)

static void set_insert_clean(setentry *table, size_t mask, WORD *key, size_t hash)
{
    setentry *entry;
    size_t perturb = hash;
    size_t i = (size_t)hash & mask;
    size_t j;

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

static int set_table_resize(set_t so, int minsize) {
    setentry *oldtable, *newtable, *entry;
    size_t oldmask = so->mask;
    size_t newmask;

    /* Find the smallest table size > minused. */
    /* XXX speed-up with intrinsics */
    size_t newsize = MIN_SIZE;
    while (newsize <= (size_t)minsize) {
        newsize <<= 1; // The largest possible value is PY_SSIZE_T_MAX + 1.
    }
    /* Get space for a new table. */
    oldtable = so->table;

    newtable = malloc(sizeof(setentry) * newsize);
    if (newtable == NULL) {
      return -1;
    }

    /* Make the set empty, using the new table. */
    memset(newtable, 0, sizeof(setentry) * newsize);
    so->mask = newsize - 1;
    so->table = newtable;

    /* Copy the data over; this is refcount-neutral for active entries;
       dummy entries aren't copied over, of course */
    newmask = (size_t)so->mask;
    if (so->fill == so->numelements) {
        for (entry = oldtable; entry <= oldtable + oldmask; entry++) {
            if (entry->key != NULL) {
                set_insert_clean(newtable, newmask, entry->key, entry->hash);
            }
        }
    } else {
        so->fill = so->numelements;
        for (entry = oldtable; entry <= oldtable + oldmask; entry++) {
            if (entry->key != NULL && entry->key != dummy) {
                set_insert_clean(newtable, newmask, entry->key, entry->hash);
            }
        }
    }

    free(oldtable);
    return 0;
}

static setentry *set_lookkey(set_t set, WORD key, size_t hash) {
    setentry *entry;
    size_t perturb;
    size_t mask = set->mask;
    size_t i = (size_t)hash & mask;

    entry = &set->table[i];
    if (entry->key == NULL)
        return entry;

    perturb = hash;

    while (1) {
        if (entry->hash == hash) {
            WORD *startkey = entry->key;
            // startkey cannot be a dummy because the dummy hash field is -1 
            // assert(startkey != dummy);
            if (startkey == key || set->h->eq(startkey,key))
              return entry;
        }
        perturb >>= PERTURB_SHIFT;
        i = (i * 5 + 1 + perturb) & mask;

        entry = &set->table[i];
        if (entry->key == NULL)
            return entry;
    }
}

set_t set_new(Hashable h) {
  set_t res = malloc(4*sizeof(long)+sizeof(Hashable)+sizeof(setentry*));
  res->numelements = 0;
  res->fill = 0;
  res->mask = MIN_SIZE-1;
  res->finger = 0;
  res->h = h;
  res->table = malloc(MIN_SIZE*sizeof(setentry));
  memset(res->table,0,MIN_SIZE*sizeof(setentry));
  return res;
}



static void set_add_entry(set_t set, WORD *key, size_t hash) {
  setentry *freeslot;
  setentry *entry;
  size_t perturb;
  size_t mask;
  size_t i;                       /* Unsigned for defined overflow behavior */
  mask = set->mask;
  i = (size_t)hash & mask;

  entry = &set->table[i];
  if (entry->key == NULL)
    goto found_unused;

  freeslot = NULL;
  perturb = hash;

  while (1) {
    if (entry->hash == hash) {
      WORD *startkey = entry->key;
      /* startkey cannot be a dummy because the dummy hash field is -1 */
      //assert(startkey != dummy);
      if (startkey == key || set->h->eq(startkey,key))
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
    set_table_resize(set, set->numelements>50000 ? set->numelements*2 : set->numelements*4);
    return;
  found_active:
    return;
}

void set_add(set_t s, WORD elem) {
  set_add_entry(s,elem,s->h->hash(elem));
}

int set_len(set_t set) {
  return set->numelements;
}

static int set_contains_entry(set_t s, WORD elem, size_t hash) {
  return set_lookkey(s, elem, hash)->key != NULL;
}

int set_contains(set_t s, WORD elem) {
  return set_contains_entry(s,elem,s->h->hash(elem));
}

#define DISCARD_NOTFOUND 0
#define DISCARD_FOUND 1

int set_discard_entry(set_t s, WORD elem, size_t hash) {
  setentry *entry = set_lookkey(s, elem, hash);
  if (entry->key != NULL) {
    entry->key = dummy;
    entry->hash = -1;
    s->numelements--;
    return DISCARD_FOUND;
  } else
    return DISCARD_NOTFOUND;
}

void set_discard(set_t s, WORD elem) {
  set_discard_entry(s,elem,s->h->hash(elem));
}

int set_remove(set_t s, WORD elem) {
  size_t hash = s->h->hash(elem);
  if(set_discard_entry(s,elem,hash))
    return 0;
  else {
    return KEYERROR;
  }
}

int set_pop(set_t s, WORD *res) {
    /* Make sure the search finger is in bounds */
    setentry *entry = s->table + (s->finger & s->mask);
    setentry *limit = s->table + s->mask;

    if (s->numelements == 0) {
      *res = NULL;
      return KEYERROR;
    }
    while (entry->key == NULL || entry->key==dummy) {
        entry++;
        if (entry > limit)
            entry = s->table;
    }
    *res = entry->key;
    entry->key = dummy;
    entry->hash = -1;
    s->numelements--;
    s->finger = entry - s->table + 1;   /* next place to start */
    return 0;
}

typedef struct set_iterator_struct {
  set_t src;
  int nxt;
} *set_iterator_state_t; 

int set_iterator_next_entry(iterator_t iter, WORD *res) {
  set_iterator_state_t state = iter->state;
  setentry *table = state->src->table;
  int n = state->src->mask;
  int i = state->nxt;
  while (i <= n) {
    setentry *entry = &table[i];
    if (entry->key != NULL && entry->key != dummy) {
      *res = entry;
      state->nxt = i+1;
      return 0;
    }
    i++;
  }
  *res = NULL;
  errno = EINVAL;
  return -1;
}

int set_iterator_next(iterator_t iter, WORD *res) {
  WORD res2;
  if(!set_iterator_next_entry(iter,&res2)) {
    *res = ((setentry*)res2)->key;
    return 0;
  } 
  *res = NULL;
  errno = EINVAL;
  return -1;
}

iterator_t set_iter(set_t s) { 
  iterator_t iter = malloc(sizeof(struct iterator_struct));
  set_iterator_state_t state = malloc(sizeof(struct set_iterator_struct));
  state->src = s;
  state->nxt = 0;
  iter->state = state;
  iter->next = set_iterator_next;
  return iter;
}

// this iterator returns entries, not elements.
static iterator_t set_iter_entry(set_t s) { 
  iterator_t iter = malloc(sizeof(struct iterator_struct));
  set_iterator_state_t state = malloc(sizeof(struct set_iterator_struct));
  state->src = s;
  state->nxt = 0;
  iter->state = state;
  iter->next = set_iterator_next_entry;
  return iter;
}


static set_t set_copy(set_t s) {
  set_t res = set_new(s->h);
  iterator_t iter = set_iter_entry(s);
  WORD w;
  while(!iterator_next(iter,&w)){
    WORD key = ((setentry*)w)->key;
    size_t hash = ((setentry*)w)->hash;
    set_add_entry(res,key,hash);
  }
  return res;
}

set_t set_intersection(set_t s, set_t other) {
  if (set_len(other) > set_len(s))
    return set_intersection(other,s);
  set_t res = set_new(s->h);
  iterator_t iter = set_iter_entry(other);
  WORD w;
  while(!iterator_next(iter,&w)){
    WORD key = ((setentry*)w)->key;
    size_t hash = ((setentry*)w)->hash;
    if (set_contains_entry(s,key,hash))
      set_add_entry(res,key,hash);
  }
  return res;
}

set_t set_union(set_t s, set_t other) {
  set_t res = set_copy(s);
  iterator_t iter = set_iter_entry(other);
  WORD w;
  while(!iterator_next(iter,&w)){
    WORD key = ((setentry*)w)->key;
    size_t hash = ((setentry*)w)->hash;
    set_add_entry(res,key,hash);
  }
  return res;
}

set_t set_symmetric_difference(set_t s, set_t other) {
  set_t res = set_copy(s);
  iterator_t iter = set_iter_entry(other);
  WORD w;
  while(!iterator_next(iter,&w)){
    WORD key = ((setentry*)w)->key;
    size_t hash = ((setentry*)w)->hash;
    if(!set_discard_entry(res,key,hash))
      set_add_entry(res,key,hash);
  }
  return res;
}

int set_isdisjoint(set_t s, set_t other) {
    if (s == other) 
      return s->numelements == 0;

    if (other->numelements > s->numelements)
      return set_isdisjoint(other,s);

    iterator_t iter = set_iter_entry(other);
    WORD w;
    while (!iterator_next(iter, &w)) {
      if(set_contains_entry(s, ((setentry*)w)->key, ((setentry*)w)->hash))
        return 0;
    }
    return 1;
}


int set_eq(set_t s, set_t other) {
  if (s == other) 
    return 1;
    
  if (s->numelements != other->numelements)
    return 0;
  
  iterator_t iter = set_iter_entry(other);
  WORD w;
  while (!iterator_next(iter,&w))
    if(!set_contains_entry(s, ((setentry*)w)->key, ((setentry*)w)->hash))
      return 0;
  return 1;
}

int set_ge(set_t s, set_t other) {
  if (s == other) 
    return 1;
    
  if (s->numelements < other->numelements)
    return 0;
  
  iterator_t iter = set_iter_entry(other);
  WORD w;
  while (!iterator_next(iter,&w))
    if(!set_contains_entry(s, ((setentry*)w)->key, ((setentry*)w)->hash))
      return 0;
  return 1;
}

int set_gt(set_t s, set_t other) {
  if (s == other) 
    return 0;
    
  if (s->numelements <= other->numelements)
    return 0;
  
  iterator_t iter = set_iter_entry(other);
  WORD w;
  while (!iterator_next(iter,&w))
    if(!set_contains_entry(s, ((setentry*)w)->key, ((setentry*)w)->hash))
      return 0;
  return 1;
}

int set_ne(set_t s, set_t other) {
  return !set_eq(s,other);
}

int set_le(set_t s, set_t other) {
  return set_ge(other,s);
}

int set_lt(set_t s, set_t other) {
  return set_gt(other,s);
}
