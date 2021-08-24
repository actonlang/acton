/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <string.h> /*memset*/

#define DISCARD_NOTFOUND 0
#define DISCARD_FOUND 1
#include "builtin.h"

/* 
This implementation of sets is an adaptation of CPython's set implementation.

*/

#define PERTURB_SHIFT 5
#define MIN_SIZE 8

static $WORD _dummy;
#define dummy (&_dummy)

static $Iterator $set_iter_entry($set set);

// General methods ///////////////////////////////////////////////////////////////////////////////////

$set $set$new($Hashable hashwit, $Iterable wit, $WORD iterable) {
  return $NEW($set, hashwit, wit, iterable);
}

void $set_init($set set, $Hashable hashwit, $Iterable wit, $WORD iterable) {
  set->numelements = 0;
  set->fill = 0;
  set->mask = MIN_SIZE-1;
  set->finger = 0;
  set->table = malloc(MIN_SIZE*sizeof($setentry));
  memset(set->table,0,MIN_SIZE*sizeof($setentry));
  if (wit && iterable) {
    $Iterator it = wit->$class->__iter__(wit,iterable);
    $WORD nxt;
    while((nxt = it->$class->__next__(it))) {
      $set_add(set,hashwit,nxt);
    }
  }
}

$bool $set_bool($set self) {
  return to$bool(self->numelements>0);
}

$str $set_str($set self) {
  $list s2 = $list_new(self->numelements);
  $Iterator$set iter = $NEW($Iterator$set,self);
  $value elem;
  for (int i=0; i<self->numelements; i++) {
    elem = ($value)iter->$class->__next__(iter);
    $list_append(s2,elem->$class->__str__(elem));
  }
  return $str_join_par('{',s2,'}');
}

void $set_serialize($set self, $Serial$state state) {
  $int prevkey = ($int)$dict_get(state->done,($Hashable)$Hashable$WORD$witness,self,NULL);
  if (prevkey) {
      $val_serialize(-SET_ID,&prevkey->val,state);
    return;
  }
  $dict_setitem(state->done,($Hashable)$Hashable$WORD$witness,self,to$int(state->row_no));
  $ROW row = $add_header(SET_ID,4,state);
  row->blob[0] = ($WORD)self->numelements;
  row->blob[1] = ($WORD)self->fill;
  row->blob[2] = ($WORD)self->mask;
  row->blob[3] = ($WORD)self->finger;
  for (long i=0; i<=self->mask; i++) {
    $setentry *entry = &self->table[i];
    $step_serialize(to$int(entry->hash),state);
    $step_serialize(entry->key,state);
  }
}
 
$set $set_deserialize ($set res, $Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row_no++;
  if (this->class_id < 0) {
    return $dict_get(state->done,($Hashable)$Hashable$int$witness,to$int((long)this->blob[0]),NULL);
  } else {
    if (!res)
       res = malloc(sizeof(struct $set));
    $dict_setitem(state->done,($Hashable)$Hashable$int$witness,to$int(state->row_no-1),res);
    res->$class = &$set$methods;
    res->numelements = (long)this->blob[0];
    res->fill = (long)this->blob[1];
    res->mask = (long)this->blob[2];
    res->finger = (long)this->blob[3];
    res->table = malloc((res->mask+1)*sizeof($setentry));
    memset(res->table,0,(res->mask+1)*sizeof($setentry));
    for (int i=0; i<=res->mask;i++) {
      $setentry *entry = &res->table[i];
      entry->hash = from$int(($int)$step_deserialize(state));
      entry->key = $step_deserialize(state);
        if (entry->hash==-1)
          entry->key = dummy;
    }
    return res;
  }
}

// Maybe we should  offer union, intersection and symmetric difference under those names.
struct $set$class $set$methods = {"$set",UNASSIGNED,($Super$class)&$object$methods,$set_init,$set_serialize,$set_deserialize,$set_bool,$set_str,$set_copy}; 


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


$set $set_copy($set set, $Hashable hashwit) {
  $set res = malloc(sizeof(struct $set));
  memcpy(res,set,sizeof(struct $set));
  res->table = malloc((set->mask+1)*sizeof($setentry));
  memcpy(res->table,set->table,(set->mask+1)*sizeof($setentry));
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
  $set res = $NEW($set,hashwit,NULL,NULL);
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
  $set res = $set_copy(set, hashwit);
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
  $set res = $set_copy(set, hashwit);
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
    $RAISE(($BaseException)$NEW($KeyError,to$str("remove: element not set member")));
  }
}

$WORD $set_pop($set set) {
  $WORD res;
  // Make sure the search finger is in bounds 
  $setentry *entry = set->table + (set->finger & set->mask);
  $setentry *limit = set->table + set->mask;

  if (set->numelements == 0) {
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

$set $set_fromiter($Hashable hashwit,$Iterator it) {
  $set res = $NEW($set,hashwit,NULL,NULL);
  res->numelements = 0;
  res->fill = 0;
  res->mask = MIN_SIZE-1;
  res->finger = 0;
  res->table = malloc(MIN_SIZE*sizeof($setentry));
  memset(res->table,0,MIN_SIZE*sizeof($setentry));
  $WORD nxt;
  while((nxt = it->$class->__next__(it))) {
    $set_add(res,hashwit,nxt);
  }
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

static $WORD $Iterator$set_next_entry($Iterator$set self) {
  $setentry *table = self->src->table;
  long n = self->src->mask;
  long i = self->nxt;
  while (i <= n) {
    $setentry *entry = &table[i];
    if (entry->key != NULL && entry->key != dummy) {
      self->nxt = i+1;
      return entry;
    }
    i++;
  }
  return NULL;
}

static $Iterator $set_iter_entry($set set) {
  $Iterator$set iter =  malloc(sizeof(struct $Iterator$set));
  struct $Iterator$set$class *methods = malloc(sizeof(struct $Iterator$set$class));
  iter->$class = methods;
  methods->__next__ =  $Iterator$set_next_entry;
  iter->src = set;
  iter->nxt = 0;
  return ($Iterator)iter;
}
                                            
static $WORD $Iterator$set_next($Iterator$set self) {
  $WORD res;
  if((res = $Iterator$set_next_entry(self))) {
    return (($setentry*)res)->key;
  } 
  return NULL;
}

$Iterator$set $Iterator$set$new($set s) {
  return $NEW($Iterator$set, s);
}

void $Iterator$set_init($Iterator$set self, $set set) {
  self->src = set;
  self->nxt = 0;
}

$bool $Iterator$set_bool($Iterator$set self) {
  return $True;
}

$str $Iterator$set_str($Iterator$set self) {
  char *s;
  asprintf(&s,"<set keys iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$set_serialize($Iterator$set self, $Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}

$Iterator$set $Iterator$set$_deserialize($Iterator$set res, $Serial$state state) {
   if (!res)
      res = $DNEW($Iterator$set,state);
   res->src = ($set)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}

struct $Iterator$set$class $Iterator$set$methods = {"$Iterator$set",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$set_init,
                                                      $Iterator$set_serialize, $Iterator$set$_deserialize,$Iterator$set_bool,$Iterator$set_str, $Iterator$set_next};


$Iterator $set_iter($set set) {
  return ($Iterator)$NEW($Iterator$set,set);
}
 
// Minus ///////////////////////////////////////////////////////////////////////////////////////////

$set $set_difference($Hashable hashwit, $set set, $set other) {
  $set res = $set_copy(set,hashwit);
  $Iterator iter = $set_iter_entry(other);
  $WORD w;
  while((w = $next(iter))){
    $WORD key = (($setentry*)w)->key;
    long hash = (($setentry*)w)->hash;
    $set_discard_entry(res,hashwit,key,hash);
  }
  return res;
}
 
