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

static B_Iterator B_set_iter_entry(B_set set);

// General methods ///////////////////////////////////////////////////////////////////////////////////

B_set B_setG_new(B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return $NEW(B_set, hashwit, wit, iterable);
}

void B_set_init(B_set set, B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    set->numelements = 0;
    set->fill = 0;
    set->mask = MIN_SIZE-1;
    set->finger = 0;
    set->table = malloc(MIN_SIZE*sizeof(B_setentry));
    memset(set->table,0,MIN_SIZE*sizeof(B_setentry));
    if (wit && iterable) {
        B_Iterator it = wit->$class->__iter__(wit,iterable);
        $WORD nxt;
        while((nxt = it->$class->__next__(it))) {
            B_set_add(set,hashwit,nxt);
        }
    }
}

B_bool B_set_bool(B_set self) {
    return toB_bool(self->numelements>0);
}

B_str B_set_str(B_set self) {
    B_list s2 = B_listD_new(self->numelements);
    B_IteratorD_set iter = $NEW(B_IteratorD_set,self);
    B_value elem;
    for (int i=0; i<self->numelements; i++) {
        elem = (B_value)iter->$class->__next__(iter);
        B_listD_append(s2,elem->$class->__repr__(elem));
    }
    return B_strD_join_par('{',s2,'}');
}

void B_set_serialize(B_set self, $Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        long pk = fromB_int(prevkey);
        $val_serialize(-SET_ID,&pk,state);
        return;
    }
    B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,toB_int(state->row_no));
    $ROW row = $add_header(SET_ID,4,state);
    row->blob[0] = ($WORD)self->numelements;
    row->blob[1] = ($WORD)self->fill;
    row->blob[2] = ($WORD)self->mask;
    row->blob[3] = ($WORD)self->finger;
    for (long i=0; i<=self->mask; i++) {
        B_setentry *entry = &self->table[i];
        $step_serialize(toB_int(entry->hash),state);
        $step_serialize(entry->key,state);
    }
}
 
B_set B_set_deserialize (B_set res, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,toB_int((long)this->blob[0]),NULL);
    } else {
        if (!res)
            res = malloc(sizeof(struct B_set));
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,toB_int(state->row_no-1),res);
        res->$class = &B_setG_methods;
        res->numelements = (long)this->blob[0];
        res->fill = (long)this->blob[1];
        res->mask = (long)this->blob[2];
        res->finger = (long)this->blob[3];
        res->table = malloc((res->mask+1)*sizeof(B_setentry));
        memset(res->table,0,(res->mask+1)*sizeof(B_setentry));
        for (int i=0; i<=res->mask;i++) {
            B_setentry *entry = &res->table[i];
            entry->hash = fromB_int((B_int)$step_deserialize(state));
            entry->key = $step_deserialize(state);
            if (entry->hash==-1)
                entry->key = dummy;
        }
        return res;
    }
}

// Maybe we should  offer union, intersection and symmetric difference under those names.
struct B_setG_class B_setG_methods = {"B_set",UNASSIGNED,($SuperG_class)&B_objectG_methods,B_set_init,B_set_serialize,B_set_deserialize,B_set_bool,B_set_str,B_set_str,B_set_copy}; 


static void B_set_insert_clean(B_setentry *table, long mask, $WORD *key, long hash) {
    B_setentry *entry;
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

static int B_set_table_resize(B_set so, int minsize) {
    B_setentry *oldtable, *newtable, *entry;
    long oldmask = so->mask;
    long newmask;

    /* Find the smallest table size > minsize. */
    long newsize = MIN_SIZE;
    while (newsize <= (long)minsize) {
        newsize <<= 1; // The largest possible value is PY_SSIZE_T_MAX + 1.
    }
    /* Get space for a new table. */
    oldtable = so->table;

    newtable = malloc(sizeof(B_setentry) * newsize);
    if (newtable == NULL) {
        return -1;
    }

    /* Make the B_set empty, using the new table. */
    memset(newtable, 0, sizeof(B_setentry) * newsize);
    so->mask = newsize - 1;
    so->table = newtable;

    /* Copy the data over; 
       dummy entries aren't copied over, of course */
    newmask = (long)so->mask;
    if (so->fill == so->numelements) {
        for (entry = oldtable; entry <= oldtable + oldmask; entry++) {
            if (entry->key != NULL) {
                B_set_insert_clean(newtable, newmask, entry->key, entry->hash);
            }
        }
    } else {
        so->fill = so->numelements;
        for (entry = oldtable; entry <= oldtable + oldmask; entry++) {
            if (entry->key != NULL && entry->key != dummy) {
                B_set_insert_clean(newtable, newmask, entry->key, entry->hash);
            }
        }
    }

    free(oldtable);
    return 0;
}

static B_setentry *B_set_lookkey(B_set set, B_Hashable hashwit, $WORD key, long hash) {
    B_setentry *entry;
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

static int B_set_contains_entry(B_set set,  B_Hashable hashwit, $WORD elem, long hash) {
    return B_set_lookkey(set, hashwit, elem, hash)->key != NULL;
}

static void B_set_add_entry(B_set set, B_Hashable hashwit, $WORD key, long hash) {
    B_setentry *freeslot;
    B_setentry *entry;
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
    B_set_table_resize(set, set->numelements>50000 ? set->numelements*2 : set->numelements*4);
    return;
 found_active:
    return;
}


B_set B_set_copy(B_set set, B_Hashable hashwit) {
    B_set res = malloc(sizeof(struct B_set));
    memcpy(res,set,sizeof(struct B_set));
    res->table = malloc((set->mask+1)*sizeof(B_setentry));
    memcpy(res->table,set->table,(set->mask+1)*sizeof(B_setentry));
    return res;
}

static int B_set_discard_entry(B_set set, B_Hashable hashwit, $WORD elem, long hash) {
    B_setentry *entry = B_set_lookkey(set,hashwit,elem, hash);
    if (entry->key != NULL) {
        entry->key = dummy;
        entry->hash = -1;
        set->numelements--;
        return DISCARD_FOUND;
    } else
        return DISCARD_NOTFOUND;
}

// Eq //////////////////////////////////////////////////////////////////////////////////////////////


int B_set_eq(B_Hashable hashwit, B_set set, B_set other) {
    if (set == other) 
        return 1;
    if (set->numelements != other->numelements)
        return 0;
    B_Iterator iter = B_set_iter_entry(other);
    $WORD w;
    while((w = $next(iter))){
        if(!B_set_contains_entry(set, hashwit, ((B_setentry*)w)->key, ((B_setentry*)w)->hash))
            return 0;
    }
    return 1;
}

// Ord /////////////////////////////////////////////////////////////////////////////////////////////

int B_set_ge(B_Hashable hashwit, B_set set, B_set other) {
    if (set == other) 
        return 1;    
    if (set->numelements < other->numelements)
        return 0;
    B_Iterator iter = B_set_iter_entry(other);
    $WORD w;
    while((w = $next(iter))){
        if(!B_set_contains_entry(set, hashwit, ((B_setentry*)w)->key, ((B_setentry*)w)->hash))
            return 0;
    }
    return 1;
}

int B_set_gt(B_Hashable hashwit, B_set set, B_set other) {
    if (set == other) 
        return 0;    
    if (set->numelements <= other->numelements)
        return 0;
    B_Iterator iter = B_set_iter_entry(other);
    $WORD w;
    while((w = $next(iter))){
        if(!B_set_contains_entry(set, hashwit, ((B_setentry*)w)->key, ((B_setentry*)w)->hash))
            return 0;
    }
    return 1;
}

int B_set_le(B_Hashable hashwit, B_set set, B_set other) {
    return B_set_ge(hashwit,other,set);
}

int B_set_lt(B_Hashable hashwit, B_set set, B_set other) {
    return B_set_gt(hashwit,other,set);
}

// Logical /////////////////////////////////////////////////////////////////////////////////////////////


B_set B_set_intersection(B_Hashable hashwit, B_set set, B_set other) {
    if (B_set_len(other) > B_set_len(set))
        return B_set_intersection(hashwit,other,set);
    B_set res = $NEW(B_set,hashwit,NULL,NULL);
    B_Iterator iter = B_set_iter_entry(set);
    $WORD w;
    while((w = $next(iter))){
        $WORD key = ((B_setentry*)w)->key;
        long hash = ((B_setentry*)w)->hash;
        if (B_set_contains_entry(other,hashwit,key,hash))
            B_set_add_entry(res,hashwit,key,hash);
    }
    return res;
}


B_set B_set_union(B_Hashable hashwit, B_set set, B_set other) {
    if (B_set_len(other) > B_set_len(set))
        return B_set_union(hashwit,other,set);
    B_set res = B_set_copy(set, hashwit);
    B_Iterator iter = B_set_iter_entry(other);
    $WORD w;
    while((w = $next(iter))){
        $WORD key = ((B_setentry*)w)->key;
        long hash = ((B_setentry*)w)->hash;
        B_set_add_entry(res,hashwit,key,hash);
    }
    return res;
}

B_set B_set_symmetric_difference(B_Hashable hashwit, B_set set, B_set other) {
    B_set res = B_set_copy(set, hashwit);
    B_Iterator iter = B_set_iter_entry(other);
    $WORD w;
    while((w = $next(iter))){
        $WORD key = ((B_setentry*)w)->key;
        long hash = ((B_setentry*)w)->hash;
        if(!B_set_discard_entry(res,hashwit,key,hash))
            B_set_add_entry(res,hashwit,key,hash);
    }
    return res;
}

// Set /////////////////////////////////////////////////////////////////////////////////////////////

void B_set_add(B_set set, B_Hashable hashwit,  $WORD elem) {
    B_set_add_entry(set,hashwit,elem,fromB_int(hashwit->$class->__hash__(hashwit,elem)));
}


void B_set_discard(B_set set, B_Hashable hashwit, $WORD elem) {
    B_set_discard_entry(set,hashwit,elem,fromB_int(hashwit->$class->__hash__(hashwit,elem)));
}

void B_set_remove(B_set set, B_Hashable hashwit, $WORD elem) {
    long hash = fromB_int(hashwit->$class->__hash__(hashwit,elem));
    if(B_set_discard_entry(set,hashwit,elem,hash))
        return;
    else {
        $RAISE((B_BaseException)$NEW(B_KeyError,to$str("remove: element not set member")));
    }
}

$WORD B_set_pop(B_set set) {
    $WORD res;
    // Make sure the search finger is in bounds 
    B_setentry *entry = set->table + (set->finger & set->mask);
    B_setentry *limit = set->table + set->mask;

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

int B_set_isdisjoint(B_Hashable hashwit, B_set set, B_set other) {
    if (set == other) 
        return set->numelements == 0;
    if (other->numelements > set->numelements)
        return B_set_isdisjoint(hashwit,other,set);
    B_Iterator iter = B_set_iter_entry(other);
    $WORD w;
    while((w = $next(iter))){
        if(B_set_contains_entry(set, hashwit,((B_setentry*)w)->key, ((B_setentry*)w)->hash))
            return 0;
    }
    return 1;
}

// Collection /////////////////////////////////////////////////////////////////////////////////////////////

B_set B_set_fromiter(B_Hashable hashwit,B_Iterator it) {
    B_set res = $NEW(B_set,hashwit,NULL,NULL);
    res->numelements = 0;
    res->fill = 0;
    res->mask = MIN_SIZE-1;
    res->finger = 0;
    res->table = malloc(MIN_SIZE*sizeof(B_setentry));
    memset(res->table,0,MIN_SIZE*sizeof(B_setentry));
    $WORD nxt;
    while((nxt = it->$class->__next__(it))) {
        B_set_add(res,hashwit,nxt);
    }
    return res;
}

long B_set_len(B_set set) {
    return set->numelements;
}

// Container_Eq ///////////////////////////////////////////////////////////////////////////////////////

int B_set_contains(B_set set, B_Hashable hashwit, $WORD elem) {
    return B_set_contains_entry(set,hashwit,elem,fromB_int(hashwit->$class->__hash__(hashwit,elem)));
}
 
// Iterable ///////////////////////////////////////////////////////////////////////////////////////

static $WORD B_IteratorD_set_next_entry(B_IteratorD_set self) {
    B_setentry *table = self->src->table;
    long n = self->src->mask;
    long i = self->nxt;
    while (i <= n) {
        B_setentry *entry = &table[i];
        if (entry->key != NULL && entry->key != dummy) {
            self->nxt = i+1;
            return entry;
        }
        i++;
    }
    return NULL;
}

static B_Iterator B_set_iter_entry(B_set set) {
    B_IteratorD_set iter =  malloc(sizeof(struct B_IteratorD_set));
    struct B_IteratorD_setG_class *methods = malloc(sizeof(struct B_IteratorD_setG_class));
    iter->$class = methods;
    methods->__next__ =  B_IteratorD_set_next_entry;
    iter->src = set;
    iter->nxt = 0;
    return (B_Iterator)iter;
}
                                            
static $WORD B_IteratorD_set_next(B_IteratorD_set self) {
    $WORD res;
    if((res = B_IteratorD_set_next_entry(self))) {
        return ((B_setentry*)res)->key;
    } 
    return NULL;
}

B_IteratorD_set B_IteratorD_setG_new(B_set s) {
    return $NEW(B_IteratorD_set, s);
}

void B_IteratorD_set_init(B_IteratorD_set self, B_set set) {
    self->src = set;
    self->nxt = 0;
}

B_bool B_IteratorD_set_bool(B_IteratorD_set self) {
    return $True;
}

B_str B_IteratorD_set_str(B_IteratorD_set self) {
    char *s;
    asprintf(&s,"<set keys iterator object at %p>",self);
    return to$str(s);
}

void B_IteratorD_set_serialize(B_IteratorD_set self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(toB_int(self->nxt),state);
}

B_IteratorD_set B_IteratorD_setD__deserialize(B_IteratorD_set res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_set,state);
    res->src = (B_set)$step_deserialize(state);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}

struct B_IteratorD_setG_class B_IteratorD_setG_methods = {"B_IteratorD_set",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_set_init,
                                                    B_IteratorD_set_serialize, B_IteratorD_setD__deserialize,B_IteratorD_set_bool,B_IteratorD_set_str,B_IteratorD_set_str, B_IteratorD_set_next};


B_Iterator B_set_iter(B_set set) {
    return (B_Iterator)$NEW(B_IteratorD_set,set);
}
 
// Minus ///////////////////////////////////////////////////////////////////////////////////////////

B_set B_set_difference(B_Hashable hashwit, B_set set, B_set other) {
    B_set res = B_set_copy(set,hashwit);
    B_Iterator iter = B_set_iter_entry(other);
    $WORD w;
    while((w = $next(iter))){
        $WORD key = ((B_setentry*)w)->key;
        long hash = ((B_setentry*)w)->hash;
        B_set_discard_entry(res,hashwit,key,hash);
    }
    return res;
}
 
