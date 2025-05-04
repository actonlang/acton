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

 
#define DISCARD_NOTFOUND 0
#define DISCARD_FOUND 1
#define PERTURB_SHIFT 5
#define MIN_SIZE 8

static $WORD _dummy;
#define dummy (&_dummy)


// Auxiliary functions ///////////////////////////////////////////////////////////////////////////////

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

    newtable = acton_malloc(sizeof(B_setentry) * newsize);
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

    acton_free(oldtable);
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

void B_set_add_entry(B_set set, B_Hashable hashwit, $WORD key, long hash) {
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
    B_set res = acton_malloc(sizeof(struct B_set));
    memcpy(res,set,sizeof(struct B_set));
    res->table = acton_malloc((set->mask+1)*sizeof(B_setentry));
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

// General methods ///////////////////////////////////////////////////////////////////////////////////

B_set B_setG_new(B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return $NEW(B_set, hashwit, wit, iterable);
}

B_NoneType B_setD___init__(B_set set, B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    set->numelements = 0;
    set->fill = 0;
    set->mask = MIN_SIZE-1;
    set->finger = 0;
    set->table = acton_malloc(MIN_SIZE*sizeof(B_setentry));
    memset(set->table,0,MIN_SIZE*sizeof(B_setentry));
    if (wit && iterable) {
        B_Iterator it = wit->$class->__iter__(wit,iterable);
        while(1) {
            if ($PUSH()) {
                $WORD nxt = it->$class->__next__(it);
                B_set_add_entry(set,hashwit,nxt,fromB_u64(hashwit->$class->__hash__(hashwit,nxt)));
                $DROP();
            } else {
                B_BaseException ex = $POP();
                if ($ISINSTANCE0(ex, B_StopIteration))
                    break;
                else
                    $RAISE(ex);
            }
        }
    }
    return B_None;
}

B_bool B_setD___bool__(B_set self) {
    return toB_bool(self->numelements>0);
}

B_str B_setD___str__(B_set self) {
    B_list s2 = B_listD_new(self->numelements);
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_IteratorD_set iter = $NEW(B_IteratorD_set,self);
    B_value elem;
    for (int i=0; i<self->numelements; i++) {
        elem = (B_value)iter->$class->__next__(iter);
        wit->$class->append(wit,s2,elem->$class->__repr__(elem));
    }
    return B_strD_join_par('{',s2,'}');
}

B_str B_setD___repr__(B_set self) {
    return B_setD___str__(self);
}

void B_setD___serialize__(B_set self, $Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        long pk = from$int(prevkey);
        $val_serialize(-SET_ID,&pk,state);
        return;
    }
    B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,to$int(state->row_no));
    $ROW row = $add_header(SET_ID,4,state);
    row->blob[0] = ($WORD)self->numelements;
    row->blob[1] = ($WORD)self->fill;
    row->blob[2] = ($WORD)self->mask;
    row->blob[3] = ($WORD)self->finger;
    for (long i=0; i<=self->mask; i++) {
        B_setentry *entry = &self->table[i];
        $step_serialize(to$int(entry->hash),state);
        $step_serialize(entry->key,state);
    }
}
 
B_set B_setD___deserialize__ (B_set res, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,to$int((long)this->blob[0]),NULL);
    } else {
        if (!res)
            res = acton_malloc(sizeof(struct B_set));
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,to$int(state->row_no-1),res);
        res->$class = &B_setG_methods;
        res->numelements = (long)this->blob[0];
        res->fill = (long)this->blob[1];
        res->mask = (long)this->blob[2];
        res->finger = (long)this->blob[3];
        res->table = acton_malloc((res->mask+1)*sizeof(B_setentry));
        memset(res->table,0,(res->mask+1)*sizeof(B_setentry));
        for (int i=0; i<=res->mask;i++) {
            B_setentry *entry = &res->table[i];
            entry->hash = from$int((B_int)$step_deserialize(state));
            entry->key = $step_deserialize(state);
            if (entry->hash==-1)
                entry->key = dummy;
        }
        return res;
    }
}


// B_Set

// Iterable ///////////////////////////////////////////////////////////////////////////////////////  Leif 

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
    $RAISE ((B_BaseException)$NEW(B_StopIteration, to$str("set iterator terminated")));
    return NULL; //to avoid compiler warning
}

static B_Iterator B_set_iter_entry(B_set set) {
    B_IteratorD_set iter =  acton_malloc(sizeof(struct B_IteratorD_set));
    struct B_IteratorD_setG_class *methods = acton_malloc(sizeof(struct B_IteratorD_setG_class));
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
    return B_True;
}

B_str B_IteratorD_set_str(B_IteratorD_set self) {
    return $FORMAT("<set keys iterator object at %p>", self);
}

void B_IteratorD_set_serialize(B_IteratorD_set self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}

B_IteratorD_set B_IteratorD_setD__deserialize(B_IteratorD_set res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_set,state);
    res->src = (B_set)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}

struct B_IteratorD_setG_class B_IteratorD_setG_methods = {"B_IteratorD_set",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_set_init,
                                                    B_IteratorD_set_serialize, B_IteratorD_setD__deserialize,B_IteratorD_set_bool,B_IteratorD_set_str,B_IteratorD_set_str, B_IteratorD_set_next};


B_Iterator B_SetD_setD___iter__ (B_SetD_set wit, B_set set) {
    return (B_Iterator)$NEW(B_IteratorD_set,set);
}

B_NoneType B_SetD_setD_add (B_SetD_set wit, B_set set, $WORD elem) {
    B_Hashable hashwit = wit->W_HashableD_AD_SetD_set;
    B_set_add_entry(set,hashwit,elem,fromB_u64(hashwit->$class->__hash__(hashwit,elem)));
    return B_None;
}

B_set B_SetD_setD___fromiter__(B_SetD_set wit, B_Iterable wit2, $WORD iter) {
    return B_setG_new(wit->W_HashableD_AD_SetD_set, wit2, iter);
    /*
    B_Iterator it = wit2->$class->__iter__(wit2,iter);
    B_set res = $NEW(B_set,hashwit,NULL,NULL);
    res->numelements = 0;
    res->fill = 0;
    res->mask = MIN_SIZE-1;
    res->finger = 0;
    res->table = acton_malloc(MIN_SIZE*sizeof(B_setentry));
    memset(res->table,0,MIN_SIZE*sizeof(B_setentry));
    $WORD nxt;
    while((nxt = it->$class->__next__(it))) {
        B_set_add_entry(res,hashwit,nxt,fromB_u64(hashwit->$class->__hash__(hashwit,nxt)));
    }
    return res;
    */
}

B_int B_SetD_setD___len__ (B_SetD_set wit, B_set set) {
    return to$int(set->numelements);
}

B_bool B_SetD_setD___contains__ (B_SetD_set wit, B_set set, $WORD val) {
    B_Hashable hashwit = wit->W_HashableD_AD_SetD_set;
    return toB_bool(B_set_contains_entry(set,hashwit,val,fromB_u64(hashwit->$class->__hash__(hashwit,val))));
}

B_bool B_SetD_setD___containsnot__ (B_SetD_set wit, B_set set, $WORD v) {
    return  toB_bool(!B_SetD_setD___contains__(wit,set,v)->val);
}

B_bool B_SetD_setD_isdisjoint (B_SetD_set wit, B_set set, B_set other) {
    B_Hashable hashwit = wit->W_HashableD_AD_SetD_set;
    if (set == other) 
        return toB_bool(set->numelements == 0);
    if (other->numelements > set->numelements)
        return B_SetD_setD_isdisjoint(wit,other,set);
    B_Iterator iter = B_set_iter_entry(other);
    $WORD w;
    long res = 1;
    while((w = $next(iter))){
        if(B_set_contains_entry(set, hashwit,((B_setentry*)w)->key, ((B_setentry*)w)->hash)) {
            res = 0;
            break;
        }
    }
    return toB_bool(res);
}

// TODO: ideally this could be defined in .act file instead of C since we just
// need to call the .add() method, but that doesn't currently seem to work
B_NoneType B_SetD_setD_update (B_SetD_set wit, B_set set, B_Iterable otherwit, $WORD other) {
    B_Hashable hashwit = wit->W_HashableD_AD_SetD_set;
    if (set == other)
        return B_None;
    B_Iterator it = otherwit->$class->__iter__(otherwit, other);
    while(1) {
        if ($PUSH()) {
            $WORD e = it->$class->__next__(it);
            B_set_add_entry(set, hashwit, e, fromB_u64(hashwit->$class->__hash__(hashwit,e)));
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if ($ISINSTANCE0(ex, B_StopIteration))
                break;
           else
               $RAISE(ex);
        }
    }
    return B_None;
}

B_NoneType B_SetD_setD_discard (B_SetD_set wit, B_set set, $WORD elem) {
    B_Hashable hashwit = wit->W_HashableD_AD_SetD_set;
    B_set_discard_entry(set,hashwit,elem,fromB_u64(hashwit->$class->__hash__(hashwit,elem)));
    return B_None;
}

$WORD B_SetD_setD_pop (B_SetD_set wit, B_set set) {
    if (set->numelements == 0)
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("pop from an empty set")));

    $WORD res;
    // Make sure the search finger is in bounds 
    B_setentry *entry = set->table + (set->finger & set->mask);
    B_setentry *limit = set->table + set->mask;

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


// B_Ord

B_bool B_OrdD_SetD_setD___eq__ (B_OrdD_SetD_set wit, B_set set, B_set other) {
    B_Hashable hashwit = ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set;
    if (set == other) 
        return B_True;
    if (set->numelements != other->numelements)
        return B_False;
    B_Iterator iter = B_set_iter_entry(other);
    long n = 0;
    while(n < set->numelements) {
        $WORD w = $next(iter);
        if(!B_set_contains_entry(set, hashwit, ((B_setentry*)w)->key, ((B_setentry*)w)->hash))
            return B_False;
        n++;
    }
    return B_True;
}
  
B_bool B_OrdD_SetD_setD___ne__ (B_OrdD_SetD_set wit, B_set set, B_set other) {
    return toB_bool(!B_OrdD_SetD_setD___eq__ (wit,set,other)->val);
}
  
B_bool B_OrdD_SetD_setD___gt__ (B_OrdD_SetD_set wit, B_set set, B_set other) {
    B_Hashable hashwit = ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set;
    if (set == other) 
        return B_False;    
    if (set->numelements <= other->numelements)
        return B_False;
    B_Iterator iter = B_set_iter_entry(other);
    long n = 0;
    while(n < other->numelements) {
        $WORD w = $next(iter);
        if(!B_set_contains_entry(set, hashwit, ((B_setentry*)w)->key, ((B_setentry*)w)->hash))
            return B_False;
        n++;
    }
    return B_True;
}
  
B_bool B_OrdD_SetD_setD___ge__ (B_OrdD_SetD_set wit, B_set set, B_set other) {
    B_Hashable hashwit = ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set;
    if (set == other) 
        return B_False;    
    if (set->numelements < other->numelements)
        return B_False;
    B_Iterator iter = B_set_iter_entry(other);
    long n = 0;
    while(n < other->numelements) {
        $WORD w = $next(iter);
        if(!B_set_contains_entry(set, hashwit, ((B_setentry*)w)->key, ((B_setentry*)w)->hash))
            return B_False;
        n++;
    }
    return B_True;
}

B_bool B_OrdD_SetD_setD___lt__ (B_OrdD_SetD_set wit, B_set set, B_set other) {
    return B_OrdD_SetD_setD___gt__(wit, other, set);
}
  
B_bool B_OrdD_SetD_setD___le__ (B_OrdD_SetD_set wit, B_set set, B_set other) {
    return  B_OrdD_SetD_setD___ge__(wit, other, set);
}
  
// B_Minus

 
B_set B_MinusD_SetD_setD___sub__ (B_MinusD_SetD_set wit, B_set set, B_set other) {
    B_Hashable hashwit = ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set;
    B_set res = B_set_copy(set,hashwit);
    B_Iterator iter = B_set_iter_entry(other);
    long n = 0;
    while(n < other->numelements) {
        $WORD w = $next(iter);
        $WORD key = ((B_setentry*)w)->key;
        long hash = ((B_setentry*)w)->hash;
        B_set_discard_entry(res,hashwit,key,hash);
        n++;
    }
    return res;
}

// B_Logical

 
B_set B_LogicalD_SetD_setD___and__(B_LogicalD_SetD_set wit, B_set set, B_set other) {
    B_Hashable hashwit = ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set;
    if (other->numelements > set->numelements)
        return  B_LogicalD_SetD_setD___and__(wit,other,set);
    B_set res = $NEW(B_set,hashwit,NULL,NULL);
    B_Iterator iter = B_set_iter_entry(set);
    long n = 0;
    while(n < set->numelements) {
        $WORD w = $next(iter);
        $WORD key = ((B_setentry*)w)->key;
        long hash = ((B_setentry*)w)->hash;
        if (B_set_contains_entry(other,hashwit,key,hash))
            B_set_add_entry(res,hashwit,key,hash);
        n++;
    }
    return res;
}

B_set B_LogicalD_SetD_setD___or__ (B_LogicalD_SetD_set wit, B_set set, B_set other) {
    B_Hashable hashwit = ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set;
    if (other->numelements > set->numelements)
        return B_LogicalD_SetD_setD___or__ (wit,other,set);
    B_set res = B_set_copy(set, hashwit);
    B_Iterator iter = B_set_iter_entry(other);
    long n = 0;
    while(n < other->numelements) {
        $WORD w = $next(iter);
        $WORD key = ((B_setentry*)w)->key;
        long hash = ((B_setentry*)w)->hash;
        B_set_add_entry(res,hashwit,key,hash);
        n++;
    }
    return res;
}

B_set B_LogicalD_SetD_setD___xor__(B_LogicalD_SetD_set wit, B_set set, B_set other) {
    B_Hashable hashwit = ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set;
    B_set res = B_set_copy(set, hashwit);
    B_Iterator iter = B_set_iter_entry(other);
    long n = 0;
    while(n < other->numelements) {
        $WORD w = $next(iter);
        $WORD key = ((B_setentry*)w)->key;
        long hash = ((B_setentry*)w)->hash;
        if(!B_set_discard_entry(res,hashwit,key,hash))
            B_set_add_entry(res,hashwit,key,hash);
        n++;
    }
    return res;
}

#undef dummy
