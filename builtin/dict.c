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
#define TB_ENTRIES(tb)                                          \
    (($entry_t)(&((int*)((tb)->tb_indices))[(tb)->tb_size]))

#define PERTURB_SHIFT 5

GC_word B_dictD_gcbm[GC_BITMAP_SIZE(struct B_dict)];

// Internal routines //////////////////////////////////////////////////////////////////

/*
  Internal routine used by dictresize() to build a hashtable of entries.
%*/
static void build_indices($table tbl, $entry_t ep, long n) {
    long mask = tbl->tb_size - 1;
    for (int ix = 0; ix != n; ix++, ep++) {
        long hash = ep->hash;

        unsigned long i = (unsigned long)hash & mask;
        for (unsigned long perturb = hash; tbl->tb_indices[i] != DKIX_EMPTY;) {
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

static int dictresize(B_dict d) {
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
static int $lookdict_index($table table, long hash, int index) {
    unsigned long mask =  (table->tb_size)-1;
    unsigned long perturb = hash;
    unsigned long i = (unsigned long)hash & mask;

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
int $lookdict(B_dict dict, B_Hashable hashwit, long hash, $WORD key, $WORD *res) {
    $table table = dict->table;
    unsigned long mask = (table->tb_size)-1, i = (unsigned long)hash & mask, perturb = hash;
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
            if (entry->value != NULL && (entry->key == key || (entry->hash == hash && hashwit->$class->__eq__(hashwit,key,entry->key)->val))) {
                // found an entry with the same or equal key
                *res = entry->value;
                return ix;
            }
            // collision; probe another location
        }
        perturb >>= PERTURB_SHIFT;
        i = (i*5 + perturb + 1) & mask;
        //printf("collision; perturb is %ld, hash is %ld, mask is %ld, next probe is %ld\n", perturb,  hash, mask, i);
    }
    // this should be unreachable
}

//  Internal function to find slot in index array for an item from its hash
//  when it is known that the key is not present in the dict.
  
static long find_empty_slot($table table, long hash) {
    const unsigned long mask = (table->tb_size)-1;

    unsigned long i = (unsigned long)hash & mask;
    int ix = table->tb_indices[i];
    for (unsigned long perturb = hash; ix >= 0;) {
        perturb >>= PERTURB_SHIFT;
        i = (i*5 + perturb + 1) & mask;
        ix = table->tb_indices[i];
    }
    return i;
}

static int insertdict(B_dict dict, B_Hashable hashwit, long hash, $WORD key, $WORD value) {
    $WORD old_value;
    $table table;
    $entry_t ep;
    int ix = $lookdict(dict,hashwit,hash,key,&old_value);
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

// General methods /////////////////////////////////////////////////////////////////////////

B_dict B_dictG_new(B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return $NEW(B_dict,hashwit, wit, iterable);
}

B_NoneType B_dictD___init__(B_dict dict, B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    dict->numelements = 0;
    dict->table = malloc(sizeof(char*)+3*sizeof(long) + 8*sizeof(int) + 5*sizeof(struct $entry_struct));
    dict->table->tb_size = 8;
    dict->table->tb_usable = 5;
    dict->table->tb_nentries = 0;
    memset(&(dict->table->tb_indices[0]), 0xff, 8*sizeof(int));
    if (wit && iterable) {
        B_Iterator it = wit->$class->__iter__(wit,iterable);
        B_tuple nxt;
        while((nxt = (B_tuple)it->$class->__next__(it))) {
            B_dictD_setitem(dict,hashwit,nxt->components[0],nxt->components[1]);
        }
    }
    return B_None;
}

B_bool B_dictD___bool__(B_dict self) {
    return toB_bool(self->numelements>0);
}

B_str B_dictD___str__(B_dict self) {
    B_list s2 = B_listD_new(self->numelements);
    B_IteratorD_dict_items iter = $NEW(B_IteratorD_dict_items,self);
    B_tuple item;
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    for (int i=0; i<self->numelements; i++) {
        item = (B_tuple)iter->$class->__next__(iter);
        B_value key = ((B_value)item->components[0]);
        B_value value = ((B_value)item->components[1]);
        B_str keystr = key->$class->__repr__(key);
        B_str valuestr = value->$class->__repr__(value);
        B_str elem = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct B_str), B_strG_methods.$GCdescr);
        elem->$class = &B_strG_methods;
        elem->nbytes = keystr->nbytes+valuestr->nbytes+1;
        elem->nchars = keystr->nchars+valuestr->nchars+1;
        elem->str = GC_MALLOC_ATOMIC(elem->nbytes+1);
        memcpy(elem->str,keystr->str,keystr->nbytes);
        elem->str[keystr->nbytes] = ':';
        memcpy(&elem->str[keystr->nbytes+1],valuestr->str,valuestr->nbytes);
        elem->str[elem->nbytes] = '\0';    
        wit->$class->append(wit,s2,elem);
    }
    return B_strD_join_par('{',s2,'}');
}

B_str B_dictD___repr__(B_dict self) {
    return B_dictD___str__(self);
}

void B_dictD___serialize__(B_dict self,$Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        long pk = from$int(prevkey);
        $val_serialize(-DICT_ID,&pk,state);
        return;
    }
    B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,to$int(state->row_no));
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

B_dict B_dictD___deserialize__(B_dict res, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,to$int((long)this->blob[0]),NULL);
    } else {
        if (!res)
            res = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct B_dict), B_dictG_methods.$GCdescr);
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,to$int(state->row_no-1),res);
        res->$class = &B_dictG_methods;
        res->numelements = (long)this->blob[0];
        long tb_size = (long)this->blob[1];
        res->table = malloc(sizeof(char*) + 3*sizeof(long) + tb_size*sizeof(int) + (2*tb_size/3)*sizeof(struct $entry_struct));
        res->table->tb_size = tb_size;
        res->table->tb_usable = (long)this->blob[2];
        res->table->tb_nentries = (long)this->blob[3];
        memcpy(res->table->tb_indices,&this->blob[4],tb_size*sizeof(int));
        for (int i=0; i<res->table->tb_nentries; i++) {
            $entry_t entry = &TB_ENTRIES(res->table)[i];
            entry->hash = from$int((B_int)$step_deserialize(state));
            entry->key =  $step_deserialize(state);
            entry->value = $step_deserialize(state);
        }
        return res;
    }
}

// B_OrdD_dict ////////////////////////////////////////////////////////////////////////

B_bool B_dictrel(bool directfalse,B_OrdD_dict w, B_dict a, B_dict b) {
    if (directfalse) {
        return B_False;
    }; 
    B_Hashable wH = w->W_HashableD_AD_OrdD_dict;
    B_Eq wB = w->W_EqD_BD_OrdD_dict;
    B_MappingD_dict m = B_MappingD_dictG_new(wH);
    B_Iterator it = m->$class->keys(m,a);
    $WORD x,resa,resb;
    while ((x = $next(it))) {
        long h = from$int(wH->$class->__hash__(wH,x));
        int ixa = $lookdict(a, wH, h, x, &resa);
        int ixb = $lookdict(b, wH, h, x ,&resb);
        if (ixb<0 || wB->$class->__ne__(wB,resa,resb)->val) return B_False;
    }
    return B_True;
}

B_bool B_OrdD_dictD___eq__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return B_dictrel(a->numelements != b->numelements,w,a,b);
}

B_bool B_OrdD_dictD___ne__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return toB_bool(!(w->$class->__eq__(w,a,b)->val));
}

B_bool B_OrdD_dictD___lt__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return B_dictrel(a->numelements >= b->numelements,w,a,b);
}

B_bool B_OrdD_dictD___le__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return B_dictrel(a->numelements > b->numelements,w,a,b);
}

B_bool B_OrdD_dictD___gt__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return toB_bool(!(w->$class->__lt__(w,b,a)->val));
}

B_bool B_OrdD_dictD___ge__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return toB_bool(!(w->$class->__le__(w,b,a)->val));
}

// B_MappingD_dict ///////////////////////////////////////////////////////////////

// First, define Iterator class for keys  //////////////////////////////////////////////////////////////////////////////
 
static $WORD B_IteratorD_dictD_next(B_IteratorD_dict self) {
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

B_IteratorD_dict B_IteratorD_dictG_new(B_dict dict) {
    return $NEW(B_IteratorD_dict, dict);
}
 
void B_IteratorD_dictD_init(B_IteratorD_dict self, B_dict dict) {
    self->src = dict;
    self->nxt = 0;
}


B_bool B_IteratorD_dictD_bool(B_IteratorD_dict self) {
    return B_True;
}

B_str B_IteratorD_dictD_str(B_IteratorD_dict self) {
    char *s;
    asprintf(&s,"<dict keys iterator object at %p>",self);
    return to$str(s);
}

void B_IteratorD_dictD_serialize(B_IteratorD_dict self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}


B_IteratorD_dict B_IteratorD_dict__deserialize(B_IteratorD_dict res, $Serial$state state) {
    if (!res)
        res = $DNEW( B_IteratorD_dict,state);
    res->src = (B_dict)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}


struct B_IteratorD_dictG_class B_IteratorD_dictG_methods = {0,"B_IteratorD_dict",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_dictD_init,                                                      B_IteratorD_dictD_serialize, B_IteratorD_dict__deserialize, B_IteratorD_dictD_bool,B_IteratorD_dictD_str,B_IteratorD_dictD_str, B_IteratorD_dictD_next};


B_Iterator B_MappingD_dictD___iter__ (B_MappingD_dict wit, B_dict dict) {
    return (B_Iterator)$NEW(B_IteratorD_dict,dict);
}

B_dict B_MappingD_dictD___fromiter__ (B_MappingD_dict wit, B_Iterable wit2, $WORD iter) {
    B_Iterator it = wit2->$class->__iter__(wit2,iter);
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    B_dict dict = $NEW(B_dict,hashwit,NULL,NULL);
    dict->numelements = 0;
    dict->table = malloc(sizeof(char*)+3*sizeof(long) + 8*sizeof(int) + 5*sizeof(struct $entry_struct));
    dict->table->tb_size = 8;
    dict->table->tb_usable = 5;
    dict->table->tb_nentries = 0;
    memset(&(dict->table->tb_indices[0]), 0xff, 8*sizeof(int));
    B_tuple nxt;
    while((nxt = (B_tuple)it->$class->__next__(it))) {
        B_dictD_setitem(dict,hashwit,nxt->components[0],nxt->components[1]);
    }
    return dict;
}

B_int B_MappingD_dictD___len__ (B_MappingD_dict wit, B_dict dict) {
    return to$int(dict->numelements);
}
  
B_bool B_MappingD_dictD___contains__ (B_MappingD_dict wit, B_dict dict, $WORD key) {
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    $WORD res;
    return toB_bool($lookdict(dict,hashwit,from$int(hashwit->$class->__hash__(hashwit,key)),key,&res) >= 0);
}

B_bool B_MappingD_dictD___containsnot__ (B_MappingD_dict wit, B_dict dict, $WORD key) {
    return toB_bool(!B_MappingD_dictD___contains__(wit, dict, key)->val);
}

$WORD B_MappingD_dictD_get (B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt) {
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    long hash = from$int(hashwit->$class->__hash__(hashwit,key));
    $WORD res;
    int ix = $lookdict(dict,hashwit,hash,key,&res);
    if (ix < 0) 
        return deflt;
    else
        return res;
}

B_Iterator B_MappingD_dictD_keys (B_MappingD_dict wit, B_dict dict) {
    return (B_Iterator)$NEW(B_IteratorD_dict,dict);
}

// Iterator classes for values and items

// values iterator

static $WORD B_IteratorD_dict_values_next(B_IteratorD_dict_values self) {
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
 
B_IteratorD_dict_values B_IteratorD_dict_valuesG_new(B_dict dict) {
    return $NEW(B_IteratorD_dict_values, dict);
}
 
void B_IteratorD_dict_values_init(B_IteratorD_dict_values self, B_dict dict) {
    self->src = dict;
    self->nxt = 0;
}


B_bool B_IteratorD_dict_values_bool(B_IteratorD_dict_values self) {
    return B_True;
}

B_str B_IteratorD_dict_values_str(B_IteratorD_dict_values self) {
    char *s;
    asprintf(&s,"<dict values iterator object at %p>",self);
    return to$str(s);
}

void B_IteratorD_dict_values_serialize(B_IteratorD_dict_values self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}

B_IteratorD_dict_values B_IteratorD_dict_values_deserialize(B_IteratorD_dict_values res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_dict_values,state);
    res->src = (B_dict)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}

struct B_IteratorD_dict_valuesG_class B_IteratorD_dict_valuesG_methods = {0,"B_IteratorD_dict_values",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_dict_values_init,                                                                    B_IteratorD_dict_values_serialize, B_IteratorD_dict_values_deserialize, B_IteratorD_dict_values_bool, B_IteratorD_dict_values_str,B_IteratorD_dict_values_str,
                                                                    B_IteratorD_dict_values_next};

// items iterator

static $WORD B_IteratorD_dict_items_next(B_IteratorD_dict_items self) {
    int i = self->nxt;
    $table table = self->src->table;
    int n = table->tb_nentries;
    while (i < n) {
        $entry_t entry =  &TB_ENTRIES(table)[i];
        if (entry->value != NULL) {
            self->nxt = i+1;
            return $NEWTUPLE(2,entry->key,entry->value);
        }
        i++;
    }
    return NULL;
}
 
B_IteratorD_dict_items B_IteratorD_dict_itemsG_new(B_dict dict) {
    return $NEW(B_IteratorD_dict_items, dict);
}
 
void B_IteratorD_dict_items_init(B_IteratorD_dict_items self, B_dict dict) {
    self->src = dict;
    self->nxt = 0;
}


B_bool B_IteratorD_dict_items_bool(B_IteratorD_dict_items self) {
    return B_True;
}

B_str B_IteratorD_dict_items_str(B_IteratorD_dict_items self) {
    char *s;
    asprintf(&s,"<dict items iterator object at %p>",self);
    return to$str(s);
}

void B_IteratorD_dict_items_serialize(B_IteratorD_dict_items self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}

B_IteratorD_dict_items B_IteratorD_dict_items_deserialize(B_IteratorD_dict_items res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_dict_items,state);
    res->src = (B_dict)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}



struct B_IteratorD_dict_itemsG_class B_IteratorD_dict_itemsG_methods = {0,"B_IteratorD_dict_items",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_dict_items_init,                                                                  B_IteratorD_dict_items_serialize, B_IteratorD_dict_items_deserialize,B_IteratorD_dict_items_bool, B_IteratorD_dict_items_str, B_IteratorD_dict_items_str, B_IteratorD_dict_items_next};


B_Iterator B_MappingD_dictD_values (B_MappingD_dict wit, B_dict dict) {
    return (B_Iterator)$NEW(B_IteratorD_dict_values, dict);
}

B_Iterator B_MappingD_dictD_items (B_MappingD_dict wit, B_dict dict) {
    return (B_Iterator)$NEW(B_IteratorD_dict_items, dict);
}

B_NoneType B_MappingD_dictD_update (B_MappingD_dict wit, B_dict dict, B_Iterable wit2, $WORD other) {
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    B_Iterator it = wit2->$class->__iter__(wit2,other);
    B_tuple item;
    while ((item = (B_tuple)it->$class->__next__(it)))
        B_dictD_setitem(dict,hashwit,item->components[0],item->components[1]);
    return B_None;
}

B_tuple B_MappingD_dictD_popitem (B_MappingD_dict wit, B_dict dict) {
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    $table table = dict->table;
    int ix = table->tb_nentries-1;
    while (ix >= 0) {
        $entry_t entry =  &TB_ENTRIES(table)[ix];
        if (entry->value != NULL) {
            long hash = from$int(hashwit->$class->__hash__(hashwit,entry->key));
            int i = $lookdict_index(table,hash,ix);
            table->tb_indices[i] = DKIX_DUMMY;
            dict->numelements--;
            table->tb_nentries = ix;
            return $NEWTUPLE(2,entry->key,entry->value);
        }
        ix--;
    }
    return NULL;
}

B_NoneType B_MappingD_dictD_setdefault (B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt) {
    if (!deflt) deflt = B_None;
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    long hash = from$int(hashwit->$class->__hash__(hashwit,key));
    $WORD value;
    int ix = $lookdict(dict,hashwit,hash,key,&value);
    if (ix >= 0)
        return value;
    TB_ENTRIES(dict->table)[ix].value = deflt;
    return deflt;
}
 

// B_IndexedD_MappingD_dict ///////////////////////////////////////////////////////////////////////

$WORD B_IndexedD_MappingD_dictD___getitem__(B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key) {
    B_Hashable hashwit =  ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingD_dict;
    long hash = from$int(hashwit->$class->__hash__(hashwit,key));
    $WORD res;
    int ix = $lookdict(dict,hashwit,hash,key,&res);
    if (ix < 0)  {
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("getitem: key not in dictionary")));
    }      
    return res;
}

B_NoneType B_IndexedD_MappingD_dictD___setitem__ (B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key, $WORD value) {
    B_dictD_setitem(dict, ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingD_dict,key,value);
    return B_None;
}
B_NoneType B_IndexedD_MappingD_dictD___delitem__ (B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key) {
    B_Hashable hashwit =  ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingD_dict;
    long hash = from$int(hashwit->$class->__hash__(hashwit,key));
    $WORD res;
    int ix = $lookdict(dict,hashwit,hash,key,&res);
    $table table = dict->table;
    if (ix < 0)  {
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("getitem: key not in dictionary")));
    }      
    $entry_t entry = &TB_ENTRIES(table)[ix];
    int i = $lookdict_index(table,hash,ix);
    table->tb_indices[i] = DKIX_DUMMY;
    res = entry->value;
    if (res == NULL) {
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("delitem: key not in dictionary")));
    }
    entry->value = NULL;
    dict->numelements--;
    if (10*dict->numelements < dict->table->tb_size) 
        dictresize(dict);
    return B_None;
}

/*
B_MappingD_dict B_MappingD_dictG_new(B_Hashable h) {
    return $NEW(B_MappingD_dict, h);
}

B_NoneType B_MappingD_dictD___init__(B_MappingD_dict self, B_Hashable h) {
    self-> W_EqD_AD_Container = (B_Eq)h;
    self->W_Indexed = (B_Indexed)$NEW(B_IndexedD_MappingD_dict,h,(B_Mapping)self);
    self->W_EqD_AD_Mapping = (B_Eq)h;
    self->W_HashableD_AD_MappingD_dict = h;
    return B_None;
}

B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictG_new(B_Hashable h, B_Mapping master) {
    return $NEW(B_IndexedD_MappingD_dict, h, master);
}


B_NoneType B_IndexedD_MappingD_dictD___init__(B_IndexedD_MappingD_dict self,  B_Hashable h, B_Mapping master) {
    self->W_EqD_AD_Indexed = (B_Eq)h;
    self->W_EqD_AD_Mapping =  (B_Eq)h;
    self->W_Mapping = master;
    self->W_HashableD_AD_MappingD_dict = h;
    return B_None;
}
    B_Eq W_EqD_AD_Indexed;
    B_Eq W_EqD_AD_Mapping;
    B_Mapping W_Mapping;
    B_Hashable W_HashableD_AD_MappingD_dict;

struct  B_MappingD_dictG_class B_MappingD_dictG_methods = {
    "B_MappingD_dict",
    UNASSIGNED,
    ($SuperG_class)&B_MappingG_methods,
    B_MappingD_dictD___init__,
    B_MappingD_dictD___serialize__,
    B_MappingD_dictD___deserialize__,
    (B_bool (*)(B_MappingD_dict))$default__bool__,
    (B_str (*)(B_MappingD_dict))$default__str__,
    (B_str (*)(B_MappingD_dict))$default__str__,
    B_MappingD_dictD___iter__,
    B_MappingD_dictD___fromiter__,
    B_MappingD_dictD___len__,
    B_MappingD_dictD___contains__,
    B_MappingD_dictD___containsnot__,
    B_MappingD_dictD_get,
    B_MappingD_dictD_keys,
    B_MappingD_dictD_values,
    B_MappingD_dictD_items,
    B_MappingD_dictD_update,
    B_MappingD_dictD_popitem,
    B_MappingD_dictD_setdefault
};

struct B_IndexedD_MappingD_dictG_class B_IndexedD_MappingD_dictG_methods = {
    0,
    "B_IndexedD_MappingD_dict",
    UNASSIGNED,
    ($SuperG_class)&B_IndexedG_methods,
    B_IndexedD_MappingD_dictD___init__,
    B_IndexedD_MappingD_dictD___serialize__,
    B_IndexedD_MappingD_dictD___deserialize__,
    (B_bool (*)(B_IndexedD_MappingD_dict))$default__bool__,
    (B_str (*)(B_IndexedD_MappingD_dict))$default__str__,
    (B_str (*)(B_IndexedD_MappingD_dict))$default__str__,
    B_IndexedD_MappingD_dictD___getitem__,
    B_IndexedD_MappingD_dictD___setitem__,
    B_IndexedD_MappingD_dictD___delitem__
};


struct B_OrdD_dictG_class B_OrdD_dictG_methods = {
    0,
    "B_OrdD_dict",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    B_OrdD_dictD___init__,
    B_OrdD_dictD___serialize__,
    B_OrdD_dictD___deserialize__,
    (B_bool (*)(B_OrdD_dict))$default__bool__,
    (B_str (*)(B_OrdD_dict))$default__str__,
    (B_str (*)(B_OrdD_dict))$default__str__,
    B_OrdD_dictD___eq__,
    B_OrdD_dictD___ne__,
    B_OrdD_dictD___lt__,
    B_OrdD_dictD___le__,
    B_OrdD_dictD___gt__,
    B_OrdD_dictD___ge__
};

*/

void B_dictD_setitem(B_dict dict, B_Hashable hashwit, $WORD key, $WORD value) {
    long hash = from$int(hashwit->$class->__hash__(hashwit,key));
    if (insertdict(dict, hashwit, hash, key, value)<0) {
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("setitem: key not in dictionary")));
    }      
}

$WORD B_dictD_get(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt) {
    long hash = from$int(hashwit->$class->__hash__(hashwit,key));
    $WORD res;
    int ix = $lookdict(dict,hashwit,hash,key,&res);
    if (ix < 0) 
        return deflt;
    else
        return res;
}
