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
    $WORD value;  // deleted entry has value DELETED
} *$entry_t;

struct $table_struct {
    char *$GCINFO;
    long tb_size;        // size of dk_indices array; must be power of 2
    long tb_usable;      // nr of unused entries in tb_entries (deleted entries are counted as used)
    long tb_nentries;    // nr of used entries in tb_entries
    int  tb_indices[];   // array of indices
    // after this follows tb_entries array;
};

#define DELETED (($WORD)1)
#define INIT_SIZE 4
#define DKIX_EMPTY (-1)
#define DKIX_DUMMY (-2)  /* Used internally */
#define TB_ENTRIES(tb)                                          \
    (($entry_t)(&((int*)((tb)->tb_indices))[(tb)->tb_size]))

#define PERTURB_SHIFT 5

// Internal routines //////////////////////////////////////////////////////////////////

/*
  Internal routine used by dictresize() to build a hashtable of entries.
%*/
static void build_indices(B_Hashable hashwit, $table oldtable, $table newtable, $entry_t ep, long n) {
    long mask = newtable->tb_size - 1;
    for (int ix = 0; ix < n; ix++, ep++) {
        uint64_t hash;
        if (oldtable->tb_size > INIT_SIZE)
            hash = ep->hash;
        else {
            hash = B_hash(hashwit, ep->key);
            ep->hash = hash;
        }
        unsigned long i = (unsigned long)hash & mask;
        for (unsigned long perturb = hash; newtable->tb_indices[i] != DKIX_EMPTY;) {
            perturb >>= PERTURB_SHIFT;
            i = mask & (i*5 + perturb + 1);
        }
        newtable->tb_indices[i] = ix;
    }
}

/*
  Restructure the table by allocating a new table and reinserting all
  items again.  When entries have been deleted, the new table may
  actually be smaller than the old one.
*/

static int dictresize(B_Hashable hashwit, B_dict_base d) {
    $table oldtable = d->table;
    long numelements = d->numelements;
    long newsize, minsize = 3*numelements;
    $entry_t oldentries, newentries;
    for (newsize = INIT_SIZE; newsize < minsize; newsize <<= 1);
    /* Allocate a new table. */
    $table newtable =  acton_malloc(sizeof(char*) + 3*sizeof(long) + newsize*sizeof(int) + (2*newsize/3)*sizeof(struct $entry_struct));
    newtable->tb_size = newsize;
    newtable->tb_usable = 2*newsize/3-numelements;
    newtable->tb_nentries = numelements;
    memset(&(newtable->tb_indices[0]), 0xff, newsize*sizeof(int));
    newentries = TB_ENTRIES(newtable);
    if (numelements > 0) {
        oldentries = TB_ENTRIES(oldtable);
        if (oldtable->tb_nentries == numelements) {
            memcpy(newentries, oldentries, numelements*sizeof(struct $entry_struct));
        }
        else {
            $entry_t ep = oldentries;
            for (int i = 0; i < numelements; i++) {
                while (ep->value == DELETED) ep++;
                newentries[i] = *ep++;
            }
        }
        if (newsize > INIT_SIZE)
            build_indices(hashwit, oldtable, newtable, newentries, numelements);
    }
    // for (int i=0; i < newsize; i++) printf("%d -> %d\n",i,newtable->tb_indices[i]);
    // for (int i=0; i < newtable->tb_nentries; i++){
    //   $entry_t e = newentries + i;
        // printf("%ld     %lu\n", e->hash, ((B_int)e->key)->val.n[0])
        
    d->table = newtable;
    return 0;
}


// Search index of hash table from offset of entry table
// Only called when the dict is a hashtable (i.e. table->tb_size > INIT_SIZE)
static int $lookdict_index($table table, uint64_t hash, int index) {
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
static int $lookdict(B_dict_base dict, B_Hashable hashwit, uint64_t hash, $WORD key, $WORD *res) {
    $table table = dict->table;
    if (!table) {
        *res = NULL;
        //printf("no table\n");
        return DKIX_EMPTY;
    }
    if (table->tb_size == INIT_SIZE) {
        // Ignore hash and do linear search
        for (int ix = 0; ix < (int)table->tb_nentries; ix++) {
            $entry_t entry = &TB_ENTRIES(table)[ix];
            if (entry->value != DELETED && (entry->key == key || (hashwit->$class->__eq__(hashwit,key,entry->key)))) {
                // found an entry with the same or equal key
                *res = entry->value; 
                return ix;
            }
        }
        //printf("failed linear search\n");
        return DKIX_EMPTY;
    } else {
        unsigned long mask = (table->tb_size)-1, i = (unsigned long)hash & mask, perturb = hash;
        int ix;
        for(;;) {
            ix = table->tb_indices[i];
            // printf("ix = %d\n",ix);
            if (ix == DKIX_EMPTY) {
                // Unused slot
                *res = NULL;
                // printf("found unused slot\n");
                return ix;
            }
            if (ix >= 0) {
                $entry_t entry = &TB_ENTRIES(table)[ix];
                if (entry->value != DELETED && (entry->key == key || (entry->hash == hash && hashwit->$class->__eq__(hashwit,key,entry->key)))) {
                    // found an entry with the same or equal key
                    *res = entry->value;
                    return ix;
                }
                // collision; probe another location
            }
            perturb >>= PERTURB_SHIFT;
            i = (i*5 + perturb + 1) & mask;
            // printf("collision; perturb is %ld, hash is %ld, mask is %ld, next probe is %ld\n", perturb,  hash, mask, i);
        }
        // this should be unreachable
    }
}

//  Internal function to find slot in index array for an item from its hash
//  when it is known that the key is not present in the dict.
  
static long find_empty_slot($table table, uint64_t hash) {
    if (table->tb_size==INIT_SIZE)
        return table->tb_nentries;
    const unsigned long mask = table->tb_size-1;
    unsigned long i = (unsigned long)hash & mask;
    int ix = table->tb_indices[i];
    for (unsigned long perturb = hash; ix >= 0;) {
        perturb >>= PERTURB_SHIFT;
        i = (i*5 + perturb + 1) & mask;
        ix = table->tb_indices[i];
    }
    return i;
}

static void insertdict(B_dict_base dict, B_Hashable hashwit, uint64_t hash, $WORD key, $WORD value) {
    $WORD old_value;
    $table table;
    $entry_t ep;
    if (!dict->table || dict->table->tb_usable <= 0)
        dictresize(hashwit,dict);
    if (dict->table->tb_size == 2*INIT_SIZE)
         hash = B_hash(hashwit, key);
    int ix = $lookdict(dict,hashwit,hash,key,&old_value);
    if (ix == DKIX_EMPTY) {
        table = dict->table;
        long newpos = find_empty_slot(table,hash);
        ep = &TB_ENTRIES(table)[table->tb_nentries];
        table->tb_indices[newpos] = table->tb_nentries;
        ep->key = key;
        ep->hash = hash;
        ep->value = value;
        table->tb_usable--;
        table->tb_nentries++;
        dict->numelements++;
        return;
    }
    if (old_value != value)  //eq ??
        TB_ENTRIES(dict->table)[ix].value = value;
    return;
}

// Generic dict/idict object implementation ///////////////////////////////////////////////////////

static B_dict_base B_dict_base_new($SuperG_class cls, B_Hashable hashwit,
                                   B_Iterable wit, $WORD iterable) {
    B_dict_base dict = acton_malloc(sizeof(struct B_dict_base));
    if (dict == NULL)
        $RAISE((B_BaseException)$NEW(B_MemoryError, to$str("memory allocation failed")));
    dict->$class = cls;
    dict->numelements = 0;
    dict->table = NULL;
    if (wit && iterable) {
        B_Iterator it = wit->$class->__iter__(wit, iterable);
        $WORD nxt;
        while (it->$class->__next__(it, &nxt)) {
            B_tuple val = (B_tuple)nxt;
            uint64_t hash = dict->table && dict->table->tb_size > INIT_SIZE
                ? B_hash(hashwit, val->components[0]) : 0;
            insertdict(dict, hashwit, hash, val->components[0], val->components[1]);
        }
    }
    return dict;
}

static B_NoneType B_dict_base_init(B_dict_base dict, B_Hashable hashwit,
                                   B_Iterable wit, $WORD iterable) {
    dict->numelements = 0;
    dict->table = NULL;
    if (wit && iterable) {
        B_Iterator it = wit->$class->__iter__(wit, iterable);
        $WORD nxt;
        while (it->$class->__next__(it, &nxt)) {
            B_tuple val = (B_tuple)nxt;
            uint64_t hash = dict->table && dict->table->tb_size > INIT_SIZE
                ? B_hash(hashwit, val->components[0]) : 0;
            insertdict(dict, hashwit, hash, val->components[0], val->components[1]);
        }
    }
    return B_None;
}

static bool B_dict_base_bool(B_dict_base self) {
    return self->numelements > 0;
}

static B_str B_dict_base_str(B_dict_base self) {
    B_list parts = B_listD_new(self->numelements);
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (self->table) {
        for (int i = 0; i < self->table->tb_nentries; i++) {
            $entry_t entry = &TB_ENTRIES(self->table)[i];
            if (entry->value == DELETED)
                continue;
            B_value key = (B_value)entry->key;
            B_value value = (B_value)entry->value;
            B_str keystr = key->$class->__repr__(key);
            B_str valuestr = value ? value->$class->__repr__(value) : to$str("None");
            B_str elem = acton_malloc(sizeof(struct B_str));
            elem->$class = &B_strG_methods;
            elem->nbytes = keystr->nbytes + valuestr->nbytes + 1;
            elem->nchars = keystr->nchars + valuestr->nchars + 1;
            elem->str = acton_malloc(elem->nbytes + 1);
            memcpy(elem->str, keystr->str, keystr->nbytes);
            elem->str[keystr->nbytes] = ':';
            memcpy(&elem->str[keystr->nbytes + 1], valuestr->str, valuestr->nbytes);
            elem->str[elem->nbytes] = '\0';
            wit->$class->append(wit, parts, elem);
        }
    }
    return B_strD_join_par('{', parts, '}');
}

static void B_dict_base_serialize_payload(B_dict_base self, int class_id,
                                          $Serial$state state) {
    long tb_size = self->table ? self->table->tb_size : 0;
    int blobsize = 4 + (tb_size + 1) * sizeof(int) / sizeof($WORD);
    $ROW row = $add_header(class_id, blobsize, state);
    row->blob[0] = ($WORD)self->numelements;
    row->blob[1] = ($WORD)tb_size;
    row->blob[2] = ($WORD)(self->table ? self->table->tb_usable : 0);
    row->blob[3] = ($WORD)(self->table ? self->table->tb_nentries : 0);
    if (!self->table)
        return;
    memcpy(&row->blob[4], self->table->tb_indices, tb_size * sizeof(int));
    for (int i = 0; i < self->table->tb_nentries; i++) {
        $entry_t entry = &TB_ENTRIES(self->table)[i];
        $step_serialize(toB_u64(entry->hash), state);
        $step_serialize(entry->key, state);
        $step_serialize(entry->value, state);
    }
}

static void B_dict_base_serialize(B_dict_base self, int class_id, $Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done, (B_Hashable)B_HashableD_WORDG_witness,
                                      self, NULL);
    if (prevkey) {
        long pk = fromB_int(prevkey);
        $val_serialize(-class_id, &pk, state);
        return;
    }
    B_dictD_setitem(state->done, (B_Hashable)B_HashableD_WORDG_witness,
                    self, toB_int(state->row_no));
    B_dict_base_serialize_payload(self, class_id, state);
}

static void B_dict_base_deserialize_payload(B_dict_base res, $ROW row,
                                            $Serial$state state) {
    res->numelements = (long)row->blob[0];
    long tb_size = (long)row->blob[1];
    if (tb_size == 0) {
        res->table = NULL;
        return;
    }
    res->table = acton_malloc(sizeof(char *) + 3 * sizeof(long) + tb_size * sizeof(int)
                              + (2 * tb_size / 3) * sizeof(struct $entry_struct));
    res->table->tb_size = tb_size;
    res->table->tb_usable = (long)row->blob[2];
    res->table->tb_nentries = (long)row->blob[3];
    memcpy(res->table->tb_indices, &row->blob[4], tb_size * sizeof(int));
    for (int i = 0; i < res->table->tb_nentries; i++) {
        $entry_t entry = &TB_ENTRIES(res->table)[i];
        entry->hash = fromB_u64((B_u64)$step_deserialize(state));
        entry->key = $step_deserialize(state);
        entry->value = $step_deserialize(state);
    }
}

static B_dict_base B_dict_base_deserialize(B_dict_base res, $SuperG_class cls,
                                           $Serial$state state) {
    $ROW row = state->row;
    state->row = row->next;
    state->row_no++;
    if (row->class_id < 0)
        return (B_dict_base)B_dictD_get(state->done, (B_Hashable)B_HashableD_intG_witness,
                                       toB_int((long)row->blob[0]), NULL);

    if (!res)
        res = acton_malloc(sizeof(struct B_dict_base));
    B_dictD_setitem(state->done, (B_Hashable)B_HashableD_intG_witness,
                    toB_int(state->row_no - 1), res);
    res->$class = cls;
    B_dict_base_deserialize_payload(res, row, state);
    return res;
}

// dict object methods ////////////////////////////////////////////////////////////////////////////

B_dict B_dictG_new(B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return (B_dict)B_dict_base_new(($SuperG_class)&B_dictG_methods, hashwit, wit, iterable);
}

B_NoneType B_dictD___init__(B_dict self, B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return B_dict_base_init((B_dict_base)self, hashwit, wit, iterable);
}

bool B_dictD___bool__(B_dict self) {
    return B_dict_base_bool((B_dict_base)self);
}

B_str B_dictD___str__(B_dict self) {
    return B_dict_base_str((B_dict_base)self);
}

B_str B_dictD___repr__(B_dict self) {
    return B_dictD___str__(self);
}

void B_dictD___serialize__(B_dict self, $Serial$state state) {
    B_dict_base_serialize((B_dict_base)self, DICT_ID, state);
}

B_dict B_dictD___deserialize__(B_dict res, $Serial$state state) {
    return (B_dict)B_dict_base_deserialize((B_dict_base)res,
                                           ($SuperG_class)&B_dictG_methods, state);
}

// Freeze[dict] ////////////////////////////////////////////////////////////////////////////////////

B_idict B_FreezeD_dictD_freeze(B_FreezeD_dict wit, B_dict self) {
    B_idict frozen = (B_idict)self;
    frozen->$class = &B_idictG_methods;
    return frozen;
}

// idict object methods ///////////////////////////////////////////////////////////////////////////

B_idict B_idictG_new(B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return (B_idict)B_dict_base_new(($SuperG_class)&B_idictG_methods, hashwit, wit, iterable);
}

B_NoneType B_idictD___init__(B_idict self, B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return B_dict_base_init((B_dict_base)self, hashwit, wit, iterable);
}

bool B_idictD___bool__(B_idict self) {
    return B_dict_base_bool((B_dict_base)self);
}

B_str B_idictD___str__(B_idict self) {
    B_str contents = B_dict_base_str((B_dict_base)self);
    return $FORMAT("idict(%s)", contents->str);
}

B_str B_idictD___repr__(B_idict self) {
    return B_idictD___str__(self);
}

void B_idictD___serialize__(B_idict self, $Serial$state state) {
    B_dict_base_serialize_payload((B_dict_base)self, IDICT_ID, state);
}

B_idict B_idictD___deserialize__(B_idict res, $Serial$state state) {
    if (!res) {
        res = acton_malloc(sizeof(struct B_idict));
        res->$class = &B_idictG_methods;
        B_dictD_setitem(state->done, (B_Hashable)B_HashableD_intG_witness,
                        toB_int(state->row_no - 1), res);
    }
    $ROW row = state->row;
    state->row = row->next;
    state->row_no++;
    res->$class = &B_idictG_methods;
    B_dict_base_deserialize_payload((B_dict_base)res, row, state);
    return res;
}

// Generic equality and ordering //////////////////////////////////////////////////////////////////

static bool B_dict_base_rel(bool directfalse, B_Hashable hashwit, B_Eq valuewit,
                            B_dict_base a, B_dict_base b) {
    if (directfalse)
        return false;
    if (a->numelements == 0)
        return true;
    if (!b->table)
        return false;

    for (int i = 0; i < a->table->tb_nentries; i++) {
        $entry_t entry = &TB_ENTRIES(a->table)[i];
        if (entry->value == DELETED)
            continue;
        uint64_t hash = b->table->tb_size > INIT_SIZE ? B_hash(hashwit, entry->key) : 0;
        $WORD other;
        int ix = $lookdict(b, hashwit, hash, entry->key, &other);
        if (ix < 0 || valuewit->$class->__ne__(valuewit, entry->value, other))
            return false;
    }
    return true;
}

// Ord[dict] wrappers /////////////////////////////////////////////////////////////////////////////

bool B_OrdD_dictD___eq__(B_OrdD_dict wit, B_dict a, B_dict b) {
    if (a == b)
        return true;
    return B_dict_base_rel(a->numelements != b->numelements,
                           wit->W_HashableD_AD_OrdD_dict, wit->W_EqD_BD_OrdD_dict,
                           (B_dict_base)a, (B_dict_base)b);
}

bool B_OrdD_dictD___ne__(B_OrdD_dict wit, B_dict a, B_dict b) {
    return !B_OrdD_dictD___eq__(wit, a, b);
}

bool B_OrdD_dictD___lt__(B_OrdD_dict wit, B_dict a, B_dict b) {
    return B_dict_base_rel(a->numelements >= b->numelements,
                           wit->W_HashableD_AD_OrdD_dict, wit->W_EqD_BD_OrdD_dict,
                           (B_dict_base)a, (B_dict_base)b);
}

bool B_OrdD_dictD___le__(B_OrdD_dict wit, B_dict a, B_dict b) {
    return B_dict_base_rel(a->numelements > b->numelements,
                           wit->W_HashableD_AD_OrdD_dict, wit->W_EqD_BD_OrdD_dict,
                           (B_dict_base)a, (B_dict_base)b);
}

bool B_OrdD_dictD___gt__(B_OrdD_dict wit, B_dict a, B_dict b) {
    return B_OrdD_dictD___lt__(wit, b, a);
}

bool B_OrdD_dictD___ge__(B_OrdD_dict wit, B_dict a, B_dict b) {
    return B_OrdD_dictD___le__(wit, b, a);
}

// Ord[idict] wrappers ////////////////////////////////////////////////////////////////////////////

bool B_OrdD_idictD___eq__(B_OrdD_idict wit, B_idict a, B_idict b) {
    if (a == b)
        return true;
    return B_dict_base_rel(a->numelements != b->numelements,
                           wit->W_HashableD_AD_OrdD_idict, wit->W_EqD_BD_OrdD_idict,
                           (B_dict_base)a, (B_dict_base)b);
}

bool B_OrdD_idictD___ne__(B_OrdD_idict wit, B_idict a, B_idict b) {
    return !B_OrdD_idictD___eq__(wit, a, b);
}

bool B_OrdD_idictD___lt__(B_OrdD_idict wit, B_idict a, B_idict b) {
    return B_dict_base_rel(a->numelements >= b->numelements,
                           wit->W_HashableD_AD_OrdD_idict, wit->W_EqD_BD_OrdD_idict,
                           (B_dict_base)a, (B_dict_base)b);
}

bool B_OrdD_idictD___le__(B_OrdD_idict wit, B_idict a, B_idict b) {
    return B_dict_base_rel(a->numelements > b->numelements,
                           wit->W_HashableD_AD_OrdD_idict, wit->W_EqD_BD_OrdD_idict,
                           (B_dict_base)a, (B_dict_base)b);
}

bool B_OrdD_idictD___gt__(B_OrdD_idict wit, B_idict a, B_idict b) {
    return B_OrdD_idictD___lt__(wit, b, a);
}

bool B_OrdD_idictD___ge__(B_OrdD_idict wit, B_idict a, B_idict b) {
    return B_OrdD_idictD___le__(wit, b, a);
}

// B_MappingD_dict ///////////////////////////////////////////////////////////////

// First, define Iterator class for keys  //////////////////////////////////////////////////////////////////////////////
static B_dict_base B_dict_base_from_iter_src($WORD src) {
    if (src == NULL)
        return NULL;
    $SuperG_class cls = (($Super)src)->$class;
    if (cls == ($SuperG_class)&B_dictG_methods || cls == ($SuperG_class)&B_idictG_methods)
        return (B_dict_base)src;
    $RAISE((B_BaseException)$NEW(B_ValueError, to$str("dict iterator source is not a dict")));
    return NULL;
}

static B_IteratorD_dict B_dict_base_key_iterator($WORD src, B_dict_base dict) {
    B_IteratorD_dict iter = acton_malloc(sizeof(struct B_IteratorD_dict));
    iter->$class = &B_IteratorD_dictG_methods;
    iter->src = src;
    iter->data = dict;
    iter->nxt = 0;
    return iter;
}

static bool B_IteratorD_dictD_next(B_IteratorD_dict self, $WORD *out) {
    if (!self->data->table)
        return false;
    int i = self->nxt;
    $table table = self->data->table;
    int n = table->tb_nentries;
    while (i < n) {
        $entry_t entry = &TB_ENTRIES(table)[i];
        if (entry->value != DELETED) {
            self->nxt = i + 1;
            *out = entry->key;
            return true;
        }
        i++;
    }
    return false;
}

B_IteratorD_dict B_IteratorD_dictG_new(B_dict dict) {
    return $NEW(B_IteratorD_dict, dict);
}
 
void B_IteratorD_dictD_init(B_IteratorD_dict self, B_dict dict) {
    self->src = dict;
    self->data = (B_dict_base)dict;
    self->nxt = 0;
}


bool B_IteratorD_dictD_bool(B_IteratorD_dict self) {
    return true;
}

B_str B_IteratorD_dictD_str(B_IteratorD_dict self) {
    return $FORMAT("<dict keys iterator object at %p>", self);
}

void B_IteratorD_dictD_serialize(B_IteratorD_dict self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(toB_int(self->nxt),state);
}


B_IteratorD_dict B_IteratorD_dict__deserialize(B_IteratorD_dict res, $Serial$state state) {
    if (!res)
        res = $DNEW( B_IteratorD_dict,state);
    res->src = $step_deserialize(state);
    res->data = B_dict_base_from_iter_src(res->src);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}


struct B_IteratorD_dictG_class B_IteratorD_dictG_methods = {"B_IteratorD_dict",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_dictD_init,
                                                      B_IteratorD_dictD_serialize, B_IteratorD_dict__deserialize, B_IteratorD_dictD_bool,B_IteratorD_dictD_str,
                                                      B_IteratorD_dictD_str, B_IteratorD_dictD_next};


B_Iterator B_MappingD_dictD___iter__ (B_MappingD_dict wit, B_dict dict) {
    return (B_Iterator)B_dict_base_key_iterator(dict, (B_dict_base)dict);
}

B_dict B_MappingD_dictD___fromiter__ (B_MappingD_dict wit, B_Iterable wit2, $WORD iter) {
    return B_dictG_new(wit->W_HashableD_AD_MappingD_dict, wit2, iter);
}

static int64_t B_dict_base_len(B_dict_base dict) {
    return dict->numelements;
}

static bool B_dict_base_contains(B_dict_base dict, B_Hashable hashwit, $WORD key) {
    if (dict->numelements == 0)
        return false;
    $WORD res;
    long h = 0;
    if (dict->table->tb_size > INIT_SIZE)
        h = B_hash(hashwit, key);
    return $lookdict(dict, hashwit, h, key, &res) >= 0;
}

static $WORD B_dict_base_get(B_dict_base dict, B_Hashable hashwit, $WORD key, $WORD deflt) {
    if (!dict->table)
        return deflt;
    uint64_t hash = 0;
    if (dict->table->tb_size > INIT_SIZE)
        hash = B_hash(hashwit, key);
    $WORD res;
    int ix = $lookdict(dict, hashwit, hash, key, &res);
    if (ix < 0)
        return deflt;
    return res;
}

static $WORD B_dict_base_getitem(B_dict_base dict, B_Hashable hashwit, $WORD key) {
    if (dict->numelements == 0)
        $RAISE((B_BaseException)$NEW(B_KeyError, key, to$str("getitem: empty dictionary")));
    uint64_t hash = 0;
    if (dict->table->tb_size > INIT_SIZE)
        hash = B_hash(hashwit, key);
    $WORD res;
    int ix = $lookdict(dict, hashwit, hash, key, &res);
    if (ix < 0)
        $RAISE((B_BaseException)$NEW(B_KeyError, key, to$str("getitem: key not in dictionary")));
    return res;
}

static $WORD B_dict_base_pop(B_dict_base dict, B_Hashable hashwit, $WORD key, $WORD deflt) {
    if (dict->numelements == 0)
        return deflt;
    $table table = dict->table;
    uint64_t hash = 0;
    if (table->tb_size > INIT_SIZE)
        hash = B_hash(hashwit, key);
    $WORD res;
    int ix = $lookdict(dict, hashwit, hash, key, &res);
    if (ix < 0)
        return deflt;
    $entry_t entry = &TB_ENTRIES(table)[ix];
    int i = $lookdict_index(table, hash, ix);
    table->tb_indices[i] = DKIX_DUMMY;
    res = entry->value;
    entry->value = DELETED;
    dict->numelements--;
    if (10 * dict->numelements < dict->table->tb_size)
        dictresize(hashwit, dict);
    return res;
}

int64_t B_MappingD_dictD___len__(B_MappingD_dict wit, B_dict dict) {
    return B_dict_base_len((B_dict_base)dict);
}

bool B_MappingD_dictD___contains__(B_MappingD_dict wit, B_dict dict, $WORD key) {
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    return B_dict_base_contains((B_dict_base)dict, hashwit, key);
}

bool B_MappingD_dictD___containsnot__(B_MappingD_dict wit, B_dict dict, $WORD key) {
    return !B_MappingD_dictD___contains__(wit, dict, key);
}

$WORD B_MappingD_dictD_get(B_MappingD_dict wit, B_dict dict, $WORD key) {
    return B_dict_base_get((B_dict_base)dict, wit->W_HashableD_AD_MappingD_dict, key, B_None);
}

$WORD B_MappingD_dictD_get_def(B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt) {
    return B_dict_base_get((B_dict_base)dict, wit->W_HashableD_AD_MappingD_dict, key, deflt);
}

$WORD B_MappingD_dictD_pop(B_MappingD_dict wit, B_dict dict, $WORD key) {
    return B_dict_base_pop((B_dict_base)dict, wit->W_HashableD_AD_MappingD_dict, key, B_None);
}

$WORD B_MappingD_dictD_pop_def(B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt) {
    return B_dict_base_pop((B_dict_base)dict, wit->W_HashableD_AD_MappingD_dict, key, deflt);
}

B_Iterator B_MappingD_dictD_keys (B_MappingD_dict wit, B_dict dict) {
    return (B_Iterator)B_dict_base_key_iterator(dict, (B_dict_base)dict);
}

// Iterator classes for values and items

// values iterator
static bool B_IteratorD_dict_values_next(B_IteratorD_dict_values self, $WORD *out) {
    int i = self->nxt;
    $table table = self->data->table;
    if (!table)
        return false;
    int n = table->tb_nentries;
    while (i < n) {
        $entry_t entry = &TB_ENTRIES(table)[i];
        if (entry->value != DELETED) {
            self->nxt = i + 1;
            *out = entry->value;
            return true;
        }
        i++;
    }
    return false;
}
 
B_IteratorD_dict_values B_IteratorD_dict_valuesG_new(B_dict dict) {
    return $NEW(B_IteratorD_dict_values, dict);
}

static B_IteratorD_dict_values B_dict_base_values_iterator($WORD src, B_dict_base dict) {
    B_IteratorD_dict_values iter = acton_malloc(sizeof(struct B_IteratorD_dict_values));
    iter->$class = &B_IteratorD_dict_valuesG_methods;
    iter->src = src;
    iter->data = dict;
    iter->nxt = 0;
    return iter;
}
 
void B_IteratorD_dict_values_init(B_IteratorD_dict_values self, B_dict dict) {
    self->src = dict;
    self->data = (B_dict_base)dict;
    self->nxt = 0;
}


bool B_IteratorD_dict_values_bool(B_IteratorD_dict_values self) {
    return true;
}

B_str B_IteratorD_dict_values_str(B_IteratorD_dict_values self) {
    return $FORMAT("<dict values iterator object at %p>", self);
}

void B_IteratorD_dict_values_serialize(B_IteratorD_dict_values self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(toB_int(self->nxt),state);
}

B_IteratorD_dict_values B_IteratorD_dict_values_deserialize(B_IteratorD_dict_values res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_dict_values,state);
    res->src = $step_deserialize(state);
    res->data = B_dict_base_from_iter_src(res->src);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}

struct B_IteratorD_dict_valuesG_class B_IteratorD_dict_valuesG_methods = {"B_IteratorD_dict_values",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_dict_values_init,
                                                                    B_IteratorD_dict_values_serialize, B_IteratorD_dict_values_deserialize, B_IteratorD_dict_values_bool,
                                                                          B_IteratorD_dict_values_str,B_IteratorD_dict_values_str, B_IteratorD_dict_values_next};

// items iterator
static bool B_IteratorD_dict_items_next(B_IteratorD_dict_items self, $WORD *out) {
    int i = self->nxt;
    $table table = self->data->table;
    if (!table)
        return false;
    int n = table->tb_nentries;
    while (i < n) {
        $entry_t entry = &TB_ENTRIES(table)[i];
        if (entry->value != DELETED) {
            self->nxt = i + 1;
            *out = $NEWTUPLE(2, entry->key, entry->value);
            return true;
        }
        i++;
    }
    return false;
}
 
B_IteratorD_dict_items B_IteratorD_dict_itemsG_new(B_dict dict) {
    return $NEW(B_IteratorD_dict_items, dict);
}

static B_IteratorD_dict_items B_dict_base_items_iterator($WORD src, B_dict_base dict) {
    B_IteratorD_dict_items iter = acton_malloc(sizeof(struct B_IteratorD_dict_items));
    iter->$class = &B_IteratorD_dict_itemsG_methods;
    iter->src = src;
    iter->data = dict;
    iter->nxt = 0;
    return iter;
}
 
void B_IteratorD_dict_items_init(B_IteratorD_dict_items self, B_dict dict) {
    self->src = dict;
    self->data = (B_dict_base)dict;
    self->nxt = 0;
}


bool B_IteratorD_dict_items_bool(B_IteratorD_dict_items self) {
    return true;
}

B_str B_IteratorD_dict_items_str(B_IteratorD_dict_items self) {
    return $FORMAT("<dict items iterator object at %p>", self);
}

void B_IteratorD_dict_items_serialize(B_IteratorD_dict_items self, $Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(toB_int(self->nxt),state);
}

B_IteratorD_dict_items B_IteratorD_dict_items_deserialize(B_IteratorD_dict_items res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_dict_items,state);
    res->src = $step_deserialize(state);
    res->data = B_dict_base_from_iter_src(res->src);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}



struct B_IteratorD_dict_itemsG_class B_IteratorD_dict_itemsG_methods = {"B_IteratorD_dict_items",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorD_dict_items_init,
                                                                  B_IteratorD_dict_items_serialize, B_IteratorD_dict_items_deserialize,B_IteratorD_dict_items_bool, B_IteratorD_dict_items_str,
                                                                  B_IteratorD_dict_items_str, B_IteratorD_dict_items_next};


B_Iterator B_MappingD_dictD_values (B_MappingD_dict wit, B_dict dict) {
    return (B_Iterator)B_dict_base_values_iterator(dict, (B_dict_base)dict);
}

B_Iterator B_MappingD_dictD_items (B_MappingD_dict wit, B_dict dict) {
    return (B_Iterator)B_dict_base_items_iterator(dict, (B_dict_base)dict);
}

// IMapping[idict] wrappers ///////////////////////////////////////////////////////////////////////

B_Iterator B_IMappingD_idictD___iter__(B_IMappingD_idict wit, B_idict dict) {
    return (B_Iterator)B_dict_base_key_iterator(dict, (B_dict_base)dict);
}

B_idict B_IMappingD_idictD___fromiter__(B_IMappingD_idict wit, B_Iterable wit2, $WORD iter) {
    return B_idictG_new(wit->W_HashableD_AD_IMappingD_idict, wit2, iter);
}

int64_t B_IMappingD_idictD___len__(B_IMappingD_idict wit, B_idict dict) {
    return B_dict_base_len((B_dict_base)dict);
}

bool B_IMappingD_idictD___contains__(B_IMappingD_idict wit, B_idict dict, $WORD key) {
    return B_dict_base_contains((B_dict_base)dict,
                                wit->W_HashableD_AD_IMappingD_idict, key);
}

bool B_IMappingD_idictD___containsnot__(B_IMappingD_idict wit, B_idict dict, $WORD key) {
    return !B_IMappingD_idictD___contains__(wit, dict, key);
}

$WORD B_IMappingD_idictD_get(B_IMappingD_idict wit, B_idict dict, $WORD key) {
    return B_dict_base_get((B_dict_base)dict, wit->W_HashableD_AD_IMappingD_idict,
                           key, B_None);
}

$WORD B_IMappingD_idictD_get_def(B_IMappingD_idict wit, B_idict dict,
                                 $WORD key, $WORD deflt) {
    return B_dict_base_get((B_dict_base)dict, wit->W_HashableD_AD_IMappingD_idict,
                           key, deflt);
}

B_Iterator B_IMappingD_idictD_keys(B_IMappingD_idict wit, B_idict dict) {
    return (B_Iterator)B_dict_base_key_iterator(dict, (B_dict_base)dict);
}

B_Iterator B_IMappingD_idictD_values(B_IMappingD_idict wit, B_idict dict) {
    return (B_Iterator)B_dict_base_values_iterator(dict, (B_dict_base)dict);
}

B_Iterator B_IMappingD_idictD_items(B_IMappingD_idict wit, B_idict dict) {
    return (B_Iterator)B_dict_base_items_iterator(dict, (B_dict_base)dict);
}

B_NoneType B_MappingD_dictD_update (B_MappingD_dict wit, B_dict dict, B_Iterable wit2, $WORD other) {
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    B_Iterator it = wit2->$class->__iter__(wit2,other);
    $WORD next;
    while (it->$class->__next__(it, &next)) {
        B_tuple item = (B_tuple)next;
        B_dictD_setitem(dict,hashwit,item->components[0],item->components[1]);
    }
    return B_None;
}

B_tuple B_MappingD_dictD_popitem (B_MappingD_dict wit, B_dict dict) {
    if (dict->numelements == 0)  {
        return NULL;
    }
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    $table table = dict->table;
    int ix = table->tb_nentries-1;
    while (ix >= 0) {
        $entry_t entry =  &TB_ENTRIES(table)[ix];
        if (entry->value != DELETED) {
            if (table->tb_size > INIT_SIZE) {
                uint64_t hash = B_hash(hashwit, entry->key);
                int i = $lookdict_index(table,hash,ix);
                table->tb_indices[i] = DKIX_DUMMY;
            }
            dict->numelements--;
            table->tb_nentries = ix;
            return $NEWTUPLE(2,entry->key,entry->value);
        }
        ix--;
    }
    return NULL;
}

$WORD B_MappingD_dictD_setdefault (B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt) {
    if (!deflt) deflt = B_None;
    B_Hashable hashwit = wit->W_HashableD_AD_MappingD_dict;
    uint64_t hash = B_hash(hashwit, key);
    $WORD value;
    int ix = $lookdict((B_dict_base)dict, hashwit, hash, key, &value);
    if (ix >= 0)
        return value;
    insertdict((B_dict_base)dict, hashwit, hash, key, deflt);
    return deflt;
}
 

// B_IndexedD_MappingD_dict ///////////////////////////////////////////////////////////////////////

$WORD B_IndexedD_MappingD_dictD___getitem__(B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key) {
    B_Hashable hashwit = ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingD_dict;
    return B_dict_base_getitem((B_dict_base)dict, hashwit, key);
}

$WORD B_IIndexedD_MappingD_dictD___getitem__(B_IIndexedD_MappingD_dict wit,
                                             B_dict dict, $WORD key) {
    B_Hashable hashwit = ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingD_dict;
    return B_dict_base_getitem((B_dict_base)dict, hashwit, key);
}

B_NoneType B_IndexedD_MappingD_dictD___setitem__ (B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key, $WORD value) {
    B_Hashable hashwit = ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingD_dict;
    uint64_t hash = 0;
    if (dict->table && dict->table->tb_size > INIT_SIZE) {
        hash = B_hash(hashwit, key);
    }
    insertdict((B_dict_base)dict, hashwit, hash, key, value);
    return B_None;
}

B_NoneType B_IndexedD_MappingD_dictD___delitem__(B_IndexedD_MappingD_dict wit,
                                                 B_dict dict, $WORD key) {
    B_Hashable hashwit = ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingD_dict;
    B_dict_base_pop((B_dict_base)dict, hashwit, key, B_None);
    return B_None;
}

// IIndexed[IMapping[idict]] //////////////////////////////////////////////////////////////////////

$WORD B_IIndexedD_IMappingD_idictD___getitem__(B_IIndexedD_IMappingD_idict wit,
                                               B_idict dict, $WORD key) {
    B_Hashable hashwit = ((B_IMappingD_idict)wit->W_IMapping)
        ->W_HashableD_AD_IMappingD_idict;
    return B_dict_base_getitem((B_dict_base)dict, hashwit, key);
}

void B_dictD_setitem(B_dict dict, B_Hashable hashwit, $WORD key, $WORD value) {
    uint64_t hash = 0;
    if (dict->table && dict->table->tb_size > INIT_SIZE) {
        hash = B_hash(hashwit, key);
    }
    insertdict((B_dict_base)dict, hashwit, hash, key, value);
}

$WORD B_dictD_get(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt) {
    return B_dict_base_get((B_dict_base)dict, hashwit, key, deflt);
}

$WORD B_dictD_pop(B_dict dict, B_Hashable hashwit, $WORD key, $WORD deflt) {
    return B_dict_base_pop((B_dict_base)dict, hashwit, key, deflt);
}

/*
B_dict B_dictD_copy(B_dict dict, B_Hashable hashwit) {
    B_Iterable w = (B_Iterable)B_MappingD_dictG_witness;
    return B_dictG_new(hashwit, w, dict);
}

B_NoneType B_dictD_clear(B_dict dict, B_Hashable hashwit) {
    $table table = NULL;
    dict->numelements = 0;
    return B_None;
}
*/
