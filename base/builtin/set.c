/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer
 *    in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define PERTURB_SHIFT 5
#define MIN_SIZE UINT64_C(8)

static $WORD _dummy;
#define dummy (&_dummy)
#define ACTIVE_ENTRY(e) ((e)->key != NULL && (e)->key != dummy)

static uint64_t B_set_next_probe(uint64_t index, uint64_t *perturb, uint64_t mask) {
    *perturb >>= PERTURB_SHIFT;
    return (index * UINT64_C(5) + UINT64_C(1) + *perturb) & mask;
}

static void B_set_table_init_empty(B_set_table *set) {
    set->numelements = 0;
    set->fill = 0;
    set->mask = MIN_SIZE - 1;
    set->finger = 0;
    set->table = acton_malloc(MIN_SIZE * sizeof(B_setentry));
    memset(set->table, 0, MIN_SIZE * sizeof(B_setentry));
}

static void B_set_insert_clean(B_setentry *table, uint64_t mask, $WORD key, uint64_t hash) {
    uint64_t perturb = hash;
    uint64_t i = hash & mask;
    while (true) {
        B_setentry *entry = &table[i];
        if (entry->key == NULL) {
            entry->key = key;
            entry->hash = hash;
            return;
        }
        i = B_set_next_probe(i, &perturb, mask);
    }
}

static void B_set_table_resize(B_set_table *set, uint64_t minsize) {
    B_setentry *oldtable = set->table;
    uint64_t oldmask = set->mask;
    uint64_t newsize = MIN_SIZE;

    while (newsize <= minsize) {
        if (newsize > UINT64_MAX / UINT64_C(2))
            $RAISE((B_BaseException)$NEW(B_MemoryError, to$str("set table is too large")));
        newsize <<= 1;
    }
    if (newsize > SIZE_MAX / sizeof(B_setentry))
        $RAISE((B_BaseException)$NEW(B_MemoryError, to$str("set table is too large")));

    size_t table_size = (size_t)newsize * sizeof(B_setentry);
    B_setentry *newtable = acton_malloc(table_size);
    if (newtable == NULL)
        $RAISE((B_BaseException)$NEW(B_MemoryError, to$str("memory allocation failed")));

    memset(newtable, 0, table_size);
    set->mask = newsize - 1;
    set->table = newtable;

    if (set->fill == set->numelements) {
        for (B_setentry *entry = oldtable; entry <= oldtable + oldmask; entry++) {
            if (entry->key != NULL)
                B_set_insert_clean(newtable, set->mask, entry->key, entry->hash);
        }
    } else {
        set->fill = set->numelements;
        for (B_setentry *entry = oldtable; entry <= oldtable + oldmask; entry++) {
            if (ACTIVE_ENTRY(entry))
                B_set_insert_clean(newtable, set->mask, entry->key, entry->hash);
        }
    }

    acton_free(oldtable);
}

static B_setentry *B_set_table_lookkey(B_set_table *set, B_Hashable hashwit, $WORD key, uint64_t hash) {
    uint64_t perturb = hash;
    uint64_t mask = set->mask;
    uint64_t i = hash & mask;
    B_setentry *entry = &set->table[i];

    if (entry->key == NULL)
        return entry;

    while (true) {
        $WORD startkey = entry->key;
        if (startkey != dummy && entry->hash == hash) {
            if (startkey == key || hashwit->$class->__eq__(hashwit, startkey, key))
                return entry;
        }
        i = B_set_next_probe(i, &perturb, mask);
        entry = &set->table[i];
        if (entry->key == NULL)
            return entry;
    }
}

static bool B_set_table_contains_hash(B_set_table *set, B_Hashable hashwit, $WORD elem, uint64_t hash) {
    B_setentry *entry = B_set_table_lookkey(set, hashwit, elem, hash);
    return ACTIVE_ENTRY(entry);
}

static bool B_set_table_contains(B_set_table *set, B_Hashable hashwit, $WORD elem) {
    return B_set_table_contains_hash(set, hashwit, elem, B_hash(hashwit, elem));
}

static void B_set_table_add_hash(B_set_table *set, B_Hashable hashwit, $WORD key, uint64_t hash) {
    B_setentry *freeslot = NULL;
    uint64_t perturb = hash;
    uint64_t mask = set->mask;
    uint64_t i = hash & mask;
    B_setentry *entry = &set->table[i];

    if (entry->key == NULL)
        goto found_unused;

    while (true) {
        $WORD startkey = entry->key;
        if (startkey == dummy) {
            if (freeslot == NULL)
                freeslot = entry;
        } else if (entry->hash == hash) {
            if (startkey == key || hashwit->$class->__eq__(hashwit, startkey, key))
                return;
        }

        i = B_set_next_probe(i, &perturb, mask);
        entry = &set->table[i];
        if (entry->key == NULL)
            goto found_unused_or_dummy;
    }

found_unused_or_dummy:
    if (freeslot != NULL) {
        set->numelements++;
        freeslot->key = key;
        freeslot->hash = hash;
        return;
    }

found_unused:
    set->fill++;
    set->numelements++;
    entry->key = key;
    entry->hash = hash;
    if (set->fill * UINT64_C(5) < mask * UINT64_C(3))
        return;

    uint64_t growth = set->numelements > UINT64_C(50000) ? UINT64_C(2) : UINT64_C(4);
    if (set->numelements > UINT64_MAX / growth)
        $RAISE((B_BaseException)$NEW(B_MemoryError, to$str("set table is too large")));
    B_set_table_resize(set, set->numelements * growth);
}

static void B_set_table_add(B_set_table *set, B_Hashable hashwit, $WORD key) {
    B_set_table_add_hash(set, hashwit, key, B_hash(hashwit, key));
}

static bool B_set_table_discard_hash(B_set_table *set, B_Hashable hashwit, $WORD elem, uint64_t hash) {
    B_setentry *entry = B_set_table_lookkey(set, hashwit, elem, hash);
    if (ACTIVE_ENTRY(entry)) {
        entry->key = dummy;
        entry->hash = 0;
        set->numelements--;
        return true;
    }
    return false;
}

static void B_set_table_copy_into(B_set_table *dst, B_set_table *src) {
    memcpy(dst, src, sizeof(B_set_table));
    dst->table = acton_malloc((src->mask + 1) * sizeof(B_setentry));
    memcpy(dst->table, src->table, (src->mask + 1) * sizeof(B_setentry));
}

static B_NoneType B_set_table_init_from_iterable(B_set_table *set, B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    B_set_table_init_empty(set);
    if (wit && iterable) {
        B_Iterator it = wit->$class->__iter__(wit, iterable);
        if ($PUSH()) {
            while (true) {
                $WORD nxt = it->$class->__next__(it);
                B_set_table_add(set, hashwit, nxt);
            }
            $DROP();
        } else {
            B_BaseException ex = $POP();
            if (! $ISINSTANCE0(ex, B_StopIteration))
                $RAISE(ex);
        }
    }
    return B_None;
}

static B_str B_set_table_str(B_set_table *set) {
    B_list parts = B_listD_new(set->numelements);
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    for (uint64_t i = 0; i <= set->mask; i++) {
        B_setentry *entry = &set->table[i];
        if (ACTIVE_ENTRY(entry)) {
            B_value elem = (B_value)entry->key;
            wit->$class->append(wit, parts, elem->$class->__repr__(elem));
        }
    }
    return B_strD_join_par('{', parts, '}');
}

static void B_set_table_serialize_payload(B_set_table *set, int class_id, $Serial$state state) {
    $ROW row = $add_header(class_id, 4, state);
    row->blob[0] = ($WORD)set->numelements;
    row->blob[1] = ($WORD)set->fill;
    row->blob[2] = ($WORD)set->mask;
    row->blob[3] = ($WORD)set->finger;

    for (uint64_t i = 0; i <= set->mask; i++) {
        B_setentry *entry = &set->table[i];
        if (entry->key == dummy) {
            $step_serialize(toB_u64(UINT64_MAX), state);
            $step_serialize(NULL, state);
        } else {
            $step_serialize(toB_u64(entry->hash), state);
            $step_serialize(entry->key, state);
        }
    }
}

static void B_set_table_deserialize_payload(B_set_table *set, $ROW row, $Serial$state state) {
    set->numelements = (uint64_t)(uintptr_t)row->blob[0];
    set->fill = (uint64_t)(uintptr_t)row->blob[1];
    set->mask = (uint64_t)(uintptr_t)row->blob[2];
    set->finger = (uint64_t)(uintptr_t)row->blob[3];
    set->table = acton_malloc((set->mask + 1) * sizeof(B_setentry));
    memset(set->table, 0, (set->mask + 1) * sizeof(B_setentry));
    for (uint64_t i = 0; i <= set->mask; i++) {
        B_setentry *entry = &set->table[i];
        entry->hash = fromB_u64((B_u64)$step_deserialize(state));
        entry->key = $step_deserialize(state);
        if (entry->key == NULL && entry->hash == UINT64_MAX) {
            entry->key = dummy;
            entry->hash = 0;
        }
    }
}

static bool B_set_table_isdisjoint(B_set_table *set, B_set_table *other, B_Hashable hashwit) {
    if (set == other)
        return set->numelements == 0;
    if (other->numelements > set->numelements)
        return B_set_table_isdisjoint(other, set, hashwit);
    for (uint64_t i = 0; i <= other->mask; i++) {
        B_setentry *entry = &other->table[i];
        if (ACTIVE_ENTRY(entry) && B_set_table_contains_hash(set, hashwit, entry->key, entry->hash))
            return false;
    }
    return true;
}

static bool B_set_table_eq(B_set_table *set, B_set_table *other, B_Hashable hashwit) {
    if (set == other)
        return true;
    if (set->numelements != other->numelements)
        return false;
    for (uint64_t i = 0; i <= other->mask; i++) {
        B_setentry *entry = &other->table[i];
        if (ACTIVE_ENTRY(entry) && !B_set_table_contains_hash(set, hashwit, entry->key, entry->hash))
            return false;
    }
    return true;
}

static bool B_set_table_gt(B_set_table *set, B_set_table *other, B_Hashable hashwit) {
    if (set == other || set->numelements <= other->numelements)
        return false;
    for (uint64_t i = 0; i <= other->mask; i++) {
        B_setentry *entry = &other->table[i];
        if (ACTIVE_ENTRY(entry) && !B_set_table_contains_hash(set, hashwit, entry->key, entry->hash))
            return false;
    }
    return true;
}

static bool B_set_table_ge(B_set_table *set, B_set_table *other, B_Hashable hashwit) {
    if (set == other)
        return true;
    if (set->numelements < other->numelements)
        return false;
    for (uint64_t i = 0; i <= other->mask; i++) {
        B_setentry *entry = &other->table[i];
        if (ACTIVE_ENTRY(entry) && !B_set_table_contains_hash(set, hashwit, entry->key, entry->hash))
            return false;
    }
    return true;
}

static void B_set_table_sub_into(B_set_table *res, B_set_table *set, B_set_table *other, B_Hashable hashwit) {
    B_set_table_copy_into(res, set);
    for (uint64_t i = 0; i <= other->mask; i++) {
        B_setentry *entry = &other->table[i];
        if (ACTIVE_ENTRY(entry))
            B_set_table_discard_hash(res, hashwit, entry->key, entry->hash);
    }
}

static void B_set_table_and_into(B_set_table *res, B_set_table *set, B_set_table *other, B_Hashable hashwit) {
    if (other->numelements > set->numelements) {
        B_set_table_and_into(res, other, set, hashwit);
        return;
    }
    B_set_table_init_empty(res);
    for (uint64_t i = 0; i <= set->mask; i++) {
        B_setentry *entry = &set->table[i];
        if (ACTIVE_ENTRY(entry) && B_set_table_contains_hash(other, hashwit, entry->key, entry->hash))
            B_set_table_add_hash(res, hashwit, entry->key, entry->hash);
    }
}

static void B_set_table_or_into(B_set_table *res, B_set_table *set, B_set_table *other, B_Hashable hashwit) {
    if (other->numelements > set->numelements) {
        B_set_table_or_into(res, other, set, hashwit);
        return;
    }
    B_set_table_copy_into(res, set);
    for (uint64_t i = 0; i <= other->mask; i++) {
        B_setentry *entry = &other->table[i];
        if (ACTIVE_ENTRY(entry))
            B_set_table_add_hash(res, hashwit, entry->key, entry->hash);
    }
}

static void B_set_table_xor_into(B_set_table *res, B_set_table *set, B_set_table *other, B_Hashable hashwit) {
    B_set_table_copy_into(res, set);
    for (uint64_t i = 0; i <= other->mask; i++) {
        B_setentry *entry = &other->table[i];
        if (ACTIVE_ENTRY(entry) && !B_set_table_discard_hash(res, hashwit, entry->key, entry->hash))
            B_set_table_add_hash(res, hashwit, entry->key, entry->hash);
    }
}

static $WORD B_set_table_pop(B_set_table *set) {
    if (set->numelements == 0)
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("pop from an empty set")));

    B_setentry *entry = set->table + (set->finger & set->mask);
    B_setentry *limit = set->table + set->mask;

    while (entry->key == NULL || entry->key == dummy) {
        entry++;
        if (entry > limit)
            entry = set->table;
    }
    $WORD res = entry->key;
    entry->key = dummy;
    entry->hash = 0;
    set->numelements--;
    set->finger = (uint64_t)(entry - set->table) + UINT64_C(1);
    return res;
}

static B_set B_set_from_table(B_set_table *table) {
    B_set res = acton_malloc(sizeof(struct B_set));
    res->$class = &B_setG_methods;
    B_set_table_copy_into(&res->data, table);
    return res;
}

static B_iset B_iset_from_table(B_set_table *table) {
    B_iset res = acton_malloc(sizeof(struct B_iset));
    res->$class = &B_isetG_methods;
    B_set_table_copy_into(&res->data, table);
    return res;
}

static void B_set_hash_table(B_set_table *set, B_Hashable hashwit, B_hasher h) {
    uint64_t sum = UINT64_C(0x1927868237a12d3b);
    uint64_t xors = UINT64_C(0x9e3779b97f4a7c15) ^ set->numelements;
    for (uint64_t i = 0; i <= set->mask; i++) {
        B_setentry *entry = &set->table[i];
        if (ACTIVE_ENTRY(entry)) {
            B_hasher hi = B_hasherG_new(NULL);
            hashwit->$class->hash(hashwit, entry->key, hi);
            uint64_t d = B_hasherD_finalize(hi);
            sum += d;
            xors ^= d + UINT64_C(0x9e3779b97f4a7c15);
        }
    }
    uint64_t parts[3] = { set->numelements, sum, xors };
    zig_hash_wyhash_update(h->_hasher, to$bytesD_len((char *)parts, sizeof(parts)));
}

// set object methods ///////////////////////////////////////////////////////////////////////////////

B_set B_setG_new(B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return $NEW(B_set, hashwit, wit, iterable);
}

B_NoneType B_setD___init__(B_set set, B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return B_set_table_init_from_iterable(&set->data, hashwit, wit, iterable);
}

bool B_setD___bool__(B_set self) {
    return self->data.numelements > 0;
}

B_str B_setD___str__(B_set self) {
    return B_set_table_str(&self->data);
}

B_str B_setD___repr__(B_set self) {
    return B_setD___str__(self);
}

void B_setD___serialize__(B_set self, $Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done, (B_Hashable)B_HashableD_WORDG_witness, self, NULL);
    if (prevkey) {
        int64_t pk = fromB_int(prevkey);
        $val_serialize(-SET_ID, &pk, state);
        return;
    }
    B_dictD_setitem(state->done, (B_Hashable)B_HashableD_WORDG_witness, self, toB_int(state->row_no));
    B_set_table_serialize_payload(&self->data, SET_ID, state);
}

B_set B_setD___deserialize__(B_set res, $Serial$state state) {
    $ROW row = state->row;
    state->row = row->next;
    state->row_no++;
    if (row->class_id < 0)
        return B_dictD_get(state->done, (B_Hashable)B_HashableD_intG_witness, toB_int((int64_t)(intptr_t)row->blob[0]), NULL);

    if (!res)
        res = acton_malloc(sizeof(struct B_set));
    B_dictD_setitem(state->done, (B_Hashable)B_HashableD_intG_witness, toB_int(state->row_no - 1), res);
    res->$class = &B_setG_methods;
    B_set_table_deserialize_payload(&res->data, row, state);
    return res;
}

void B_set_add_entry(B_set set, B_Hashable hashwit, $WORD key, uint64_t hash) {
    B_set_table_add_hash(&set->data, hashwit, key, hash);
}

B_set B_set_copy(B_set set, B_Hashable hashwit) {
    (void)hashwit;
    return B_set_from_table(&set->data);
}

// iset object methods /////////////////////////////////////////////////////////////////////////////

B_iset B_isetG_new(B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return $NEW(B_iset, hashwit, wit, iterable);
}

B_NoneType B_isetD___init__(B_iset set, B_Hashable hashwit, B_Iterable wit, $WORD iterable) {
    return B_set_table_init_from_iterable(&set->data, hashwit, wit, iterable);
}

bool B_isetD___bool__(B_iset self) {
    return self->data.numelements > 0;
}

B_str B_isetD___str__(B_iset self) {
    return B_TimesD_strD___add__ (NULL, to$str("i"),B_set_table_str(&self->data));
}

B_str B_isetD___repr__(B_iset self) {
    return B_isetD___str__(self);
}

void B_isetD___serialize__(B_iset self, $Serial$state state) {
    B_set_table_serialize_payload(&self->data, ISET_ID, state);
}

B_iset B_isetD___deserialize__(B_iset res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_iset, state);
    $ROW row = state->row;
    state->row = row->next;
    state->row_no++;
    res->$class = &B_isetG_methods;
    B_set_table_deserialize_payload(&res->data, row, state);
    return res;
}

// Iterators ////////////////////////////////////////////////////////////////////////////////////////

static B_set_table *B_set_table_from_iter_src($WORD src) {
    if (src == NULL)
        return NULL;
    $SuperG_class cls = (($Super)src)->$class;
    if (cls == ($SuperG_class)&B_setG_methods)
        return &((B_set)src)->data;
    if (cls == ($SuperG_class)&B_isetG_methods)
        return &((B_iset)src)->data;
    $RAISE((B_BaseException)$NEW(B_ValueError, to$str("set iterator source is not a set")));
    return NULL;
}

static $WORD B_IteratorD_set_next_entry(B_IteratorD_set self) {
    B_set_table *set = self->data;
    uint64_t i = self->nxt;
    while (i <= set->mask) {
        B_setentry *entry = &set->table[i];
        if (ACTIVE_ENTRY(entry)) {
            self->nxt = i + 1;
            return entry;
        }
        i++;
    }
    $RAISE((B_BaseException)$NEW(B_StopIteration, to$str("set iterator terminated")));
    return NULL;
}

static B_Iterator B_set_iter_table($WORD src, B_set_table *set) {
    B_IteratorD_set iter = acton_malloc(sizeof(struct B_IteratorD_set));
    iter->$class = &B_IteratorD_setG_methods;
    iter->src = src;
    iter->data = set;
    iter->nxt = 0;
    return (B_Iterator)iter;
}

static $WORD B_IteratorD_set_next(B_IteratorD_set self) {
    $WORD res = B_IteratorD_set_next_entry(self);
    return ((B_setentry *)res)->key;
}

B_IteratorD_set B_IteratorD_setG_new(B_set s) {
    return $NEW(B_IteratorD_set, s);
}

void B_IteratorD_set_init(B_IteratorD_set self, B_set set) {
    self->src = set;
    self->data = &set->data;
    self->nxt = 0;
}

bool B_IteratorD_set_bool(B_IteratorD_set self) {
    return true;
}

B_str B_IteratorD_set_str(B_IteratorD_set self) {
    return $FORMAT("<set keys iterator object at %p>", self);
}

void B_IteratorD_set_serialize(B_IteratorD_set self, $Serial$state state) {
    $step_serialize(self->src, state);
    $step_serialize(toB_u64(self->nxt), state);
}

B_IteratorD_set B_IteratorD_setD__deserialize(B_IteratorD_set res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorD_set, state);
    res->src = $step_deserialize(state);
    res->data = B_set_table_from_iter_src(res->src);
    res->nxt = fromB_u64((B_u64)$step_deserialize(state));
    return res;
}

struct B_IteratorD_setG_class B_IteratorD_setG_methods = {"B_IteratorD_set", UNASSIGNED, ($SuperG_class)&B_IteratorG_methods,
                                                          B_IteratorD_set_init, B_IteratorD_set_serialize,
                                                          B_IteratorD_setD__deserialize, B_IteratorD_set_bool,
                                                          B_IteratorD_set_str, B_IteratorD_set_str, B_IteratorD_set_next};

// Set[set] wrappers ////////////////////////////////////////////////////////////////////////////////

B_Iterator B_SetD_setD___iter__(B_SetD_set wit, B_set set) {
    return B_set_iter_table(set, &set->data);
}

B_set B_SetD_setD___fromiter__(B_SetD_set wit, B_Iterable wit2, $WORD iter) {
    return B_setG_new(wit->W_HashableD_AD_SetD_set, wit2, iter);
}

int64_t B_SetD_setD___len__(B_SetD_set wit, B_set set) {
    return (int64_t)set->data.numelements;
}

bool B_SetD_setD___contains__(B_SetD_set wit, B_set set, $WORD val) {
    return B_set_table_contains(&set->data, wit->W_HashableD_AD_SetD_set, val);
}

bool B_SetD_setD___containsnot__(B_SetD_set wit, B_set set, $WORD val) {
    return !B_SetD_setD___contains__(wit, set, val);
}

bool B_SetD_setD_isdisjoint(B_SetD_set wit, B_set set, B_set other) {
    return B_set_table_isdisjoint(&set->data, &other->data, wit->W_HashableD_AD_SetD_set);
}

B_NoneType B_SetD_setD_add(B_SetD_set wit, B_set set, $WORD elem) {
    B_set_table_add(&set->data, wit->W_HashableD_AD_SetD_set, elem);
    return B_None;
}

B_NoneType B_SetD_setD_update(B_SetD_set wit, B_set set, B_Iterable otherwit, $WORD other) {
    B_Hashable hashwit = wit->W_HashableD_AD_SetD_set;
    if (set == other)
        return B_None;
    B_Iterator it = otherwit->$class->__iter__(otherwit, other);
    if ($PUSH()) {
        while (true) {
            $WORD elem = it->$class->__next__(it);
            B_set_table_add(&set->data, hashwit, elem);
        }
        $DROP();
    } else {
        B_BaseException ex = $POP();
        if (! $ISINSTANCE0(ex, B_StopIteration))
            $RAISE(ex);
    }
    return B_None;
}

B_NoneType B_SetD_setD_discard(B_SetD_set wit, B_set set, $WORD elem) {
    B_Hashable hashwit = wit->W_HashableD_AD_SetD_set;
    B_set_table_discard_hash(&set->data, hashwit, elem, B_hash(hashwit, elem));
    return B_None;
}

$WORD B_SetD_setD_pop(B_SetD_set wit, B_set set) {
    return B_set_table_pop(&set->data);
}

bool B_OrdD_SetD_setD___eq__(B_OrdD_SetD_set wit, B_set set, B_set other) {
    return B_set_table_eq(&set->data, &other->data, ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set);
}

bool B_OrdD_SetD_setD___ne__(B_OrdD_SetD_set wit, B_set set, B_set other) {
    return !B_OrdD_SetD_setD___eq__(wit, set, other);
}

bool B_OrdD_SetD_setD___gt__(B_OrdD_SetD_set wit, B_set set, B_set other) {
    return B_set_table_gt(&set->data, &other->data, ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set);
}

bool B_OrdD_SetD_setD___ge__(B_OrdD_SetD_set wit, B_set set, B_set other) {
    return B_set_table_ge(&set->data, &other->data, ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set);
}

bool B_OrdD_SetD_setD___lt__(B_OrdD_SetD_set wit, B_set set, B_set other) {
    return B_OrdD_SetD_setD___gt__(wit, other, set);
}

bool B_OrdD_SetD_setD___le__(B_OrdD_SetD_set wit, B_set set, B_set other) {
    return B_OrdD_SetD_setD___ge__(wit, other, set);
}

B_set B_MinusD_SetD_setD___sub__(B_MinusD_SetD_set wit, B_set set, B_set other) {
    B_set_table tmp;
    B_set_table_sub_into(&tmp, &set->data, &other->data, ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set);
    B_set res = B_set_from_table(&tmp);
    acton_free(tmp.table);
    return res;
}

B_set B_LogicalD_SetD_setD___and__(B_LogicalD_SetD_set wit, B_set set, B_set other) {
    B_set_table tmp;
    B_set_table_and_into(&tmp, &set->data, &other->data, ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set);
    B_set res = B_set_from_table(&tmp);
    acton_free(tmp.table);
    return res;
}

B_set B_LogicalD_SetD_setD___or__(B_LogicalD_SetD_set wit, B_set set, B_set other) {
    B_set_table tmp;
    B_set_table_or_into(&tmp, &set->data, &other->data, ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set);
    B_set res = B_set_from_table(&tmp);
    acton_free(tmp.table);
    return res;
}

B_set B_LogicalD_SetD_setD___xor__(B_LogicalD_SetD_set wit, B_set set, B_set other) {
    B_set_table tmp;
    B_set_table_xor_into(&tmp, &set->data, &other->data, ((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetD_set);
    B_set res = B_set_from_table(&tmp);
    acton_free(tmp.table);
    return res;
}

// ISet[iset] wrappers //////////////////////////////////////////////////////////////////////////////

B_Iterator B_ISetD_isetD___iter__(B_ISetD_iset wit, B_iset set) {
    return B_set_iter_table(set, &set->data);
}

B_iset B_ISetD_isetD___fromiter__(B_ISetD_iset wit, B_Iterable wit2, $WORD iter) {
    return B_isetG_new(wit->W_HashableD_AD_ISetD_iset, wit2, iter);
}

int64_t B_ISetD_isetD___len__(B_ISetD_iset wit, B_iset set) {
    return (int64_t)set->data.numelements;
}

bool B_ISetD_isetD___contains__(B_ISetD_iset wit, B_iset set, $WORD val) {
    return B_set_table_contains(&set->data, wit->W_HashableD_AD_ISetD_iset, val);
}

bool B_ISetD_isetD___containsnot__(B_ISetD_iset wit, B_iset set, $WORD val) {
    return !B_ISetD_isetD___contains__(wit, set, val);
}

bool B_ISetD_isetD_isdisjoint(B_ISetD_iset wit, B_iset set, B_iset other) {
    return B_set_table_isdisjoint(&set->data, &other->data, wit->W_HashableD_AD_ISetD_iset);
}

bool B_OrdD_ISetD_isetD___eq__(B_OrdD_ISetD_iset wit, B_iset set, B_iset other) {
    return B_set_table_eq(&set->data, &other->data, ((B_ISetD_iset)wit->W_ISet)->W_HashableD_AD_ISetD_iset);
}

bool B_OrdD_ISetD_isetD___ne__(B_OrdD_ISetD_iset wit, B_iset set, B_iset other) {
    return !B_OrdD_ISetD_isetD___eq__(wit, set, other);
}

bool B_OrdD_ISetD_isetD___gt__(B_OrdD_ISetD_iset wit, B_iset set, B_iset other) {
    return B_set_table_gt(&set->data, &other->data, ((B_ISetD_iset)wit->W_ISet)->W_HashableD_AD_ISetD_iset);
}

bool B_OrdD_ISetD_isetD___ge__(B_OrdD_ISetD_iset wit, B_iset set, B_iset other) {
    return B_set_table_ge(&set->data, &other->data, ((B_ISetD_iset)wit->W_ISet)->W_HashableD_AD_ISetD_iset);
}

bool B_OrdD_ISetD_isetD___lt__(B_OrdD_ISetD_iset wit, B_iset set, B_iset other) {
    return B_OrdD_ISetD_isetD___gt__(wit, other, set);
}

bool B_OrdD_ISetD_isetD___le__(B_OrdD_ISetD_iset wit, B_iset set, B_iset other) {
    return B_OrdD_ISetD_isetD___ge__(wit, other, set);
}

B_iset B_MinusD_ISetD_isetD___sub__(B_MinusD_ISetD_iset wit, B_iset set, B_iset other) {
    B_set_table tmp;
    B_set_table_sub_into(&tmp, &set->data, &other->data, ((B_ISetD_iset)wit->W_ISet)->W_HashableD_AD_ISetD_iset);
    B_iset res = B_iset_from_table(&tmp);
    acton_free(tmp.table);
    return res;
}

B_iset B_LogicalD_ISetD_isetD___and__(B_LogicalD_ISetD_iset wit, B_iset set, B_iset other) {
    B_set_table tmp;
    B_set_table_and_into(&tmp, &set->data, &other->data, ((B_ISetD_iset)wit->W_ISet)->W_HashableD_AD_ISetD_iset);
    B_iset res = B_iset_from_table(&tmp);
    acton_free(tmp.table);
    return res;
}

B_iset B_LogicalD_ISetD_isetD___or__(B_LogicalD_ISetD_iset wit, B_iset set, B_iset other) {
    B_set_table tmp;
    B_set_table_or_into(&tmp, &set->data, &other->data, ((B_ISetD_iset)wit->W_ISet)->W_HashableD_AD_ISetD_iset);
    B_iset res = B_iset_from_table(&tmp);
    acton_free(tmp.table);
    return res;
}

B_iset B_LogicalD_ISetD_isetD___xor__(B_LogicalD_ISetD_iset wit, B_iset set, B_iset other) {
    B_set_table tmp;
    B_set_table_xor_into(&tmp, &set->data, &other->data, ((B_ISetD_iset)wit->W_ISet)->W_HashableD_AD_ISetD_iset);
    B_iset res = B_iset_from_table(&tmp);
    acton_free(tmp.table);
    return res;
}

B_NoneType B_HashableD_isetD_hash(B_HashableD_iset wit, B_iset set, B_hasher h) {
    B_set_hash_table(&set->data, wit->W_HashableD_AD_HashableD_iset, h);
    return B_None;
}

#undef ACTIVE_ENTRY
#undef dummy
