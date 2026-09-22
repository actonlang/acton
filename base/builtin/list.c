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

// Generic list implementation /////////////////////////////////////////////////////////////////////

// For now, expansion doubles capacity.
static void B_list_base_expand(B_list_base lst, int n) {
    if (lst->capacity >= lst->length + n)
        return;
    int newcapacity = lst->capacity == 0 ? 1 : lst->capacity;
    while (newcapacity < lst->length + n)
        newcapacity <<= 1;
    $WORD *newptr = lst->data == NULL
        ? acton_malloc(newcapacity * sizeof($WORD))
        : acton_realloc(lst->data, newcapacity * sizeof($WORD));
    if (newptr == NULL)
        $RAISE((B_BaseException)$NEW(B_MemoryError, to$str("memory allocation failed")));
    lst->data = newptr;
    lst->capacity = newcapacity;
}

static void B_list_base_shrink(B_list_base lst) {
    if (lst->capacity > 20 && 2 * lst->length < lst->capacity) {
        int newcapacity = lst->length;
        $WORD *old = lst->data;
        lst->data = acton_malloc(newcapacity * sizeof($WORD));
        lst->capacity = newcapacity;
        assert(old != NULL);
        memcpy(lst->data, old, newcapacity * sizeof($WORD));
    }
}

static B_list_base B_list_base_new(int capacity, $SuperG_class cls) {
    if (capacity < 0) {
        fprintf(stderr, "Internal error list_new: negative capacity");
        exit(-1);
    }
    B_list_base lst = acton_malloc(sizeof(struct B_list_base));
    if (lst == NULL)
        $RAISE((B_BaseException)$NEW(B_MemoryError, to$str("memory allocation failed")));
    lst->data = capacity > 0 ? acton_malloc(capacity * sizeof($WORD)) : NULL;
    if (capacity > 0 && lst->data == NULL)
        $RAISE((B_BaseException)$NEW(B_MemoryError, to$str("memory allocation failed")));
    lst->length = 0;
    lst->capacity = capacity;
    lst->$class = cls;
    return lst;
}

static B_NoneType B_list_base_append(B_list_base lst, $WORD elem) {
    B_list_base_expand(lst, 1);
    lst->data[lst->length++] = elem;
    return B_None;
}

static B_NoneType B_list_base_init(B_list_base lst, B_Iterable wit, $WORD iterable) {
    lst->length = 0;
    lst->capacity = 0;
    lst->data = NULL;
    if (!iterable || !wit)
        return B_None;

    B_Iterator it = wit->$class->__iter__(wit, iterable);
    $WORD e;
    while (it->$class->__next__(it, &e))
        B_list_base_append(lst, e);
    return B_None;
}

static bool B_list_base_bool(B_list_base self) {
    return self->length > 0;
}

static B_str B_list_base_str(B_list_base self) {
    B_list parts = B_listD_new(self->length);
    for (int i = 0; i < self->length; i++) {
        B_value elem = (B_value)self->data[i];
        parts->data[parts->length++] = elem == B_None ? to$str("None") : elem->$class->__repr__(elem);
    }
    return B_strD_join_par('[', parts, ']');
}

static void B_list_base_serialize(B_list_base self, int class_id, $Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done, (B_Hashable)B_HashableD_WORDG_witness, self, NULL);
    if (prevkey) {
        long pk = fromB_int(prevkey);
        $val_serialize(-class_id, &pk, state);
        return;
    }
    B_dictD_setitem(state->done, (B_Hashable)B_HashableD_WORDG_witness, self, toB_int(state->row_no));
    long len = (long)self->length;
    $val_serialize(class_id, &len, state);
    for (int i = 0; i < self->length; i++)
        $step_serialize(self->data[i], state);
}

static B_list_base B_list_base_deserialize(B_list_base res, $SuperG_class cls, $Serial$state state) {
    $ROW row = state->row;
    state->row = row->next;
    state->row_no++;
    if (row->class_id < 0)
        return (B_list_base)B_dictD_get(state->done, (B_Hashable)B_HashableD_intG_witness,
                                       toB_int((long)row->blob[0]), NULL);

    int len = (int)(long)row->blob[0];
    if (!res)
        res = B_list_base_new(len, cls);
    else
        res->$class = cls;
    B_dictD_setitem(state->done, (B_Hashable)B_HashableD_intG_witness,
                    toB_int(state->row_no - 1), res);
    res->length = len;
    res->capacity = len;
    if (len > 0 && res->data == NULL)
        res->data = acton_malloc(len * sizeof($WORD));
    for (int i = 0; i < len; i++)
        res->data[i] = $step_deserialize(state);
    return res;
}

static B_list_base B_list_base_copy_as(B_list_base lst, $SuperG_class cls) {
    B_list_base res = B_list_base_new(lst->length, cls);
    res->length = lst->length;
    if (lst->length > 0)
        memcpy(res->data, lst->data, lst->length * sizeof($WORD));
    return res;
}

static B_NoneType B_list_base_clear(B_list_base lst) {
    lst->data = NULL;
    lst->capacity = 0;
    lst->length = 0;
    return B_None;
}

static B_NoneType B_list_base_extend(B_list_base lst, B_list_base other) {
    if (other->length == 0)
        return B_None;
    int otherlen = other->length;
    B_list_base_expand(lst, otherlen);
    memcpy(lst->data + lst->length, other->data, otherlen * sizeof($WORD));
    lst->length += otherlen;
    return B_None;
}

static $WORD B_list_base_pop(B_list_base lst, B_int i) {
    int len = lst->length;
    long ix = i ? fromB_int(i) : len - 1;
    long ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len)
        $RAISE((B_BaseException)$NEW(B_IndexError, ix0, to$str("pop: index outside list")));
    $WORD res = lst->data[ix0];
    memmove(lst->data + ix0, lst->data + ix0 + 1, (len - ix0 - 1) * sizeof($WORD));
    lst->data[len - 1] = NULL;
    lst->length--;
    B_list_base_shrink(lst);
    return res;
}

static int64_t B_list_base_index(B_list_base self, B_Eq eqwit, $WORD val, B_int start, B_int stop) {
    int strt = start ? fromB_int(start) : 0;
    if (strt < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("start position must be >= 0")));
    if (strt > self->length)
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("start position must not exceed list length")));
    int stp = self->length;
    if (stop) {
        stp = fromB_int(stop);
        if (stp <= strt)
            $RAISE((B_BaseException)$NEW(B_ValueError, to$str("stop position must be higher than start position")));
    }
    if (stp > self->length)
        stp = self->length;
    for (int i = strt; i < stp; i++) {
        if (eqwit->$class->__eq__(eqwit, val, self->data[i]))
            return i;
    }
    $RAISE((B_BaseException)$NEW(B_KeyError, val, to$str("element is not in list")));
    return 0;
}

static int64_t B_list_base_count(B_list_base self, B_Eq eqwit, $WORD val) {
    int64_t count = 0;
    for (int i = 0; i < self->length; i++) {
        if (eqwit->$class->__eq__(eqwit, val, self->data[i]))
            count++;
    }
    return count;
}

static bool B_list_base_eq(B_list_base a, B_list_base b, B_Eq eqwit) {
    if (a == b)
        return true;
    if (a->length != b->length)
        return false;
    for (int i = 0; i < a->length; i++) {
        if (eqwit->$class->__ne__(eqwit, a->data[i], b->data[i]))
            return false;
    }
    return true;
}

static bool B_list_base_lt(B_list_base a, B_list_base b, B_Ord ordwit) {
    int minlen = a->length < b->length ? a->length : b->length;
    int i = 0;
    while (i < minlen && ordwit->$class->__eq__(ordwit, a->data[i], b->data[i]))
        i++;
    if (i == a->length)
        return i < b->length;
    if (i == b->length)
        return false;
    return ordwit->$class->__lt__(ordwit, a->data[i], b->data[i]);
}

static B_list_base B_list_base_add(B_list_base lst, B_list_base other, $SuperG_class cls) {
    int reslen = lst->length + other->length;
    B_list_base res = B_list_base_new(reslen, cls);
    if (lst->length > 0)
        memcpy(res->data, lst->data, lst->length * sizeof($WORD));
    if (other->length > 0)
        memcpy(res->data + lst->length, other->data, other->length * sizeof($WORD));
    res->length = reslen;
    return res;
}

static B_list_base B_list_base_mul(B_list_base lst, B_int n, $SuperG_class cls) {
    int64_t count = n->val;
    if (lst->length == 0 || count <= 0)
        return B_list_base_new(0, cls);
    B_list_base res = B_list_base_new(lst->length * count, cls);
    for (int64_t i = 0; i < count; i++)
        memcpy(res->data + i * lst->length, lst->data, lst->length * sizeof($WORD));
    res->length = lst->length * count;
    return res;
}

static $WORD B_list_base_getitem(B_list_base lst, int64_t n) {
    int len = lst->length;
    int64_t ix0 = n < 0 ? len + n : n;
    if (ix0 < 0 || ix0 >= len)
        $RAISE((B_BaseException)$NEW(B_IndexError, ix0, to$str("getitem: index outside list")));
    return lst->data[ix0];
}

static B_NoneType B_list_base_setitem(B_list_base lst, int64_t n, $WORD val) {
    int len = lst->length;
    int64_t ix0 = n < 0 ? len + n : n;
    if (ix0 < 0 || ix0 >= len)
        $RAISE((B_BaseException)$NEW(B_IndexError, ix0, to$str("setitem: index outside list")));
    lst->data[ix0] = val;
    return B_None;
}

static B_NoneType B_list_base_delitem(B_list_base lst, int64_t n) {
    int len = lst->length;
    int64_t ix0 = n < 0 ? len + n : n;
    if (ix0 < 0 || ix0 >= len)
        return B_None;
    memmove(lst->data + ix0, lst->data + ix0 + 1, (len - ix0 - 1) * sizeof($WORD));
    lst->data[len - 1] = NULL;
    lst->length--;
    B_list_base_shrink(lst);
    return B_None;
}

static B_list_base B_list_base_getslice(B_list_base lst, B_slice slc, $SuperG_class cls) {
    int64_t start, stop, step, slen;
    normalize_slice(slc, lst->length, &slen, &start, &stop, &step);
    B_list_base res = B_list_base_new(slen, cls);
    int64_t pos = start;
    for (int64_t i = 0; i < slen; i++) {
        B_list_base_append(res, lst->data[pos]);
        pos += step;
    }
    return res;
}

static B_NoneType B_list_base_setslice(B_list_base lst, B_Iterable wit, B_slice slc, $WORD iter) {
    B_list_base other = B_list_base_new(0, ($SuperG_class)&B_listG_methods);
    B_list_base_init(other, wit, iter);

    int64_t start, stop, step, slen;
    normalize_slice(slc, lst->length, &slen, &start, &stop, &step);
    if (step != 1 && other->length != slen)
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str("setslice: illegal slice")));

    int copy = other->length <= slen ? other->length : slen;
    int pos = start;
    for (int i = 0; i < copy; i++) {
        lst->data[pos] = other->data[i];
        pos += step;
    }
    if (other->length == slen)
        return B_None;

    // If the lengths differ, normalize_slice guarantees that step is one.
    if (other->length < slen) {
        memmove(lst->data + start + copy, lst->data + start + slen,
                (lst->length - start - slen) * sizeof($WORD));
        lst->length -= slen - other->length;
    } else {
        int increment = other->length - slen;
        B_list_base_expand(lst, increment);
        int rest = lst->length - start - copy;
        memmove(lst->data + start + copy + increment, lst->data + start + copy,
                rest * sizeof($WORD));
        for (int i = copy; i < other->length; i++)
            lst->data[start + i] = other->data[i];
        lst->length += increment;
    }
    return B_None;
}

static B_NoneType B_list_base_delslice(B_list_base lst, B_slice slc) {
    int64_t start, stop, step, slen;
    normalize_slice(slc, lst->length, &slen, &start, &stop, &step);
    if (slen == 0)
        return B_None;
    $WORD *p = lst->data + start;
    for (int64_t i = 0; i < slen - 1; i++) {
        memmove(p, p + i + 1, (step - 1) * sizeof($WORD));
        p += step - 1;
    }
    memmove(p, p + slen, (lst->length - 1 - (start + step * (slen - 1))) * sizeof($WORD));
    lst->length -= slen;
    return B_None;
}

static B_NoneType B_list_base_reverse(B_list_base lst) {
    for (int i = 0; i < lst->length / 2; i++) {
        $WORD tmp = lst->data[i];
        lst->data[i] = lst->data[lst->length - 1 - i];
        lst->data[lst->length - 1 - i] = tmp;
    }
    return B_None;
}

static B_NoneType B_list_base_insert(B_list_base lst, int64_t n, $WORD elem) {
    int len = lst->length;
    B_list_base_expand(lst, 1);
    int64_t ix0 = n < 0 ? (len + n < 0 ? 0 : len + n) : (n < len ? n : len);
    memmove(lst->data + ix0 + 1, lst->data + ix0, (len - ix0) * sizeof($WORD));
    lst->data[ix0] = elem;
    lst->length++;
    return B_None;
}

static bool B_list_base_contains(B_list_base lst, B_Eq eqwit, $WORD elem) {
    for (int i = 0; i < lst->length; i++) {
        if (eqwit->$class->__eq__(eqwit, elem, lst->data[i]))
            return true;
    }
    return false;
}

// list and ilist object methods ///////////////////////////////////////////////////////////////////

B_list B_listD_new(int capacity) {
    return (B_list)B_list_base_new(capacity, ($SuperG_class)&B_listG_methods);
}

B_ilist B_ilistD_new(int capacity) {
    return (B_ilist)B_list_base_new(capacity, ($SuperG_class)&B_ilistG_methods);
}

B_list B_listG_new(B_Iterable wit, $WORD iterable) {
    return $NEW(B_list, wit, iterable);
}

B_NoneType B_listD___init__(B_list self, B_Iterable wit, $WORD iterable) {
    return B_list_base_init((B_list_base)self, wit, iterable);
}

bool B_listD___bool__(B_list self) {
    return B_list_base_bool((B_list_base)self);
}

B_str B_listD___str__(B_list self) {
    return B_list_base_str((B_list_base)self);
}

B_str B_listD___repr__(B_list self) {
    return B_listD___str__(self);
}

void B_listD___serialize__(B_list self, $Serial$state state) {
    B_list_base_serialize((B_list_base)self, LIST_ID, state);
}

B_list B_listD___deserialize__(B_list self, $Serial$state state) {
    return (B_list)B_list_base_deserialize((B_list_base)self, ($SuperG_class)&B_listG_methods, state);
}

B_list B_listD_copy(B_list self) {
    return (B_list)B_list_base_copy_as((B_list_base)self, ($SuperG_class)&B_listG_methods);
}

B_NoneType B_listD_clear(B_list self) {
    return B_list_base_clear((B_list_base)self);
}

B_NoneType B_listD_extend(B_list self, B_list other) {
    return B_list_base_extend((B_list_base)self, (B_list_base)other);
}

$WORD B_listD_pop(B_list self, B_int i) {
    return B_list_base_pop((B_list_base)self, i);
}

int64_t B_listD_index(B_list self, B_Eq eqwit, $WORD val, B_int start, B_int stop) {
    return B_list_base_index((B_list_base)self, eqwit, val, start, stop);
}

int64_t B_listD_count(B_list self, B_Eq eqwit, $WORD val) {
    return B_list_base_count((B_list_base)self, eqwit, val);
}

// Freeze[list] ////////////////////////////////////////////////////////////////////////////////////

B_ilist B_FreezeD_listD_freeze(B_FreezeD_list wit, B_list self) {
    B_ilist frozen = (B_ilist)self;
    frozen->$class = &B_ilistG_methods;
    return frozen;
}

B_ilist B_ilistG_new(B_Iterable wit, $WORD iterable) {
    return $NEW(B_ilist, wit, iterable);
}

B_NoneType B_ilistD___init__(B_ilist self, B_Iterable wit, $WORD iterable) {
    return B_list_base_init((B_list_base)self, wit, iterable);
}

bool B_ilistD___bool__(B_ilist self) {
    return B_list_base_bool((B_list_base)self);
}

B_str B_ilistD___str__(B_ilist self) {
    B_str contents = B_list_base_str((B_list_base)self);
    return $FORMAT("ilist(%s)", contents->str);
}

B_str B_ilistD___repr__(B_ilist self) {
    return B_ilistD___str__(self);
}

void B_ilistD___serialize__(B_ilist self, $Serial$state state) {
    B_list_base_serialize((B_list_base)self, ILIST_ID, state);
}

B_ilist B_ilistD___deserialize__(B_ilist self, $Serial$state state) {
    return (B_ilist)B_list_base_deserialize((B_list_base)self, ($SuperG_class)&B_ilistG_methods, state);
}

// Iterators ////////////////////////////////////////////////////////////////////////////////////////

static B_list_base B_list_base_from_iter_src($WORD src) {
    if (src == NULL)
        return NULL;
    $SuperG_class cls = (($Super)src)->$class;
    if (cls == ($SuperG_class)&B_listG_methods || cls == ($SuperG_class)&B_ilistG_methods)
        return (B_list_base)src;
    $RAISE((B_BaseException)$NEW(B_ValueError, to$str("list iterator source is not a list")));
    return NULL;
}

static bool B_IteratorD_listD_next(B_IteratorD_list self, $WORD *out) {
    if (self->nxt >= self->data->length)
        return false;
    *out = self->data->data[self->nxt++];
    return true;
}

static B_Iterator B_list_base_iter($WORD src, B_list_base data) {
    B_IteratorD_list iter = acton_malloc(sizeof(struct B_IteratorD_list));
    iter->$class = &B_IteratorD_listG_methods;
    iter->src = src;
    iter->data = data;
    iter->nxt = 0;
    return (B_Iterator)iter;
}

B_IteratorD_list B_IteratorD_listG_new(B_list lst) {
    return $NEW(B_IteratorD_list, lst);
}

void B_IteratorD_listD_init(B_IteratorD_list self, B_list lst) {
    self->src = lst;
    self->data = (B_list_base)lst;
    self->nxt = 0;
}

bool B_IteratorD_listD_bool(B_IteratorD_list self) {
    return true;
}

B_str B_IteratorD_listD_str(B_IteratorD_list self) {
    return $FORMAT("<list iterator object at %p>", self);
}

void B_IteratorD_listD_serialize(B_IteratorD_list self, $Serial$state state) {
    $step_serialize(self->src, state);
    $step_serialize(toB_int(self->nxt), state);
}

B_IteratorD_list B_IteratorD_list$_deserialize(B_IteratorD_list self, $Serial$state state) {
    if (!self)
        self = $DNEW(B_IteratorD_list, state);
    self->src = $step_deserialize(state);
    self->data = B_list_base_from_iter_src(self->src);
    self->nxt = fromB_int((B_int)$step_deserialize(state));
    return self;
}

struct B_IteratorD_listG_class B_IteratorD_listG_methods = {
    "B_IteratorD_list", UNASSIGNED, ($SuperG_class)&B_IteratorG_methods,
    B_IteratorD_listD_init, B_IteratorD_listD_serialize, B_IteratorD_list$_deserialize,
    B_IteratorD_listD_bool, B_IteratorD_listD_str, B_IteratorD_listD_str, B_IteratorD_listD_next
};

// Unboxed helpers used by generated code //////////////////////////////////////////////////////////

$WORD $listD_U__getitem__(B_list lst, int64_t n) {
    return B_list_base_getitem((B_list_base)lst, n);
}

B_NoneType listD_U__setitem__(B_list lst, int64_t n, $WORD val) {
    return B_list_base_setitem((B_list_base)lst, n, val);
}

// Sequence[list] wrappers /////////////////////////////////////////////////////////////////////////

$WORD B_SequenceD_listD___getitem__(B_SequenceD_list wit, B_list lst, B_int n) {
    return B_list_base_getitem((B_list_base)lst, n->val);
}

B_list B_SequenceD_listD___getslice__(B_SequenceD_list wit, B_list lst, B_slice slc) {
    return (B_list)B_list_base_getslice((B_list_base)lst, slc, ($SuperG_class)&B_listG_methods);
}

B_Iterator B_SequenceD_listD___reversed__(B_SequenceD_list wit, B_list lst) {
    B_list_base copy = B_list_base_copy_as((B_list_base)lst, ($SuperG_class)&B_listG_methods);
    B_list_base_reverse(copy);
    return B_list_base_iter(copy, copy);
}

B_NoneType B_SequenceD_listD_insert(B_SequenceD_list wit, B_list lst, int64_t n, $WORD elem) {
    return B_list_base_insert((B_list_base)lst, n, elem);
}

B_NoneType B_SequenceD_listD_append(B_SequenceD_list wit, B_list lst, $WORD elem) {
    return B_list_base_append((B_list_base)lst, elem);
}

B_NoneType B_SequenceD_listD_reverse(B_SequenceD_list wit, B_list lst) {
    return B_list_base_reverse((B_list_base)lst);
}

$WORD B_SliceableD_SequenceD_listD___getitem__(B_SliceableD_SequenceD_list wit, B_list lst, B_int n) {
    return B_list_base_getitem((B_list_base)lst, n->val);
}

B_list B_SliceableD_SequenceD_listD___getslice__(B_SliceableD_SequenceD_list wit, B_list lst, B_slice slc) {
    return (B_list)B_list_base_getslice((B_list_base)lst, slc, ($SuperG_class)&B_listG_methods);
}

B_NoneType B_SliceableD_SequenceD_listD___setslice__(B_SliceableD_SequenceD_list wit, B_list lst,
                                                      B_Iterable iterwit, B_slice slc, $WORD iter) {
    return B_list_base_setslice((B_list_base)lst, iterwit, slc, iter);
}

B_NoneType B_SliceableD_SequenceD_listD___delslice__(B_SliceableD_SequenceD_list wit, B_list lst, B_slice slc) {
    return B_list_base_delslice((B_list_base)lst, slc);
}

$WORD B_IndexedD_SliceableD_SequenceD_listD___getitem__(B_IndexedD_SliceableD_SequenceD_list wit,
                                                        B_list lst, B_int n) {
    return B_list_base_getitem((B_list_base)lst, n->val);
}

B_NoneType B_IndexedD_SliceableD_SequenceD_listD___setitem__(B_IndexedD_SliceableD_SequenceD_list wit,
                                                             B_list lst, B_int n, $WORD val) {
    return B_list_base_setitem((B_list_base)lst, n->val, val);
}

B_NoneType B_IndexedD_SliceableD_SequenceD_listD___delitem__(B_IndexedD_SliceableD_SequenceD_list wit,
                                                             B_list lst, B_int n) {
    return B_list_base_delitem((B_list_base)lst, n->val);
}

B_Iterator B_CollectionD_SequenceD_listD___iter__(B_CollectionD_SequenceD_list wit, B_list lst) {
    return B_list_base_iter(lst, (B_list_base)lst);
}

B_list B_CollectionD_SequenceD_listD___fromiter__(B_CollectionD_SequenceD_list wit,
                                                  B_Iterable iterwit, $WORD iter) {
    return B_listG_new(iterwit, iter);
}

int64_t B_CollectionD_SequenceD_listD___len__(B_CollectionD_SequenceD_list wit, B_list lst) {
    return lst->length;
}

B_list B_TimesD_SequenceD_listD___add__(B_TimesD_SequenceD_list wit, B_list lst, B_list other) {
    return (B_list)B_list_base_add((B_list_base)lst, (B_list_base)other,
                                  ($SuperG_class)&B_listG_methods);
}

B_list B_TimesD_SequenceD_listD___zero__(B_TimesD_SequenceD_list wit) {
    return B_listD_new(0);
}

B_list B_TimesD_SequenceD_listD___mul__(B_TimesD_SequenceD_list wit, B_list lst, B_int n) {
    return (B_list)B_list_base_mul((B_list_base)lst, n, ($SuperG_class)&B_listG_methods);
}

bool B_ContainerD_listD___contains__(B_ContainerD_list wit, B_list lst, $WORD elem) {
    return B_list_base_contains((B_list_base)lst, wit->W_EqD_AD_ContainerD_list, elem);
}

bool B_ContainerD_listD___containsnot__(B_ContainerD_list wit, B_list lst, $WORD elem) {
    return !B_ContainerD_listD___contains__(wit, lst, elem);
}

bool B_EqD_listD___eq__(B_EqD_list wit, B_list a, B_list b) {
    return B_list_base_eq((B_list_base)a, (B_list_base)b, wit->W_EqD_AD_EqD_list);
}

bool B_OrdD_listD___lt__(B_OrdD_list wit, B_list a, B_list b) {
    return B_list_base_lt((B_list_base)a, (B_list_base)b, wit->W_OrdD_AD_OrdD_list);
}

// ISequence[ilist] wrappers ///////////////////////////////////////////////////////////////////////

$WORD B_ISequenceD_ilistD___getitem__(B_ISequenceD_ilist wit, B_ilist lst, B_int n) {
    return B_list_base_getitem((B_list_base)lst, n->val);
}

B_ilist B_ISequenceD_ilistD___getslice__(B_ISequenceD_ilist wit, B_ilist lst, B_slice slc) {
    return (B_ilist)B_list_base_getslice((B_list_base)lst, slc, ($SuperG_class)&B_ilistG_methods);
}

B_Iterator B_ISequenceD_ilistD___reversed__(B_ISequenceD_ilist wit, B_ilist lst) {
    B_list_base copy = B_list_base_copy_as((B_list_base)lst, ($SuperG_class)&B_ilistG_methods);
    B_list_base_reverse(copy);
    return B_list_base_iter(copy, copy);
}

B_Iterator B_CollectionD_ISequenceD_ilistD___iter__(B_CollectionD_ISequenceD_ilist wit, B_ilist lst) {
    return B_list_base_iter(lst, (B_list_base)lst);
}

B_ilist B_CollectionD_ISequenceD_ilistD___fromiter__(B_CollectionD_ISequenceD_ilist wit,
                                                      B_Iterable iterwit, $WORD iter) {
    return B_ilistG_new(iterwit, iter);
}

int64_t B_CollectionD_ISequenceD_ilistD___len__(B_CollectionD_ISequenceD_ilist wit, B_ilist lst) {
    return lst->length;
}

B_ilist B_TimesD_ISequenceD_ilistD___add__(B_TimesD_ISequenceD_ilist wit, B_ilist lst, B_ilist other) {
    return (B_ilist)B_list_base_add((B_list_base)lst, (B_list_base)other,
                                   ($SuperG_class)&B_ilistG_methods);
}

B_ilist B_TimesD_ISequenceD_ilistD___zero__(B_TimesD_ISequenceD_ilist wit) {
    return B_ilistD_new(0);
}

B_ilist B_TimesD_ISequenceD_ilistD___mul__(B_TimesD_ISequenceD_ilist wit, B_ilist lst, B_int n) {
    return (B_ilist)B_list_base_mul((B_list_base)lst, n, ($SuperG_class)&B_ilistG_methods);
}

bool B_ContainerD_ilistD___contains__(B_ContainerD_ilist wit, B_ilist lst, $WORD elem) {
    return B_list_base_contains((B_list_base)lst, wit->W_EqD_AD_ContainerD_ilist, elem);
}

bool B_ContainerD_ilistD___containsnot__(B_ContainerD_ilist wit, B_ilist lst, $WORD elem) {
    return !B_ContainerD_ilistD___contains__(wit, lst, elem);
}

bool B_EqD_ilistD___eq__(B_EqD_ilist wit, B_ilist a, B_ilist b) {
    return B_list_base_eq((B_list_base)a, (B_list_base)b, wit->W_EqD_AD_EqD_ilist);
}

bool B_OrdD_ilistD___lt__(B_OrdD_ilist wit, B_ilist a, B_ilist b) {
    return B_list_base_lt((B_list_base)a, (B_list_base)b, wit->W_OrdD_AD_OrdD_ilist);
}

// Witness needed while the method-table registry itself is being initialized. ////////////////////

struct B_SequenceD_listG_class B_SequenceD_listG_methods = {
    "B_SequenceD_list",
    UNASSIGNED,
    ($SuperG_class)&B_SequenceG_methods,
    NULL, // B_SequenceD_listD___init__
    NULL, // B_SequenceD_listD___serialize__
    NULL, // B_SequenceD_listD___deserialize__
    (bool (*)(B_SequenceD_list))$default__bool__,
    (B_str (*)(B_SequenceD_list))$default__str__,
    (B_str (*)(B_SequenceD_list))$default__str__,
    B_SequenceD_listD___getitem__,
    B_SequenceD_listD___getslice__,
    B_SequenceD_listD___reversed__,
    B_SequenceD_listD_insert,
    B_SequenceD_listD_append,
    B_SequenceD_listD_reverse
};
