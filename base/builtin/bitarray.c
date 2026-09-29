static uint64_t B_bitarray_word_count(int64_t length) {
    return ((uint64_t)length + 63) / 64;
}

static uint64_t B_bitarray_tail_mask(int64_t length) {
    unsigned used = (unsigned)((uint64_t)length & 63);
    return used == 0 ? UINT64_MAX : (UINT64_C(1) << used) - 1;
}

static void B_bitarray_init_storage(B_bitarray self, int64_t length, bool initial) {
    if (length < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("bitarray length must be non-negative")));

    uint64_t word_count = B_bitarray_word_count(length);
    if (word_count > SIZE_MAX / sizeof(uint64_t))
        $RAISE((B_BaseException)$NEW(B_MemoryError,
                                     to$str("bitarray is too large")));

    self->length = length;
    self->count = initial ? length : 0;
    if (word_count == 0) {
        self->data = NULL;
        return;
    }

    size_t nbytes = (size_t)word_count * sizeof(uint64_t);
    self->data = acton_malloc_atomic(nbytes);
    memset(self->data, initial ? 0xff : 0, nbytes);
    if (initial)
        self->data[word_count - 1] &= B_bitarray_tail_mask(length);
}

static int64_t B_bitarray_checked_index(B_bitarray self, int64_t index) {
    if (index < 0 || index >= self->length)
        $RAISE((B_BaseException)$NEW(B_IndexError, index,
                                     to$str("bitarray index out of range")));
    return index;
}

B_bitarray B_bitarrayG_new(int64_t length, B_bool initial) {
    B_bitarray self = acton_malloc(sizeof(struct B_bitarray));
    self->$class = &B_bitarrayG_methods;
    B_bitarray_init_storage(self, length, initial && initial->val);
    return self;
}

B_NoneType B_bitarrayD___init__(B_bitarray self, int64_t length, B_bool initial) {
    B_bitarray_init_storage(self, length, initial && initial->val);
    return B_None;
}

bool B_bitarrayD___bool__(B_bitarray self) {
    return self->length != 0;
}

B_str B_bitarrayD___str__(B_bitarray self) {
    return B_objectD___str__((B_object)self);
}

B_str B_bitarrayD___repr__(B_bitarray self) {
    return B_bitarrayD___str__(self);
}

bool $bitarrayD_U__getitem__(B_bitarray self, int64_t index) {
    index = B_bitarray_checked_index(self, index);
    uint64_t word = self->data[(uint64_t)index >> 6];
    return (word >> ((uint64_t)index & 63)) & UINT64_C(1);
}

B_NoneType $bitarrayD_U__setitem__(B_bitarray self, int64_t index, bool value) {
    index = B_bitarray_checked_index(self, index);
    uint64_t *word = &self->data[(uint64_t)index >> 6];
    uint64_t mask = UINT64_C(1) << ((uint64_t)index & 63);
    bool old = (*word & mask) != 0;
    if (value)
        *word |= mask;
    else
        *word &= ~mask;
    if (old != value)
        self->count += value ? 1 : -1;
    return B_None;
}

int64_t $bitarrayD_U__len(B_bitarray self) {
    return self->length;
}

int64_t B_bitarrayD___len__(B_bitarray self) {
    return self->length;
}

bool B_bitarrayD___getitem__(B_bitarray self, int64_t index) {
    return $bitarrayD_U__getitem__(self, index);
}

B_NoneType B_bitarrayD___setitem__(B_bitarray self, int64_t index, bool value) {
    return $bitarrayD_U__setitem__(self, index, value);
}

// MutIndexed uses the ordinary boxed protocol ABI. Direct bitarray indexing
// continues to use the raw bool workers above.
B_bool B_MutIndexedD_bitarrayD___getitem__(B_MutIndexedD_bitarray wit,
                                           B_bitarray self, B_int index) {
    return toB_bool($bitarrayD_U__getitem__(self, index->val));
}

B_NoneType B_MutIndexedD_bitarrayD___setitem__(B_MutIndexedD_bitarray wit,
                                               B_bitarray self, B_int index,
                                               B_bool value) {
    return $bitarrayD_U__setitem__(self, index->val, value->val);
}

// Container[bool] /////////////////////////////////////////////////////////////////////////////////

static bool B_IteratorD_bitarrayD_next(B_IteratorD_bitarray self, $WORD *out) {
    if (self->next >= self->src->length)
        return false;
    *out = (B_value)toB_bool($bitarrayD_U__getitem__(self->src, self->next++));
    return true;
}

B_IteratorD_bitarray B_IteratorD_bitarrayG_new(B_bitarray src) {
    return $NEW(B_IteratorD_bitarray, src);
}

void B_IteratorD_bitarrayD_init(B_IteratorD_bitarray self, B_bitarray src) {
    self->src = src;
    self->next = 0;
}

bool B_IteratorD_bitarrayD_bool(B_IteratorD_bitarray self) {
    return true;
}

B_str B_IteratorD_bitarrayD_str(B_IteratorD_bitarray self) {
    return $FORMAT("<bitarray iterator object at %p>", self);
}

void B_IteratorD_bitarrayD_serialize(B_IteratorD_bitarray self, $Serial$state state) {
    $step_serialize(self->src, state);
    $step_serialize(toB_int(self->next), state);
}

B_IteratorD_bitarray B_IteratorD_bitarrayD_deserialize(B_IteratorD_bitarray self,
                                                        $Serial$state state) {
    if (!self)
        self = $DNEW(B_IteratorD_bitarray, state);
    self->src = (B_bitarray)$step_deserialize(state);
    self->next = fromB_int((B_int)$step_deserialize(state));
    return self;
}

struct B_IteratorD_bitarrayG_class B_IteratorD_bitarrayG_methods = {
    "B_IteratorD_bitarray", UNASSIGNED, ($SuperG_class)&B_IteratorG_methods,
    B_IteratorD_bitarrayD_init, B_IteratorD_bitarrayD_serialize,
    B_IteratorD_bitarrayD_deserialize, B_IteratorD_bitarrayD_bool,
    B_IteratorD_bitarrayD_str, B_IteratorD_bitarrayD_str,
    B_IteratorD_bitarrayD_next
};

B_Iterator B_ContainerD_bitarrayD___iter__(B_ContainerD_bitarray wit,
                                            B_bitarray self) {
    return (B_Iterator)B_IteratorD_bitarrayG_new(self);
}

B_bitarray B_ContainerD_bitarrayD___fromiter__(B_ContainerD_bitarray wit,
                                               B_Iterable iter_wit, $WORD iterable) {
    B_list values = B_listG_new(iter_wit, iterable);
    B_bitarray result = B_bitarrayG_new(values->length, B_False);
    for (int64_t i = 0; i < values->length; i++) {
        if (((B_bool)values->data[i])->val)
            $bitarrayD_U__setitem__(result, i, true);
    }
    return result;
}

int64_t B_ContainerD_bitarrayD___len__(B_ContainerD_bitarray wit,
                                       B_bitarray self) {
    return self->length;
}

bool B_ContainerD_bitarrayD___contains__(B_ContainerD_bitarray wit,
                                         B_bitarray self, B_bool value) {
    return value->val ? self->count > 0 : self->count < self->length;
}

bool B_ContainerD_bitarrayD___containsnot__(B_ContainerD_bitarray wit,
                                            B_bitarray self, B_bool value) {
    return !B_ContainerD_bitarrayD___contains__(wit, self, value);
}

void B_bitarrayD___serialize__(B_bitarray self, $Serial$state state) {
    uint64_t word_count = B_bitarray_word_count(self->length);
    if (word_count > INT_MAX - 1)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("bitarray is too large to serialize")));

    // BITARRAY_ID is above ITEM_ID, so the generic serializer has already
    // emitted the object header and installed self in its back-reference table.
    $ROW row = $add_header(BITARRAY_ID, (int)word_count + 1, state);
    row->blob[0] = ($WORD)(intptr_t)self->length;
    if (word_count > 0)
        memcpy(&row->blob[1], self->data, (size_t)word_count * sizeof(uint64_t));
}

B_bitarray B_bitarrayD___deserialize__(B_bitarray self, $Serial$state state) {
    // The generic deserializer consumed the object header before dispatching
    // here.  Register the object before consuming its packed payload.
    if (!self) {
        self = acton_malloc(sizeof(struct B_bitarray));
        self->$class = &B_bitarrayG_methods;
        B_dictD_setitem(state->done, (B_Hashable)B_HashableD_intG_witness,
                        toB_int(state->row_no - 1), self);
    }

    $ROW row = state->row;
    if (!row || row->class_id != BITARRAY_ID || row->blob_size < 1)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("invalid serialized bitarray")));
    state->row = row->next;
    state->row_no++;

    int64_t length = (int64_t)(intptr_t)row->blob[0];
    if (length < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("invalid serialized bitarray")));
    uint64_t word_count = B_bitarray_word_count(length);
    if (word_count > INT_MAX - 1 || row->blob_size != (int)word_count + 1)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("invalid serialized bitarray")));

    self->$class = &B_bitarrayG_methods;
    B_bitarray_init_storage(self, length, false);
    if (word_count > 0) {
        memcpy(self->data, &row->blob[1], (size_t)word_count * sizeof(uint64_t));
        self->data[word_count - 1] &= B_bitarray_tail_mask(length);
        self->count = 0;
        for (uint64_t i = 0; i < word_count; i++)
            self->count += __builtin_popcountll(self->data[i]);
    }
    return self;
}
