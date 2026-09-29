static enum B_array_kind B_array_kind_from_witness(B_ArrayElement wit) {
    if ((void *)wit->$class == (void *)&B_ArrayElementD_intG_methods)
        return B_ARRAY_INT;
    if ((void *)wit->$class == (void *)&B_ArrayElementD_floatG_methods)
        return B_ARRAY_FLOAT;
    $RAISE((B_BaseException)$NEW(B_ValueError,
                                 to$str("array element type must be int or float")));
    return B_ARRAY_INT; // unreachable; keeps conservative C compilers happy
}

static void B_array_init_storage(B_array self, enum B_array_kind kind, int64_t length,
                                 $WORD initial) {
    if (length < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("array length must be non-negative")));
    if ((uint64_t)length > SIZE_MAX / sizeof(uint64_t))
        $RAISE((B_BaseException)$NEW(B_MemoryError,
                                     to$str("array is too large")));

    self->kind = kind;
    self->length = length;
    if (length == 0) {
        self->data = NULL;
        return;
    }

    size_t nbytes = (size_t)length * sizeof(uint64_t);
    self->data = acton_malloc_atomic(nbytes);
    if (initial == B_None) {
        memset(self->data, 0, nbytes);
    } else if (kind == B_ARRAY_INT) {
        int64_t value = fromB_int((B_int)initial);
        int64_t *data = self->data;
        for (int64_t i = 0; i < length; i++)
            data[i] = value;
    } else {
        double value = fromB_float((B_float)initial);
        double *data = self->data;
        for (int64_t i = 0; i < length; i++)
            data[i] = value;
    }
}

static int64_t B_array_checked_index(B_array self, int64_t index) {
    if (index < 0 || index >= self->length)
        $RAISE((B_BaseException)$NEW(B_IndexError, index,
                                     to$str("array index out of range")));
    return index;
}

B_array B_arrayG_new(B_ArrayElement wit, int64_t length, $WORD initial) {
    B_array self = acton_malloc(sizeof(struct B_array));
    self->$class = &B_arrayG_methods;
    B_array_init_storage(self, B_array_kind_from_witness(wit), length, initial);
    return self;
}

B_NoneType B_arrayD___init__(B_array self, B_ArrayElement wit, int64_t length,
                            $WORD initial) {
    B_array_init_storage(self, B_array_kind_from_witness(wit), length, initial);
    return B_None;
}

bool B_arrayD___bool__(B_array self) {
    return self->length != 0;
}

B_str B_arrayD___str__(B_array self) {
    if (self->length > INT_MAX)
        $RAISE((B_BaseException)$NEW(B_MemoryError,
                                     to$str("array is too large to represent")));

    B_list parts = B_listD_new((int)self->length);
    if (self->kind == B_ARRAY_INT) {
        int64_t *data = self->data;
        for (int64_t i = 0; i < self->length; i++)
            parts->data[parts->length++] = $FORMAT("%lld", data[i]);
    } else {
        double *data = self->data;
        for (int64_t i = 0; i < self->length; i++)
            parts->data[parts->length++] = $FORMAT("%g", data[i]);
    }

    B_str bracketed = B_strD_join_par('[', parts, ']');
    return $FORMAT("[|%.*s|]", bracketed->nbytes - 2, bracketed->str + 1);
}

B_str B_arrayD___repr__(B_array self) {
    return B_arrayD___str__(self);
}

int64_t $arrayD_U__getitem_int(B_array self, int64_t index) {
    index = B_array_checked_index(self, index);
    return ((int64_t *)self->data)[index];
}

double $arrayD_U__getitem_float(B_array self, int64_t index) {
    index = B_array_checked_index(self, index);
    return ((double *)self->data)[index];
}

B_NoneType $arrayD_U__setitem_int(B_array self, int64_t index, int64_t value) {
    index = B_array_checked_index(self, index);
    ((int64_t *)self->data)[index] = value;
    return B_None;
}

B_NoneType $arrayD_U__setitem_float(B_array self, int64_t index, double value) {
    index = B_array_checked_index(self, index);
    ((double *)self->data)[index] = value;
    return B_None;
}

int64_t $arrayD_U__len(B_array self) {
    return self->length;
}

int64_t B_arrayD___len__(B_array self) {
    return self->length;
}

$WORD B_arrayD___getitem__(B_array self, int64_t index) {
    if (self->kind == B_ARRAY_INT)
        return toB_int($arrayD_U__getitem_int(self, index));
    return toB_float($arrayD_U__getitem_float(self, index));
}

B_NoneType B_arrayD___setitem__(B_array self, int64_t index, $WORD value) {
    if (self->kind == B_ARRAY_INT)
        return $arrayD_U__setitem_int(self, index, fromB_int((B_int)value));
    return $arrayD_U__setitem_float(self, index, ((B_float)value)->val);
}

// Protocol adapters retain boxed values at the polymorphic boundary; concrete
// array indexing is lowered to the raw workers above by the compiler.
$WORD B_MutIndexedD_arrayD___getitem__(B_MutIndexedD_array wit, B_array self,
                                      B_int index) {
    return B_arrayD___getitem__(self, index->val);
}

B_NoneType B_MutIndexedD_arrayD___setitem__(B_MutIndexedD_array wit,
                                            B_array self, B_int index,
                                            $WORD value) {
    return B_arrayD___setitem__(self, index->val, value);
}

// Container[A] ////////////////////////////////////////////////////////////////////////////////////

static bool B_IteratorD_arrayD_next(B_IteratorD_array self, $WORD *out) {
    if (self->next >= self->src->length)
        return false;
    *out = B_arrayD___getitem__(self->src, self->next++);
    return true;
}

B_IteratorD_array B_IteratorD_arrayG_new(B_array src) {
    return $NEW(B_IteratorD_array, src);
}

void B_IteratorD_arrayD_init(B_IteratorD_array self, B_array src) {
    self->src = src;
    self->next = 0;
}

bool B_IteratorD_arrayD_bool(B_IteratorD_array self) {
    return true;
}

B_str B_IteratorD_arrayD_str(B_IteratorD_array self) {
    return $FORMAT("<array iterator object at %p>", self);
}

void B_IteratorD_arrayD_serialize(B_IteratorD_array self, $Serial$state state) {
    $step_serialize(self->src, state);
    $step_serialize(toB_int(self->next), state);
}

B_IteratorD_array B_IteratorD_arrayD_deserialize(B_IteratorD_array self,
                                                  $Serial$state state) {
    if (!self)
        self = $DNEW(B_IteratorD_array, state);
    self->src = (B_array)$step_deserialize(state);
    self->next = fromB_int((B_int)$step_deserialize(state));
    return self;
}

struct B_IteratorD_arrayG_class B_IteratorD_arrayG_methods = {
    "B_IteratorD_array", UNASSIGNED, ($SuperG_class)&B_IteratorG_methods,
    B_IteratorD_arrayD_init, B_IteratorD_arrayD_serialize,
    B_IteratorD_arrayD_deserialize, B_IteratorD_arrayD_bool,
    B_IteratorD_arrayD_str, B_IteratorD_arrayD_str,
    B_IteratorD_arrayD_next
};

B_Iterator B_ContainerD_arrayD___iter__(B_ContainerD_array wit, B_array self) {
    return (B_Iterator)B_IteratorD_arrayG_new(self);
}

B_array B_ContainerD_arrayD___fromiter__(B_ContainerD_array wit,
                                         B_Iterable iter_wit, $WORD iterable) {
    B_list values = B_listG_new(iter_wit, iterable);
    B_array result = B_arrayG_new(wit->W_ArrayElementD_AD_ContainerD_array,
                                  values->length, B_None);
    for (int64_t i = 0; i < values->length; i++)
        B_arrayD___setitem__(result, i, values->data[i]);
    return result;
}

int64_t B_ContainerD_arrayD___len__(B_ContainerD_array wit, B_array self) {
    return self->length;
}

bool B_ContainerD_arrayD___contains__(B_ContainerD_array wit, B_array self,
                                      $WORD value) {
    B_Eq eq_wit = wit->W_EqD_AD_ContainerD_array;
    for (int64_t i = 0; i < self->length; i++) {
        if (eq_wit->$class->__eq__(eq_wit, B_arrayD___getitem__(self, i), value))
            return true;
    }
    return false;
}

bool B_ContainerD_arrayD___containsnot__(B_ContainerD_array wit, B_array self,
                                         $WORD value) {
    return !B_ContainerD_arrayD___contains__(wit, self, value);
}

void B_arrayD___serialize__(B_array self, $Serial$state state) {
    if (self->length > INT_MAX - 2)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("array is too large to serialize")));

    // ARRAY_ID is above ITEM_ID, so the generic serializer has already emitted
    // the object header and installed self in its back-reference table.  This
    // row contains only array-specific payload, like iset/ilist/idict payloads.
    $ROW row = $add_header(ARRAY_ID, (int)self->length + 2, state);
    row->blob[0] = ($WORD)(intptr_t)self->kind;
    row->blob[1] = ($WORD)(intptr_t)self->length;
    if (self->length > 0)
        memcpy(&row->blob[2], self->data, (size_t)self->length * sizeof(uint64_t));
}

B_array B_arrayD___deserialize__(B_array self, $Serial$state state) {
    // The generic deserializer consumed the object header before dispatching
    // here.  Register the object at that row before consuming its payload.
    if (!self) {
        self = acton_malloc(sizeof(struct B_array));
        self->$class = &B_arrayG_methods;
        B_dictD_setitem(state->done, (B_Hashable)B_HashableD_intG_witness,
                        toB_int(state->row_no - 1), self);
    }

    $ROW row = state->row;
    if (!row || row->class_id != ARRAY_ID || row->blob_size < 2)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("invalid serialized array")));
    state->row = row->next;
    state->row_no++;

    enum B_array_kind kind = (enum B_array_kind)(intptr_t)row->blob[0];
    int64_t length = (int64_t)(intptr_t)row->blob[1];
    if ((kind != B_ARRAY_INT && kind != B_ARRAY_FLOAT) ||
        length < 0 || length > INT_MAX - 2 || row->blob_size != length + 2)
        $RAISE((B_BaseException)$NEW(B_ValueError,
                                     to$str("invalid serialized array")));

    self->$class = &B_arrayG_methods;
    B_array_init_storage(self, kind, length, B_None);
    if (length > 0)
        memcpy(self->data, &row->blob[2], (size_t)length * sizeof(uint64_t));
    return self;
}
