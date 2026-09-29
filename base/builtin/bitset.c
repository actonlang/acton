static struct B_ValueError B_bitset_negative_capacity_error =
    STATIC_EXCEPTION(B_ValueError, "bitset capacity must be non-negative");
static struct B_ValueError B_bitset_too_large_error =
    STATIC_EXCEPTION(B_ValueError, "bitset is too large");
static struct B_ValueError B_bitset_element_error =
    STATIC_EXCEPTION(B_ValueError, "bitset element outside universe");
static struct B_ValueError B_bitset_empty_pop_error =
    STATIC_EXCEPTION(B_ValueError, "pop from an empty bitset");
static struct B_ValueError B_bitset_invalid_serialized_error =
    STATIC_EXCEPTION(B_ValueError, "invalid serialized bitset");

static uint64_t B_bitset_word_count(int64_t capacity) {
    return ((uint64_t)capacity + 63) / 64;
}

static uint64_t B_bitset_tail_mask(int64_t capacity) {
    unsigned used = (unsigned)((uint64_t)capacity & 63);
    return used == 0 ? UINT64_MAX : (UINT64_C(1) << used) - 1;
}

static void B_bitset_init_storage(B_bitset self, int64_t capacity) {
    if (capacity < 0)
        RAISE_EXC(&B_bitset_negative_capacity_error);

    uint64_t word_count = B_bitset_word_count(capacity);
    if (word_count > SIZE_MAX / sizeof(uint64_t))
        RAISE_EXC(&B_bitset_too_large_error);

    self->capacity = capacity;
    self->count = 0;
    self->pop_cursor = 0;
    if (word_count == 0) {
        self->data = NULL;
        return;
    }
    self->data = acton_malloc_atomic((size_t)word_count * sizeof(uint64_t));
    memset(self->data, 0, (size_t)word_count * sizeof(uint64_t));
}

static B_bitset B_bitset_alloc(int64_t capacity) {
    B_bitset self = acton_malloc(sizeof(struct B_bitset));
    self->$class = &B_bitsetG_methods;
    B_bitset_init_storage(self, capacity);
    return self;
}

static bool B_bitset_contains_raw(B_bitset self, int64_t elem) {
    if (elem < 0 || elem >= self->capacity)
        return false;
    uint64_t bit = (uint64_t)elem;
    return (self->data[bit >> 6] >> (bit & 63)) & UINT64_C(1);
}

static bool B_bitset_assign_raw(B_bitset self, int64_t elem, bool value) {
    uint64_t bit = (uint64_t)elem;
    uint64_t *word = &self->data[bit >> 6];
    uint64_t mask = UINT64_C(1) << (bit & 63);
    bool old = (*word & mask) != 0;
    if (old == value)
        return false;
    if (value)
        *word |= mask;
    else
        *word &= ~mask;
    self->count += value ? 1 : -1;
    return true;
}

static void B_bitset_add_raw(B_bitset self, int64_t elem) {
    if (elem < 0 || elem >= self->capacity)
        RAISE_EXC(&B_bitset_element_error);
    B_bitset_assign_raw(self, elem, true);
}

static void B_bitset_add_iterable(B_bitset self, B_Iterable wit, $WORD iterable) {
    if (!iterable)
        return;
    B_Iterator it = wit->$class->__iter__(wit, iterable);
    $WORD value;
    while (it->$class->__next__(it, &value))
        B_bitset_add_raw(self, fromB_int((B_int)value));
}

static bool B_bitset_find(B_bitset self, uint64_t begin, uint64_t end,
                          int64_t *result) {
    if (begin >= end)
        return false;
    uint64_t first_word = begin >> 6;
    uint64_t last_word = (end - 1) >> 6;
    for (uint64_t i = first_word; i <= last_word; i++) {
        uint64_t bits = self->data[i];
        if (i == first_word)
            bits &= UINT64_MAX << (begin & 63);
        if (i == last_word && (end & 63))
            bits &= (UINT64_C(1) << (end & 63)) - 1;
        if (bits) {
            *result = (int64_t)((i << 6) + (uint64_t)__builtin_ctzll(bits));
            return true;
        }
    }
    return false;
}

static uint64_t B_bitset_word_at(B_bitset self, uint64_t index) {
    return index < B_bitset_word_count(self->capacity) ? self->data[index] : 0;
}

static void B_bitset_recount(B_bitset self) {
    self->count = 0;
    uint64_t word_count = B_bitset_word_count(self->capacity);
    for (uint64_t i = 0; i < word_count; i++)
        self->count += __builtin_popcountll(self->data[i]);
}

enum B_bitset_op { B_BITSET_UNION, B_BITSET_INTERSECTION,
                   B_BITSET_DIFFERENCE, B_BITSET_XOR };

static B_bitset B_bitset_binary(B_bitset left, B_bitset right,
                                enum B_bitset_op op) {
    int64_t capacity;
    if (op == B_BITSET_INTERSECTION)
        capacity = left->capacity < right->capacity ? left->capacity : right->capacity;
    else if (op == B_BITSET_DIFFERENCE)
        capacity = left->capacity;
    else
        capacity = left->capacity > right->capacity ? left->capacity : right->capacity;

    B_bitset result = B_bitset_alloc(capacity);
    uint64_t word_count = B_bitset_word_count(capacity);
    for (uint64_t i = 0; i < word_count; i++) {
        uint64_t a = B_bitset_word_at(left, i);
        uint64_t b = B_bitset_word_at(right, i);
        switch (op) {
        case B_BITSET_UNION:        result->data[i] = a | b; break;
        case B_BITSET_INTERSECTION: result->data[i] = a & b; break;
        case B_BITSET_DIFFERENCE:   result->data[i] = a & ~b; break;
        case B_BITSET_XOR:          result->data[i] = a ^ b; break;
        }
    }
    if (word_count)
        result->data[word_count - 1] &= B_bitset_tail_mask(capacity);
    B_bitset_recount(result);
    return result;
}

static bool B_bitset_subset(B_bitset left, B_bitset right) {
    uint64_t word_count = B_bitset_word_count(left->capacity);
    for (uint64_t i = 0; i < word_count; i++) {
        uint64_t a = left->data[i];
        if (a & ~B_bitset_word_at(right, i))
            return false;
    }
    return true;
}

// bitset object methods ///////////////////////////////////////////////////////////////////////////

B_bitset B_bitsetG_new(B_Iterable wit, int64_t capacity, $WORD iterable) {
    return $NEW(B_bitset, wit, capacity, iterable);
}

B_NoneType B_bitsetD___init__(B_bitset self, B_Iterable wit,
                              int64_t capacity, $WORD iterable) {
    B_bitset_init_storage(self, capacity);
    B_bitset_add_iterable(self, wit, iterable);
    return B_None;
}

bool B_bitsetD___bool__(B_bitset self) {
    return self->count != 0;
}

static B_list B_bitset_elements(B_bitset self) {
    B_list values = $NEW(B_list, NULL, NULL);
    B_SequenceD_list list_wit = B_SequenceD_listG_witness;
    int64_t elem;
    uint64_t next = 0;
    while (B_bitset_find(self, next, (uint64_t)self->capacity, &elem)) {
        B_int value = toB_int(elem);
        list_wit->$class->append(list_wit, values,
                                 value->$class->__repr__(value));
        next = (uint64_t)elem + 1;
    }
    return values;
}

B_str B_bitsetD___str__(B_bitset self) {
    B_str values = B_strD_join_par('[', B_bitset_elements(self), ']');
    return $FORMAT("bitset(%lld, %.*s)", (long long)self->capacity,
                   values->nbytes, values->str);
}

B_str B_bitsetD___repr__(B_bitset self) {
    return B_bitsetD___str__(self);
}

int64_t B_bitsetD_capacity(B_bitset self) {
    return self->capacity;
}

B_bitset B_bitsetD_union(B_bitset self, B_bitset other) {
    return B_bitset_binary(self, other, B_BITSET_UNION);
}

B_bitset B_bitsetD_intersection(B_bitset self, B_bitset other) {
    return B_bitset_binary(self, other, B_BITSET_INTERSECTION);
}

B_bitset B_bitsetD_difference(B_bitset self, B_bitset other) {
    return B_bitset_binary(self, other, B_BITSET_DIFFERENCE);
}

B_bitset B_bitsetD_symmetric_difference(B_bitset self, B_bitset other) {
    return B_bitset_binary(self, other, B_BITSET_XOR);
}

// Iterators ////////////////////////////////////////////////////////////////////////////////////////

static bool B_IteratorD_bitsetD_next(B_IteratorD_bitset self, $WORD *out) {
    int64_t elem;
    if (!B_bitset_find(self->src, self->next, (uint64_t)self->src->capacity, &elem))
        return false;
    self->next = (uint64_t)elem + 1;
    *out = (B_value)toB_int(elem);
    return true;
}

B_IteratorD_bitset B_IteratorD_bitsetG_new(B_bitset src) {
    return $NEW(B_IteratorD_bitset, src);
}

void B_IteratorD_bitsetD_init(B_IteratorD_bitset self, B_bitset src) {
    self->src = src;
    self->next = 0;
}

bool B_IteratorD_bitsetD_bool(B_IteratorD_bitset self) {
    return true;
}

B_str B_IteratorD_bitsetD_str(B_IteratorD_bitset self) {
    return $FORMAT("<bitset iterator object at %p>", self);
}

void B_IteratorD_bitsetD_serialize(B_IteratorD_bitset self, $Serial$state state) {
    $step_serialize(self->src, state);
    $step_serialize(toB_u64(self->next), state);
}

B_IteratorD_bitset B_IteratorD_bitsetD_deserialize(B_IteratorD_bitset self,
                                                    $Serial$state state) {
    if (!self)
        self = $DNEW(B_IteratorD_bitset, state);
    self->src = (B_bitset)$step_deserialize(state);
    self->next = fromB_u64((B_u64)$step_deserialize(state));
    return self;
}

struct B_IteratorD_bitsetG_class B_IteratorD_bitsetG_methods = {
    "B_IteratorD_bitset", UNASSIGNED, ($SuperG_class)&B_IteratorG_methods,
    B_IteratorD_bitsetD_init, B_IteratorD_bitsetD_serialize,
    B_IteratorD_bitsetD_deserialize, B_IteratorD_bitsetD_bool,
    B_IteratorD_bitsetD_str, B_IteratorD_bitsetD_str,
    B_IteratorD_bitsetD_next
};

// Set[int] /////////////////////////////////////////////////////////////////////////////////////////

B_Iterator B_SetD_bitsetD___iter__(B_SetD_bitset wit, B_bitset self) {
    return (B_Iterator)B_IteratorD_bitsetG_new(self);
}

B_bitset B_SetD_bitsetD___fromiter__(B_SetD_bitset wit,
                                     B_Iterable iter_wit, $WORD iterable) {
    B_list values = B_listG_new(iter_wit, iterable);
    int64_t maximum = -1;
    for (int64_t i = 0; i < values->length; i++) {
        int64_t elem = fromB_int((B_int)values->data[i]);
        if (elem < 0)
            RAISE_EXC(&B_bitset_element_error);
        if (elem > maximum)
            maximum = elem;
    }
    if (maximum == INT64_MAX)
        RAISE_EXC(&B_bitset_too_large_error);
    B_bitset result = B_bitset_alloc(maximum + 1);
    for (int64_t i = 0; i < values->length; i++)
        B_bitset_add_raw(result, fromB_int((B_int)values->data[i]));
    return result;
}

int64_t B_SetD_bitsetD___len__(B_SetD_bitset wit, B_bitset self) {
    return self->count;
}

bool B_SetD_bitsetD___contains__(B_SetD_bitset wit, B_bitset self, B_int elem) {
    return B_bitset_contains_raw(self, elem->val);
}

bool B_SetD_bitsetD___containsnot__(B_SetD_bitset wit, B_bitset self, B_int elem) {
    return !B_bitset_contains_raw(self, elem->val);
}

bool B_SetD_bitsetD_isdisjoint(B_SetD_bitset wit, B_bitset left, B_bitset right) {
    uint64_t words = B_bitset_word_count(left->capacity < right->capacity
                                         ? left->capacity : right->capacity);
    for (uint64_t i = 0; i < words; i++) {
        if (left->data[i] & right->data[i])
            return false;
    }
    return true;
}

B_NoneType B_SetD_bitsetD_add(B_SetD_bitset wit, B_bitset self, B_int elem) {
    B_bitset_add_raw(self, elem->val);
    return B_None;
}

B_NoneType B_SetD_bitsetD_discard(B_SetD_bitset wit, B_bitset self, B_int elem) {
    if (elem->val >= 0 && elem->val < self->capacity)
        B_bitset_assign_raw(self, elem->val, false);
    return B_None;
}

B_int B_SetD_bitsetD_pop(B_SetD_bitset wit, B_bitset self) {
    if (self->count == 0)
        RAISE_EXC(&B_bitset_empty_pop_error);

    int64_t elem;
    uint64_t capacity = (uint64_t)self->capacity;
    uint64_t begin = self->pop_cursor < capacity ? self->pop_cursor : 0;
    if (!B_bitset_find(self, begin, capacity, &elem) &&
        !B_bitset_find(self, 0, begin, &elem))
        RAISE_EXC(&B_bitset_empty_pop_error);

    B_bitset_assign_raw(self, elem, false);
    self->pop_cursor = (uint64_t)elem + 1;
    if (self->pop_cursor >= capacity)
        self->pop_cursor = 0;
    return toB_int(elem);
}

B_NoneType B_SetD_bitsetD_update(B_SetD_bitset wit, B_bitset self,
                                 B_Iterable iter_wit, $WORD iterable) {
    B_bitset_add_iterable(self, iter_wit, iterable);
    return B_None;
}

bool B_OrdD_SetD_bitsetD___eq__(B_OrdD_SetD_bitset wit,
                                B_bitset left, B_bitset right) {
    return left->count == right->count && B_bitset_subset(left, right);
}

bool B_OrdD_SetD_bitsetD___lt__(B_OrdD_SetD_bitset wit,
                                B_bitset left, B_bitset right) {
    return left->count < right->count && B_bitset_subset(left, right);
}

B_bitset B_LogicalD_SetD_bitsetD___and__(B_LogicalD_SetD_bitset wit,
                                         B_bitset left, B_bitset right) {
    return B_bitsetD_intersection(left, right);
}

B_bitset B_LogicalD_SetD_bitsetD___or__(B_LogicalD_SetD_bitset wit,
                                        B_bitset left, B_bitset right) {
    return B_bitsetD_union(left, right);
}

B_bitset B_LogicalD_SetD_bitsetD___xor__(B_LogicalD_SetD_bitset wit,
                                         B_bitset left, B_bitset right) {
    return B_bitsetD_symmetric_difference(left, right);
}

B_bitset B_MinusD_SetD_bitsetD___sub__(B_MinusD_SetD_bitset wit,
                                       B_bitset left, B_bitset right) {
    return B_bitsetD_difference(left, right);
}

// Serialization ///////////////////////////////////////////////////////////////////////////////////

void B_bitsetD___serialize__(B_bitset self, $Serial$state state) {
    uint64_t word_count = B_bitset_word_count(self->capacity);
    if (word_count > INT_MAX - 1)
        RAISE_EXC(&B_bitset_too_large_error);
    $ROW row = $add_header(BITSET_ID, (int)word_count + 1, state);
    row->blob[0] = ($WORD)(intptr_t)self->capacity;
    if (word_count)
        memcpy(&row->blob[1], self->data, (size_t)word_count * sizeof(uint64_t));
}

B_bitset B_bitsetD___deserialize__(B_bitset self, $Serial$state state) {
    if (!self) {
        self = acton_malloc(sizeof(struct B_bitset));
        self->$class = &B_bitsetG_methods;
        B_dictD_setitem(state->done, (B_Hashable)B_HashableD_intG_witness,
                        toB_int(state->row_no - 1), self);
    }

    $ROW row = state->row;
    if (!row || row->class_id != BITSET_ID || row->blob_size < 1)
        RAISE_EXC(&B_bitset_invalid_serialized_error);
    state->row = row->next;
    state->row_no++;

    int64_t capacity = (int64_t)(intptr_t)row->blob[0];
    if (capacity < 0)
        RAISE_EXC(&B_bitset_invalid_serialized_error);
    uint64_t word_count = B_bitset_word_count(capacity);
    if (word_count > INT_MAX - 1 || row->blob_size != (int)word_count + 1)
        RAISE_EXC(&B_bitset_invalid_serialized_error);

    self->$class = &B_bitsetG_methods;
    B_bitset_init_storage(self, capacity);
    if (word_count) {
        memcpy(self->data, &row->blob[1], (size_t)word_count * sizeof(uint64_t));
        self->data[word_count - 1] &= B_bitset_tail_mask(capacity);
        B_bitset_recount(self);
    }
    return self;
}
