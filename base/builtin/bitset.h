#pragma once

struct B_bitset {
    struct B_bitsetG_class *$class;
    int64_t capacity;
    int64_t count;
    uint64_t pop_cursor;
    uint64_t *data;
};

// Iterators over bitsets yield the contained integer elements in increasing order.
typedef struct B_IteratorD_bitset *B_IteratorD_bitset;

struct B_IteratorD_bitsetG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_bitset, B_bitset);
    void (*__serialize__)(B_IteratorD_bitset, $Serial$state);
    B_IteratorD_bitset (*__deserialize__)(B_IteratorD_bitset, $Serial$state);
    bool (*__bool__)(B_IteratorD_bitset);
    B_str (*__str__)(B_IteratorD_bitset);
    B_str (*__repr__)(B_IteratorD_bitset);
    bool (*__next__)(B_IteratorD_bitset, $WORD *);
};

struct B_IteratorD_bitset {
    struct B_IteratorD_bitsetG_class *$class;
    B_bitset src;
    uint64_t next;
};

extern struct B_IteratorD_bitsetG_class B_IteratorD_bitsetG_methods;
B_IteratorD_bitset B_IteratorD_bitsetG_new(B_bitset src);

