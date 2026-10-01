#pragma once

struct B_bitarray {
    struct B_bitarrayG_class *$class;
    int64_t length;
    int64_t count;
    uint64_t *data;
};

// Iterators over bitarrays yield every Boolean element in index order.
typedef struct B_IteratorD_bitarray *B_IteratorD_bitarray;

struct B_IteratorD_bitarrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_bitarray, B_bitarray);
    void (*__serialize__)(B_IteratorD_bitarray, $Serial$state);
    B_IteratorD_bitarray (*__deserialize__)(B_IteratorD_bitarray, $Serial$state);
    bool (*__bool__)(B_IteratorD_bitarray);
    B_str (*__str__)(B_IteratorD_bitarray);
    B_str (*__repr__)(B_IteratorD_bitarray);
    bool (*__next__)(B_IteratorD_bitarray, $WORD *);
};

struct B_IteratorD_bitarray {
    struct B_IteratorD_bitarrayG_class *$class;
    B_bitarray src;
    int64_t next;
};

extern struct B_IteratorD_bitarrayG_class B_IteratorD_bitarrayG_methods;
B_IteratorD_bitarray B_IteratorD_bitarrayG_new(B_bitarray src);

// Raw entry points selected by the compiler for direct bitarray operations.
// They preserve bounds checks while avoiding virtual method dispatch.
bool $bitarrayD_U__getitem__(B_bitarray self, int64_t index);
B_NoneType $bitarrayD_U__setitem__(B_bitarray self, int64_t index, bool value);
int64_t $bitarrayD_U__len(B_bitarray self);

// Concrete inherited-method symbols required by C-defined builtin classes.
bool B_bitarrayD___bool__(B_bitarray self);
B_str B_bitarrayD___str__(B_bitarray self);
B_str B_bitarrayD___repr__(B_bitarray self);
