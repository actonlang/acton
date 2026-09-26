#pragma once

struct B_bitarray {
    struct B_bitarrayG_class *$class;
    int64_t length;
    uint64_t *data;
};

// Raw entry points selected by the compiler for direct bitarray operations.
// They preserve bounds checks while avoiding virtual method dispatch.
bool $bitarrayD_U__getitem__(B_bitarray self, int64_t index);
B_NoneType $bitarrayD_U__setitem__(B_bitarray self, int64_t index, bool value);
int64_t $bitarrayD_U__len(B_bitarray self);

// Concrete inherited-method symbols required by C-defined builtin classes.
bool B_bitarrayD___bool__(B_bitarray self);
B_str B_bitarrayD___str__(B_bitarray self);
B_str B_bitarrayD___repr__(B_bitarray self);
