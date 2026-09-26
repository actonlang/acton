#pragma once

enum B_array_kind {
    B_ARRAY_INT,
    B_ARRAY_FLOAT
};

struct B_array {
    struct B_arrayG_class *$class;
    enum B_array_kind kind;
    int64_t length;
    void *data;
};

// Raw entry points selected by the compiler when the element type is known.
// They preserve bounds checks while avoiding the boxed generic method ABI.
int64_t $arrayD_U__getitem_int(B_array self, int64_t index);
double $arrayD_U__getitem_float(B_array self, int64_t index);
B_NoneType $arrayD_U__setitem_int(B_array self, int64_t index, int64_t value);
B_NoneType $arrayD_U__setitem_float(B_array self, int64_t index, double value);
int64_t $arrayD_U__len(B_array self);

// Concrete inherited-method symbols required by C-defined builtin classes.
bool B_arrayD___bool__(B_array self);
B_str B_arrayD___str__(B_array self);
B_str B_arrayD___repr__(B_array self);
