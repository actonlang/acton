#pragma once

#include "__builtin__.h"

struct B_IteratorG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_Iterator);
    void (*__serialize__)(B_Iterator,$Serial$state);
    B_Iterator (*__deserialize__)(B_Iterator,$Serial$state);
    B_bool (*__bool__)(B_Iterator);
    B_str (*__str__)(B_Iterator);
    B_str (*__repr__)(B_Iterator);
    $WORD (*__next__)(B_Iterator);
};

struct B_Iterator {
    struct B_IteratorG_class *$class;
};

extern struct B_IterableD_IteratorG_class B_IterableD_IteratorG_methods;
B_IterableD_Iterator B_IterableD_IteratorG_new();
extern struct B_IterableD_Iterator *B_IterableD_IteratorG_witness;

extern struct B_IteratorG_class B_IteratorG_methods;
B_Iterator B_IteratorG_new();
extern struct B_Iterator *B_IteratorG_witness;

$WORD $next(B_Iterator);
