/*
 * This file is deliberately named complx.h to avoid collisions under gcc or
 * clang. Details are murky, is it with the standard complex.h? (Path should be
 * different, no?) Björn should know the details..
 */
#include <complex.h>

struct B_complexG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_complex, B_Number, $WORD);
    void (*__serialize__)(B_complex,$Serial$state);
    B_complex (*__deserialize__)(B_complex,$Serial$state);
    B_bool (*__bool__)(B_complex);
    B_str (*__str__)(B_complex);
    B_str (*__repr__)(B_complex);
};

struct B_complex {
    struct B_complexG_class *$class;
    complex double val;
};

extern struct B_complexG_class B_complexG_methods;
B_complex B_complexG_new(B_Number, $WORD);

B_complex toB_complex(complex double c);

extern struct B_NumberD_complexG_class B_NumberD_complexG_methods;
B_NumberD_complex B_NumberD_complexG_new();
extern struct B_DivD_complexG_class B_DivD_complexG_methods;
B_DivD_complex B_DivD_complexG_new();
extern struct B_MinusD_NumberD_complexG_class B_MinusD_NumberD_complexG_methods;
B_MinusD_NumberD_complex B_MinusD_NumberD_complexG_new(B_Number);
extern struct B_EqD_complexG_class B_EqD_complexG_methods;
B_EqD_complex B_EqD_complexG_new();
extern struct B_HashableD_complexG_class B_HashableD_complexG_methods;
B_HashableD_complex B_HashableD_complexG_new();

extern struct B_NumberD_complex *B_NumberD_complexG_witness;
extern struct B_MinusD_NumberD_complex *B_MinusD_NumberD_complexG_witness;
extern struct B_EqD_complex *B_EqD_complexG_witness;
extern struct B_HashableD_complex *B_HashableD_complexG_witness;
