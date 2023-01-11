#include <bsdnt/zz.h>

struct B_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_int, B_atom);
    void (*__serialize__)(B_int,$NoneType);
    B_int (*__deserialize__)(B_int,$NoneType);
    B_bool (*__bool__)(B_int);
    B_str (*__str__)(B_int);
    B_str (*__repr__)(B_int);
};

struct B_int {
    struct B_intG_class *$class;
    zz_struct val;
};

extern struct B_intG_class B_intG_methods;
B_int B_intG_new(B_atom);

extern struct B_IntegralD_intG_class B_IntegralD_intG_methods;
B_IntegralD_int B_IntegralD_intG_new();
extern struct B_LogicalD_IntegralD_intG_class B_LogicalD_IntegralD_intG_methods;
B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intG_new(B_Integral);
extern struct B_MinusD_IntegralD_intG_class B_MinusD_IntegralD_intG_methods;
B_MinusD_IntegralD_int B_MinusD_IntegralD_intG_new(B_Integral);
extern struct B_OrdD_intG_class B_OrdD_intG_methods;
B_OrdD_int B_OrdD_intG_new();
extern struct B_DivD_intG_class B_DivD_intG_methods;
B_DivD_int B_DivD_intG_new();
extern struct B_HashableD_intG_class B_HashableD_intG_methods;
B_HashableD_int B_HashableD_intG_new();

extern struct B_IntegralD_int *B_IntegralD_intG_witness;
extern struct B_LogicalD_IntegralD_int *B_LogicalD_IntegralD_intG_witness;
extern struct B_MinusD_IntegralD_int *B_MinusD_IntegralD_intG_witness;
extern struct B_DivD_int *B_DivD_intG_witness;
extern struct B_OrdD_int *B_OrdD_intG_witness;
extern struct B_HashableD_int *B_HashableD_intG_witness;

B_int zz$toB_int(zz_ptr val);

long fromB_int(B_int n);
B_int toB_int(long n);
 
B_int B_intG_new(B_atom a);

B_int $gcd(B_int, B_int);
B_tuple $xgcd(B_int, B_int);

