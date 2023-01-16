struct B_i64G_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_i64, B_atom);
    void (*__serialize__)(B_i64,$Serial$state);
    B_i64 (*__deserialize__)(B_i64,$Serial$state);
    B_bool (*__bool__)(B_i64);
    B_str (*__str__)(B_i64);
    B_str (*__repr__)(B_i64);
};

struct B_i64 {
    struct B_i64G_class *$class;
    long val;
};

extern struct B_i64G_class B_i64G_methods;
B_i64 B_i64G_new(B_atom);

extern struct B_IntegralD_i64G_class B_IntegralD_i64G_methods;
B_IntegralD_i64 B_IntegralD_i64G_new();
extern struct B_LogicalD_IntegralD_i64G_class B_LogicalD_IntegralD_i64G_methods;
B_LogicalD_IntegralD_i64 B_LogicalD_IntegralD_i64G_new(B_Integral);
extern struct B_MinusD_IntegralD_i64G_class B_MinusD_IntegralD_i64G_methods;
B_MinusD_IntegralD_i64 B_MinusD_IntegralD_i64G_new(B_Integral);
extern struct B_OrdD_i64G_class B_OrdD_i64G_methods;
B_OrdD_i64 B_OrdD_i64G_new();
extern struct B_DivD_i64G_class B_DivD_i64G_methods;
B_DivD_i64 B_DivD_i64G_new();
extern struct B_HashableD_i64G_class B_HashableD_i64G_methods;
B_HashableD_i64 B_HashableD_i64G_new();

extern struct B_IntegralD_i64 *B_IntegralD_i64G_witness;
extern struct B_LogicalD_IntegralD_i64 *B_LogicalD_IntegralD_i64G_witness;
extern struct B_MinusD_IntegralD_i64 *B_MinusD_IntegralD_i64G_witness;
extern struct B_DivD_i64 *B_DivD_i64G_witness;
extern struct B_OrdD_i64 *B_OrdD_i64G_witness;
extern struct B_HashableD_i64 *B_HashableD_i64G_witness;


B_i64 toB_i64(long n);
long fromB_i64(B_i64 n);

/*
  #define toB_i64(n)  ({B_i64 $res = malloc(sizeof(struct B_i64)); \
  $res->$class = &B_i64G_methods; \
  $res->val = n; \
  $res;})

  #define fromB_i64(n)  (((B_i64)n)->val)
*/

B_i64 B_i64G_new(B_atom a);

// only called with e>=0.
long longpow(long a, long e); // used also for ndarrays

