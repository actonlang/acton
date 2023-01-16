struct B_floatG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_float, B_atom);
    void (*__serialize__)(B_float,$Serial$state);
    B_float (*__deserialize__)(B_float,$Serial$state);
    B_bool (*__bool__)(B_float);
    B_str (*__str__)(B_float);
    B_str (*__repr__)(B_float);
};


struct B_float {
    struct B_floatG_class *$class;
    double val;
};

extern struct B_floatG_class B_floatG_methods;
B_float B_floatG_new(B_atom);

extern struct B_RealD_floatG_class B_RealD_floatG_methods;
B_RealD_float B_RealD_floatG_new();

#define B_RealFloatD_floatG_new(...) B_RealD_floatG_new(__VA_ARGS__)
#define B_RealFloatD_float B_RealD_float

extern struct B_DivD_floatG_class B_DivD_floatG_methods;
B_DivD_float B_DivD_floatG_new();

extern struct B_MinusD_RealD_floatG_class B_MinusD_RealD_floatG_methods;
B_MinusD_RealD_float B_MinusD_RealD_floatG_new(B_Real);

extern struct B_OrdD_floatG_class B_OrdD_floatG_methods;
B_OrdD_float B_OrdD_floatG_new();
extern struct B_HashableD_floatG_class B_HashableD_floatG_methods;
B_HashableD_float B_HashableD_floatG_new();

extern struct B_RealD_float *B_RealD_floatG_witness;
extern struct B_MinusD_RealD_float *B_MinusD_RealD_floatG_witness;
extern struct B_OrdD_float *B_OrdD_floatG_witness;
extern struct B_HashableD_float *B_HashableD_floatG_witness;

B_float to$float(double x);
double fromB_float(B_float x);

B_float B_floatG_new(B_atom a);
