
struct B_float {
    struct B_floatG_class *$class;
    double val;
};
extern GC_word B_floatD_gcbm[GC_BITMAP_SIZE(struct B_float)];

// #define B_RealD_floatG_new(...) B_RealFloatG_new(__VA_ARGS__)
// #define B_RealD_float B_RealFloat

B_float to$float(double x);
double fromB_float(B_float x);

B_float B_floatG_new(B_atom a);
