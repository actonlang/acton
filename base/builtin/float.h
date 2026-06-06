
struct B_float {
    struct B_floatG_class *$class;
    double val;
};

// #define B_RealD_floatG_new(...) B_RealFloatG_new(__VA_ARGS__)
// #define B_RealD_float B_RealFloat

B_float to$float(double x); // Dare not remove this; possibly used in compiler...?

B_float toB_float(double x);
double fromB_float(B_float x);

double B_floatG_new(B_atom a);

#define float_DIV(x,y)           (x/y)
#define float_pow(x,y)           (pow(x,y))

B_float B_RealFloatD_floatD___add__(B_RealFloatD_float wit,  B_float a, B_float b);
B_float B_RealFloatD_floatD___zero__(B_RealFloatD_float wit);
B_complex B_RealFloatD_floatD___complex__(B_RealFloatD_float wit, B_float a);
B_float B_RealFloatD_floatD___fromatom__(B_RealFloatD_float wit, B_atom a);
B_float B_RealFloatD_floatD___mul__(B_RealFloatD_float wit,  B_float a, B_float b);
B_float B_RealFloatD_floatD___pow__(B_RealFloatD_float wit,  B_float a, B_float b);
B_float B_RealFloatD_floatD___neg__(B_RealFloatD_float wit,  B_float a);
B_float B_RealFloatD_floatD___pos__(B_RealFloatD_float wit,  B_float a);
$WORD B_RealFloatD_floatD_real(B_RealFloatD_float wit, B_float a, B_Real wit2);
$WORD B_RealFloatD_floatD_imag(B_RealFloatD_float wit, B_float a, B_Real wit2);
$WORD B_RealFloatD_floatD___abs__(B_RealFloatD_float wit, B_float a, B_Real wit2);
B_float B_RealFloatD_floatD_conjugate(B_RealFloatD_float wit,  B_float a);
double B_RealFloatD_floatD___float__ (B_RealFloatD_float wit, B_float n);
$WORD B_RealFloatD_floatD___trunc__ (B_RealFloatD_float wit, B_float n, B_Integral wit2);
$WORD B_RealFloatD_floatD___floor__ (B_RealFloatD_float wit, B_float n, B_Integral wit2);
$WORD B_RealFloatD_floatD___ceil__ (B_RealFloatD_float wit, B_float n, B_Integral wit2);
B_float B_RealFloatD_floatD___round__ (B_RealFloatD_float wit, B_float n, B_int p);
B_float B_MinusD_RealFloatDD_floatD___sub__(B_MinusD_RealFloatD_float wit,  B_float a, B_float b);
B_float B_DivD_floatD___truediv__ (B_DivD_float wit, B_float a, B_float b);
B_bool B_OrdD_floatD___eq__ (B_OrdD_float wit, B_float a, B_float b);
B_bool B_OrdD_floatD___ne__ (B_OrdD_float wit, B_float a, B_float b);
B_bool B_OrdD_floatD___lt__ (B_OrdD_float wit, B_float a, B_float b);
B_bool B_OrdD_floatD___le__ (B_OrdD_float wit, B_float a, B_float b);
B_bool B_OrdD_floatD___gt__ (B_OrdD_float wit, B_float a, B_float b);
B_bool B_OrdD_floatD___ge__ (B_OrdD_float wit, B_float a, B_float b);
B_bool B_HashableD_floatD___eq__(B_HashableD_float wit, B_float a, B_float b);
B_bool B_HashableD_floatD___ne__(B_HashableD_float wit, B_float a, B_float b);
B_NoneType B_HashableD_floatD_hash(B_HashableD_float wit, B_float a, B_hasher h);
