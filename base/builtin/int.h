struct B_int {
    struct B_intG_class *$class;
    int64_t val;
};

 
B_int toB_int(int64_t n);
B_int to$int(int64_t n);
int64_t fromB_int(B_int n);

int64_t B_intG_new(B_atom a, B_int base);

// only called with e>=0.
long int_pow(long a, long e); // used also for ndarrays

#define int_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("int truediv: division by zero"))); (double)a/(double)b;} )
#define int_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("int floordiv: division by zero")));  a/b;} )
#define int_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("int mod: division by zero"))); a%b;} )

B_int B_IntegralD_intD___add__(B_IntegralD_int wit,  B_int a, B_int b);
B_int B_IntegralD_intD___zero__(B_IntegralD_int wit);
B_complex B_IntegralD_intD___complex__(B_IntegralD_int wit, B_int a);
B_int B_IntegralD_intD___fromatom__(B_IntegralD_int wit, B_atom a);
B_int B_IntegralD_intD___mul__(B_IntegralD_int wit,  B_int a, B_int b);
B_int B_IntegralD_intD___pow__(B_IntegralD_int wit,  B_int a, B_int b);
B_int B_IntegralD_intD___neg__(B_IntegralD_int wit,  B_int a);
B_int B_IntegralD_intD___pos__(B_IntegralD_int wit,  B_int a);
$WORD B_IntegralD_intD_real(B_IntegralD_int wit, B_int a, B_Real wit2);
$WORD B_IntegralD_intD_imag(B_IntegralD_int wit, B_int a, B_Real wit2);
$WORD B_IntegralD_intD___abs__(B_IntegralD_int wit, B_int a, B_Real wit2);
B_int B_IntegralD_intD_conjugate(B_IntegralD_int wit,  B_int a);
double B_IntegralD_intD___float__ (B_IntegralD_int wit, B_int n);
$WORD B_IntegralD_intD___trunc__ (B_IntegralD_int wit, B_int n, B_Integral wit2);
$WORD B_IntegralD_intD___floor__ (B_IntegralD_int wit, B_int n, B_Integral wit2);
$WORD B_IntegralD_intD___ceil__ (B_IntegralD_int wit, B_int n, B_Integral wit2);
B_int B_IntegralD_intD___round__ (B_IntegralD_int wit, B_int n, B_int p);
$WORD B_IntegralD_intD_numerator (B_IntegralD_int wit, B_int n, B_Integral wit2);
$WORD B_IntegralD_intD_denominator (B_IntegralD_int wit, B_int n, B_Integral wit2);
int64_t B_IntegralD_intD___int__ (B_IntegralD_int wit, B_int n);
int64_t B_IntegralD_intD___index__(B_IntegralD_int wit, B_int n);
B_tuple B_IntegralD_intD___divmod__(B_IntegralD_int wit, B_int a, B_int b);
B_int B_IntegralD_intD___floordiv__(B_IntegralD_int wit, B_int a, B_int b);
B_int B_IntegralD_intD___mod__(B_IntegralD_int wit, B_int a, B_int b);
B_int B_IntegralD_intD___lshift__(B_IntegralD_int wit,  B_int a, int64_t b);
B_int B_IntegralD_intD___rshift__(B_IntegralD_int wit,  B_int a, int64_t b);
B_int B_IntegralD_intD___invert__(B_IntegralD_int wit,  B_int a);
B_int B_LogicalD_IntegralD_intD___and__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b);
B_int B_LogicalD_IntegralD_intD___or__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b);
B_int B_LogicalD_IntegralD_intD___xor__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b);
B_int B_MinusD_IntegralD_intD___sub__(B_MinusD_IntegralD_int wit,  B_int a, B_int b);
B_float B_DivD_intD___truediv__ (B_DivD_int wit, B_int a, B_int b);
B_bool B_OrdD_intD___eq__ (B_OrdD_int wit, B_int a, B_int b);
B_bool B_OrdD_intD___ne__ (B_OrdD_int wit, B_int a, B_int b);
B_bool B_OrdD_intD___lt__ (B_OrdD_int wit, B_int a, B_int b);
B_bool B_OrdD_intD___le__ (B_OrdD_int wit, B_int a, B_int b);
B_bool B_OrdD_intD___gt__ (B_OrdD_int wit, B_int a, B_int b);
B_bool B_OrdD_intD___ge__ (B_OrdD_int wit, B_int a, B_int b);
B_bool B_HashableD_intD___eq__(B_HashableD_int wit, B_int a, B_int b);
B_bool B_HashableD_intD___ne__(B_HashableD_int wit, B_int a, B_int b);
B_NoneType B_HashableD_intD_hash(B_HashableD_int wit, B_int a, B_hasher h);

