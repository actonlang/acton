struct B_i8 {
    struct B_i8G_class *$class;
    int8_t val;
};

 
B_i8 toB_i8(int8_t n);
int8_t fromB_i8(B_i8 n);

int8_t B_i8G_new(B_atom a, B_int base);
 
#define i8_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i8 truediv: division by zero"))); (double)a/(double)b;} )
#define i8_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i8 floordiv: division by zero")));  a/b;} )
#define i8_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i8 mod: division by zero"))); a%b;} )

int8_t i8_pow(int8_t a, int8_t b);

B_i8 B_IntegralD_i8D___add__(B_IntegralD_i8 wit,  B_i8 a, B_i8 b);
B_i8 B_IntegralD_i8D___zero__(B_IntegralD_i8 wit);
B_complex B_IntegralD_i8D___complex__(B_IntegralD_i8 wit, B_i8 a);
B_i8 B_IntegralD_i8D___fromatom__(B_IntegralD_i8 wit, B_atom a);
B_i8 B_IntegralD_i8D___mul__(B_IntegralD_i8 wit,  B_i8 a, B_i8 b);
B_i8 B_IntegralD_i8D___pow__(B_IntegralD_i8 wit,  B_i8 a, B_i8 b);
B_i8 B_IntegralD_i8D___neg__(B_IntegralD_i8 wit,  B_i8 a);
B_i8 B_IntegralD_i8D___pos__(B_IntegralD_i8 wit,  B_i8 a);
$WORD B_IntegralD_i8D_real(B_IntegralD_i8 wit, B_i8 a, B_Real wit2);
$WORD B_IntegralD_i8D_imag(B_IntegralD_i8 wit, B_i8 a, B_Real wit2);
$WORD B_IntegralD_i8D___abs__(B_IntegralD_i8 wit, B_i8 a, B_Real wit2);
B_i8 B_IntegralD_i8D_conjugate(B_IntegralD_i8 wit,  B_i8 a);
double B_IntegralD_i8D___float__ (B_IntegralD_i8 wit, B_i8 n);
$WORD B_IntegralD_i8D___trunc__ (B_IntegralD_i8 wit, B_i8 n, B_Integral wit2);
$WORD B_IntegralD_i8D___floor__ (B_IntegralD_i8 wit, B_i8 n, B_Integral wit2);
$WORD B_IntegralD_i8D___ceil__ (B_IntegralD_i8 wit, B_i8 n, B_Integral wit2);
B_i8 B_IntegralD_i8D___round__ (B_IntegralD_i8 wit, B_i8 n, B_int p);
$WORD B_IntegralD_i8D_numerator (B_IntegralD_i8 wit, B_i8 n, B_Integral wit2);
$WORD B_IntegralD_i8D_denominator (B_IntegralD_i8 wit, B_i8 n, B_Integral wit2);
int64_t B_IntegralD_i8D___int__ (B_IntegralD_i8 wit, B_i8 n);
int64_t B_IntegralD_i8D___index__(B_IntegralD_i8 wit, B_i8 n);
B_tuple B_IntegralD_i8D___divmod__(B_IntegralD_i8 wit, B_i8 a, B_i8 b);
B_i8 B_IntegralD_i8D___floordiv__(B_IntegralD_i8 wit, B_i8 a, B_i8 b);
B_i8 B_IntegralD_i8D___mod__(B_IntegralD_i8 wit, B_i8 a, B_i8 b);
B_i8 B_IntegralD_i8D___lshift__(B_IntegralD_i8 wit,  B_i8 a, int64_t b);
B_i8 B_IntegralD_i8D___rshift__(B_IntegralD_i8 wit,  B_i8 a, int64_t b);
B_i8 B_IntegralD_i8D___invert__(B_IntegralD_i8 wit,  B_i8 a);
B_i8 B_LogicalD_IntegralD_i8D___and__(B_LogicalD_IntegralD_i8 wit,  B_i8 a, B_i8 b);
B_i8 B_LogicalD_IntegralD_i8D___or__(B_LogicalD_IntegralD_i8 wit,  B_i8 a, B_i8 b);
B_i8 B_LogicalD_IntegralD_i8D___xor__(B_LogicalD_IntegralD_i8 wit,  B_i8 a, B_i8 b);
B_i8 B_MinusD_IntegralD_i8D___sub__(B_MinusD_IntegralD_i8 wit,  B_i8 a, B_i8 b);
B_float B_DivD_i8D___truediv__ (B_DivD_i8 wit, B_i8 a, B_i8 b);
B_bool B_OrdD_i8D___eq__ (B_OrdD_i8 wit, B_i8 a, B_i8 b);
B_bool B_OrdD_i8D___ne__ (B_OrdD_i8 wit, B_i8 a, B_i8 b);
B_bool B_OrdD_i8D___lt__ (B_OrdD_i8 wit, B_i8 a, B_i8 b);
B_bool B_OrdD_i8D___le__ (B_OrdD_i8 wit, B_i8 a, B_i8 b);
B_bool B_OrdD_i8D___gt__ (B_OrdD_i8 wit, B_i8 a, B_i8 b);
B_bool B_OrdD_i8D___ge__ (B_OrdD_i8 wit, B_i8 a, B_i8 b);
B_bool B_HashableD_i8D___eq__(B_HashableD_i8 wit, B_i8 a, B_i8 b);
B_bool B_HashableD_i8D___ne__(B_HashableD_i8 wit, B_i8 a, B_i8 b);
B_NoneType B_HashableD_i8D_hash(B_HashableD_i8 wit, B_i8 a, B_hasher h);
