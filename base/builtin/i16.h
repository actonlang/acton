struct B_i16 {
    struct B_i16G_class *$class;
    short val;
};

 
B_i16 toB_i16(short n);
short fromB_i16(B_i16 n);

int16_t B_i16G_new(B_atom a, B_int base);
 
#define i16_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 truediv: division by zero"))); (double)a/(double)b;} )
#define i16_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 floordiv: division by zero")));  a/b;} )
#define i16_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 mod: division by zero"))); a%b;} )

int16_t i16_pow(int16_t a, int16_t b);

B_i16 B_IntegralD_i16D___add__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b);
B_i16 B_IntegralD_i16D___zero__(B_IntegralD_i16 wit);
B_complex B_IntegralD_i16D___complex__(B_IntegralD_i16 wit, B_i16 a);
B_i16 B_IntegralD_i16D___fromatom__(B_IntegralD_i16 wit, B_atom a);
B_i16 B_IntegralD_i16D___mul__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b);
B_i16 B_IntegralD_i16D___pow__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b);
B_i16 B_IntegralD_i16D___neg__(B_IntegralD_i16 wit,  B_i16 a);
B_i16 B_IntegralD_i16D___pos__(B_IntegralD_i16 wit,  B_i16 a);
$WORD B_IntegralD_i16D_real(B_IntegralD_i16 wit, B_i16 a, B_Real wit2);
$WORD B_IntegralD_i16D_imag(B_IntegralD_i16 wit, B_i16 a, B_Real wit2);
$WORD B_IntegralD_i16D___abs__(B_IntegralD_i16 wit, B_i16 a, B_Real wit2);
B_i16 B_IntegralD_i16D_conjugate(B_IntegralD_i16 wit,  B_i16 a);
double B_IntegralD_i16D___float__ (B_IntegralD_i16 wit, B_i16 n);
$WORD B_IntegralD_i16D___trunc__ (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2);
$WORD B_IntegralD_i16D___floor__ (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2);
$WORD B_IntegralD_i16D___ceil__ (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2);
B_i16 B_IntegralD_i16D___round__ (B_IntegralD_i16 wit, B_i16 n, B_int p);
$WORD B_IntegralD_i16D_numerator (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2);
$WORD B_IntegralD_i16D_denominator (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2);
int64_t B_IntegralD_i16D___int__ (B_IntegralD_i16 wit, B_i16 n);
int64_t B_IntegralD_i16D___index__(B_IntegralD_i16 wit, B_i16 n);
B_tuple B_IntegralD_i16D___divmod__(B_IntegralD_i16 wit, B_i16 a, B_i16 b);
B_i16 B_IntegralD_i16D___floordiv__(B_IntegralD_i16 wit, B_i16 a, B_i16 b);
B_i16 B_IntegralD_i16D___mod__(B_IntegralD_i16 wit, B_i16 a, B_i16 b);
B_i16 B_IntegralD_i16D___lshift__(B_IntegralD_i16 wit,  B_i16 a, int64_t b);
B_i16 B_IntegralD_i16D___rshift__(B_IntegralD_i16 wit,  B_i16 a, int64_t b);
B_i16 B_IntegralD_i16D___invert__(B_IntegralD_i16 wit,  B_i16 a);
B_i16 B_LogicalD_IntegralD_i16D___and__(B_LogicalD_IntegralD_i16 wit,  B_i16 a, B_i16 b);
B_i16 B_LogicalD_IntegralD_i16D___or__(B_LogicalD_IntegralD_i16 wit,  B_i16 a, B_i16 b);
B_i16 B_LogicalD_IntegralD_i16D___xor__(B_LogicalD_IntegralD_i16 wit,  B_i16 a, B_i16 b);
B_i16 B_MinusD_IntegralD_i16D___sub__(B_MinusD_IntegralD_i16 wit,  B_i16 a, B_i16 b);
B_float B_DivD_i16D___truediv__ (B_DivD_i16 wit, B_i16 a, B_i16 b);
B_bool B_OrdD_i16D___eq__ (B_OrdD_i16 wit, B_i16 a, B_i16 b);
B_bool B_OrdD_i16D___ne__ (B_OrdD_i16 wit, B_i16 a, B_i16 b);
B_bool B_OrdD_i16D___lt__ (B_OrdD_i16 wit, B_i16 a, B_i16 b);
B_bool B_OrdD_i16D___le__ (B_OrdD_i16 wit, B_i16 a, B_i16 b);
B_bool B_OrdD_i16D___gt__ (B_OrdD_i16 wit, B_i16 a, B_i16 b);
B_bool B_OrdD_i16D___ge__ (B_OrdD_i16 wit, B_i16 a, B_i16 b);
B_bool B_HashableD_i16D___eq__(B_HashableD_i16 wit, B_i16 a, B_i16 b);
B_bool B_HashableD_i16D___ne__(B_HashableD_i16 wit, B_i16 a, B_i16 b);
B_NoneType B_HashableD_i16D_hash(B_HashableD_i16 wit, B_i16 a, B_hasher h);
