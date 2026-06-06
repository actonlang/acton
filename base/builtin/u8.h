struct B_u8 {
    struct B_u8G_class *$class;
    uint8_t val;
};

B_u8 toB_u8(uint8_t n);
uint8_t fromB_u8(B_u8 n);

uint8_t B_u8G_new(B_atom a, B_int base);

#define u8_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u8 truediv: division by zero"))); (double)a/(double)b;} )
#define u8_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u8 floordiv: division by zero")));  a/b;} )
#define u8_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u8 mod: division by zero"))); a%b;} )

uint8_t u8_pow(uint8_t a, uint8_t b);

B_u8 B_IntegralD_u8D___add__(B_IntegralD_u8 wit,  B_u8 a, B_u8 b);
B_u8 B_IntegralD_u8D___zero__(B_IntegralD_u8 wit);
B_complex B_IntegralD_u8D___complex__(B_IntegralD_u8 wit, B_u8 a);
B_u8 B_IntegralD_u8D___fromatom__(B_IntegralD_u8 wit, B_atom a);
B_u8 B_IntegralD_u8D___mul__(B_IntegralD_u8 wit,  B_u8 a, B_u8 b);
B_u8 B_IntegralD_u8D___pow__(B_IntegralD_u8 wit,  B_u8 a, B_u8 b);
B_u8 B_IntegralD_u8D___neg__(B_IntegralD_u8 wit,  B_u8 a);
B_u8 B_IntegralD_u8D___pos__(B_IntegralD_u8 wit,  B_u8 a);
$WORD B_IntegralD_u8D_real(B_IntegralD_u8 wit, B_u8 a, B_Real wit2);
$WORD B_IntegralD_u8D_imag(B_IntegralD_u8 wit, B_u8 a, B_Real wit2);
$WORD B_IntegralD_u8D___abs__(B_IntegralD_u8 wit, B_u8 a, B_Real wit2);
B_u8 B_IntegralD_u8D_conjugate(B_IntegralD_u8 wit,  B_u8 a);
double B_IntegralD_u8D___float__ (B_IntegralD_u8 wit, B_u8 n);
$WORD B_IntegralD_u8D___trunc__ (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2);
$WORD B_IntegralD_u8D___floor__ (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2);
$WORD B_IntegralD_u8D___ceil__ (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2);
B_u8 B_IntegralD_u8D___round__ (B_IntegralD_u8 wit, B_u8 n, B_int p);
$WORD B_IntegralD_u8D_numerator (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2);
$WORD B_IntegralD_u8D_denominator (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2);
int64_t B_IntegralD_u8D___int__ (B_IntegralD_u8 wit, B_u8 n);
int64_t B_IntegralD_u8D___index__(B_IntegralD_u8 wit, B_u8 n);
B_tuple B_IntegralD_u8D___divmod__(B_IntegralD_u8 wit, B_u8 a, B_u8 b);
B_u8 B_IntegralD_u8D___floordiv__(B_IntegralD_u8 wit, B_u8 a, B_u8 b);
B_u8 B_IntegralD_u8D___mod__(B_IntegralD_u8 wit, B_u8 a, B_u8 b);
B_u8 B_IntegralD_u8D___lshift__(B_IntegralD_u8 wit,  B_u8 a, int64_t b);
B_u8 B_IntegralD_u8D___rshift__(B_IntegralD_u8 wit,  B_u8 a, int64_t b);
B_u8 B_IntegralD_u8D___invert__(B_IntegralD_u8 wit,  B_u8 a);
B_u8 B_LogicalD_IntegralD_u8D___and__(B_LogicalD_IntegralD_u8 wit,  B_u8 a, B_u8 b);
B_u8 B_LogicalD_IntegralD_u8D___or__(B_LogicalD_IntegralD_u8 wit,  B_u8 a, B_u8 b);
B_u8 B_LogicalD_IntegralD_u8D___xor__(B_LogicalD_IntegralD_u8 wit,  B_u8 a, B_u8 b);
B_u8 B_MinusD_IntegralD_u8D___sub__(B_MinusD_IntegralD_u8 wit,  B_u8 a, B_u8 b);
B_float B_DivD_u8D___truediv__ (B_DivD_u8 wit, B_u8 a, B_u8 b);
B_bool B_OrdD_u8D___eq__ (B_OrdD_u8 wit, B_u8 a, B_u8 b);
B_bool B_OrdD_u8D___ne__ (B_OrdD_u8 wit, B_u8 a, B_u8 b);
B_bool B_OrdD_u8D___lt__ (B_OrdD_u8 wit, B_u8 a, B_u8 b);
B_bool B_OrdD_u8D___le__ (B_OrdD_u8 wit, B_u8 a, B_u8 b);
B_bool B_OrdD_u8D___gt__ (B_OrdD_u8 wit, B_u8 a, B_u8 b);
B_bool B_OrdD_u8D___ge__ (B_OrdD_u8 wit, B_u8 a, B_u8 b);
B_bool B_HashableD_u8D___eq__(B_HashableD_u8 wit, B_u8 a, B_u8 b);
B_bool B_HashableD_u8D___ne__(B_HashableD_u8 wit, B_u8 a, B_u8 b);
B_NoneType B_HashableD_u8D_hash(B_HashableD_u8 wit, B_u8 a, B_hasher h);
