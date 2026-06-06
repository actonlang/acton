struct B_u16 {
    struct B_u16G_class *$class;
    unsigned short val;
};

B_u16 toB_u16(unsigned short n);
unsigned short fromB_u16(B_u16 n);

uint16_t B_u16G_new(B_atom a, B_int base);

#define u16_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 truediv: division by zero"))); (double)a/(double)b;} )
#define u16_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 floordiv: division by zero")));  a/b;} )
#define u16_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 mod: division by zero"))); a%b;} )

uint16_t u16_pow(uint16_t a, uint16_t b);

B_u16 B_IntegralD_u16D___add__(B_IntegralD_u16 wit,  B_u16 a, B_u16 b);
B_u16 B_IntegralD_u16D___zero__(B_IntegralD_u16 wit);
B_complex B_IntegralD_u16D___complex__(B_IntegralD_u16 wit, B_u16 a);
B_u16 B_IntegralD_u16D___fromatom__(B_IntegralD_u16 wit, B_atom a);
B_u16 B_IntegralD_u16D___mul__(B_IntegralD_u16 wit,  B_u16 a, B_u16 b);
B_u16 B_IntegralD_u16D___pow__(B_IntegralD_u16 wit,  B_u16 a, B_u16 b);
B_u16 B_IntegralD_u16D___neg__(B_IntegralD_u16 wit,  B_u16 a);
B_u16 B_IntegralD_u16D___pos__(B_IntegralD_u16 wit,  B_u16 a);
$WORD B_IntegralD_u16D_real(B_IntegralD_u16 wit, B_u16 a, B_Real wit2);
$WORD B_IntegralD_u16D_imag(B_IntegralD_u16 wit, B_u16 a, B_Real wit2);
$WORD B_IntegralD_u16D___abs__(B_IntegralD_u16 wit, B_u16 a, B_Real wit2);
B_u16 B_IntegralD_u16D_conjugate(B_IntegralD_u16 wit,  B_u16 a);
double B_IntegralD_u16D___float__ (B_IntegralD_u16 wit, B_u16 n);
$WORD B_IntegralD_u16D___trunc__ (B_IntegralD_u16 wit, B_u16 n, B_Integral wit2);
$WORD B_IntegralD_u16D___floor__ (B_IntegralD_u16 wit, B_u16 n, B_Integral wit2);
$WORD B_IntegralD_u16D___ceil__ (B_IntegralD_u16 wit, B_u16 n, B_Integral wit2);
B_u16 B_IntegralD_u16D___round__ (B_IntegralD_u16 wit, B_u16 n, B_int p);
$WORD B_IntegralD_u16D_numerator (B_IntegralD_u16 wit, B_u16 n, B_Integral wit2);
$WORD B_IntegralD_u16D_denominator (B_IntegralD_u16 wit, B_u16 n, B_Integral wit2);
int64_t B_IntegralD_u16D___int__ (B_IntegralD_u16 wit, B_u16 n);
int64_t B_IntegralD_u16D___index__(B_IntegralD_u16 wit, B_u16 n);
B_tuple B_IntegralD_u16D___divmod__(B_IntegralD_u16 wit, B_u16 a, B_u16 b);
B_u16 B_IntegralD_u16D___floordiv__(B_IntegralD_u16 wit, B_u16 a, B_u16 b);
B_u16 B_IntegralD_u16D___mod__(B_IntegralD_u16 wit, B_u16 a, B_u16 b);
B_u16 B_IntegralD_u16D___lshift__(B_IntegralD_u16 wit,  B_u16 a, int64_t b);
B_u16 B_IntegralD_u16D___rshift__(B_IntegralD_u16 wit,  B_u16 a, int64_t b);
B_u16 B_IntegralD_u16D___invert__(B_IntegralD_u16 wit,  B_u16 a);
B_u16 B_LogicalD_IntegralD_u16D___and__(B_LogicalD_IntegralD_u16 wit,  B_u16 a, B_u16 b);
B_u16 B_LogicalD_IntegralD_u16D___or__(B_LogicalD_IntegralD_u16 wit,  B_u16 a, B_u16 b);
B_u16 B_LogicalD_IntegralD_u16D___xor__(B_LogicalD_IntegralD_u16 wit,  B_u16 a, B_u16 b);
B_u16 B_MinusD_IntegralD_u16D___sub__(B_MinusD_IntegralD_u16 wit,  B_u16 a, B_u16 b);
B_float B_DivD_u16D___truediv__ (B_DivD_u16 wit, B_u16 a, B_u16 b);
B_bool B_OrdD_u16D___eq__ (B_OrdD_u16 wit, B_u16 a, B_u16 b);
B_bool B_OrdD_u16D___ne__ (B_OrdD_u16 wit, B_u16 a, B_u16 b);
B_bool B_OrdD_u16D___lt__ (B_OrdD_u16 wit, B_u16 a, B_u16 b);
B_bool B_OrdD_u16D___le__ (B_OrdD_u16 wit, B_u16 a, B_u16 b);
B_bool B_OrdD_u16D___gt__ (B_OrdD_u16 wit, B_u16 a, B_u16 b);
B_bool B_OrdD_u16D___ge__ (B_OrdD_u16 wit, B_u16 a, B_u16 b);
B_bool B_HashableD_u16D___eq__(B_HashableD_u16 wit, B_u16 a, B_u16 b);
B_bool B_HashableD_u16D___ne__(B_HashableD_u16 wit, B_u16 a, B_u16 b);
B_NoneType B_HashableD_u16D_hash(B_HashableD_u16 wit, B_u16 a, B_hasher h);
