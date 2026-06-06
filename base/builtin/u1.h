struct B_u1 {
    struct B_u1G_class *$class;
    uint8_t val;
};

B_u1 toB_u1(uint8_t n);
uint8_t fromB_u1(B_u1 n);

uint8_t B_u1G_new(B_atom a, B_int base);

#define u1_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u1 truediv: division by zero"))); (double)a;} )
#define u1_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u1 floordiv: division by zero")));  a;} )
#define u1_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u1 mod: division by zero"))); (a%b)&1;} )

uint8_t u1_pow(uint8_t a, uint8_t b);

B_u1 B_IntegralD_u1D___add__(B_IntegralD_u1 wit,  B_u1 a, B_u1 b);
B_u1 B_IntegralD_u1D___zero__(B_IntegralD_u1 wit);
B_complex B_IntegralD_u1D___complex__(B_IntegralD_u1 wit, B_u1 a);
B_u1 B_IntegralD_u1D___fromatom__(B_IntegralD_u1 wit, B_atom a);
B_u1 B_IntegralD_u1D___mul__(B_IntegralD_u1 wit,  B_u1 a, B_u1 b);
B_u1 B_IntegralD_u1D___pow__(B_IntegralD_u1 wit,  B_u1 a, B_u1 b);
B_u1 B_IntegralD_u1D___neg__(B_IntegralD_u1 wit,  B_u1 a);
B_u1 B_IntegralD_u1D___pos__(B_IntegralD_u1 wit,  B_u1 a);
$WORD B_IntegralD_u1D_real(B_IntegralD_u1 wit, B_u1 a, B_Real wit2);
$WORD B_IntegralD_u1D_imag(B_IntegralD_u1 wit, B_u1 a, B_Real wit2);
$WORD B_IntegralD_u1D___abs__(B_IntegralD_u1 wit, B_u1 a, B_Real wit2);
B_u1 B_IntegralD_u1D_conjugate(B_IntegralD_u1 wit,  B_u1 a);
double B_IntegralD_u1D___float__ (B_IntegralD_u1 wit, B_u1 n);
$WORD B_IntegralD_u1D___trunc__ (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2);
$WORD B_IntegralD_u1D___floor__ (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2);
$WORD B_IntegralD_u1D___ceil__ (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2);
B_u1 B_IntegralD_u1D___round__ (B_IntegralD_u1 wit, B_u1 n, B_int p);
$WORD B_IntegralD_u1D_numerator (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2);
$WORD B_IntegralD_u1D_denominator (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2);
int64_t B_IntegralD_u1D___int__ (B_IntegralD_u1 wit, B_u1 n);
int64_t B_IntegralD_u1D___index__(B_IntegralD_u1 wit, B_u1 n);
B_tuple B_IntegralD_u1D___divmod__(B_IntegralD_u1 wit, B_u1 a, B_u1 b);
B_u1 B_IntegralD_u1D___floordiv__(B_IntegralD_u1 wit, B_u1 a, B_u1 b);
B_u1 B_IntegralD_u1D___mod__(B_IntegralD_u1 wit, B_u1 a, B_u1 b);
B_u1 B_IntegralD_u1D___lshift__(B_IntegralD_u1 wit,  B_u1 a, int64_t b);
B_u1 B_IntegralD_u1D___rshift__(B_IntegralD_u1 wit,  B_u1 a, int64_t b);
B_u1 B_IntegralD_u1D___invert__(B_IntegralD_u1 wit,  B_u1 a);
B_u1 B_LogicalD_IntegralD_u1D___and__(B_LogicalD_IntegralD_u1 wit,  B_u1 a, B_u1 b);
B_u1 B_LogicalD_IntegralD_u1D___or__(B_LogicalD_IntegralD_u1 wit,  B_u1 a, B_u1 b);
B_u1 B_LogicalD_IntegralD_u1D___xor__(B_LogicalD_IntegralD_u1 wit,  B_u1 a, B_u1 b);
B_u1 B_MinusD_IntegralD_u1D___sub__(B_MinusD_IntegralD_u1 wit,  B_u1 a, B_u1 b);
B_float B_DivD_u1D___truediv__ (B_DivD_u1 wit, B_u1 a, B_u1 b);
B_bool B_OrdD_u1D___eq__ (B_OrdD_u1 wit, B_u1 a, B_u1 b);
B_bool B_OrdD_u1D___ne__ (B_OrdD_u1 wit, B_u1 a, B_u1 b);
B_bool B_OrdD_u1D___lt__ (B_OrdD_u1 wit, B_u1 a, B_u1 b);
B_bool B_OrdD_u1D___le__ (B_OrdD_u1 wit, B_u1 a, B_u1 b);
B_bool B_OrdD_u1D___gt__ (B_OrdD_u1 wit, B_u1 a, B_u1 b);
B_bool B_OrdD_u1D___ge__ (B_OrdD_u1 wit, B_u1 a, B_u1 b);
B_bool B_HashableD_u1D___eq__(B_HashableD_u1 wit, B_u1 a, B_u1 b);
B_bool B_HashableD_u1D___ne__(B_HashableD_u1 wit, B_u1 a, B_u1 b);
B_NoneType B_HashableD_u1D_hash(B_HashableD_u1 wit, B_u1 a, B_hasher h);
