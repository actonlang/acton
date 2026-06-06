struct B_u32 {
    struct B_u32G_class *$class;
    unsigned int val;
};

B_u32 toB_u32(unsigned int n);
unsigned int fromB_u32(B_u32 n);

uint32_t B_u32G_new(B_atom a, B_int base);

#define u32_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u32 truediv: division by zero"))); (double)a/(double)b;} )
#define u32_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u32 floordiv: division by zero")));  a/b;} )
#define u32_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u32 mod: division by zero"))); a%b;} )

uint32_t u32_pow(uint32_t a, uint32_t b);

B_u32 B_IntegralD_u32D___add__(B_IntegralD_u32 wit,  B_u32 a, B_u32 b);
B_u32 B_IntegralD_u32D___zero__(B_IntegralD_u32 wit);
B_complex B_IntegralD_u32D___complex__(B_IntegralD_u32 wit, B_u32 a);
B_u32 B_IntegralD_u32D___fromatom__(B_IntegralD_u32 wit, B_atom a);
B_u32 B_IntegralD_u32D___mul__(B_IntegralD_u32 wit,  B_u32 a, B_u32 b);
B_u32 B_IntegralD_u32D___pow__(B_IntegralD_u32 wit,  B_u32 a, B_u32 b);
B_u32 B_IntegralD_u32D___neg__(B_IntegralD_u32 wit,  B_u32 a);
B_u32 B_IntegralD_u32D___pos__(B_IntegralD_u32 wit,  B_u32 a);
$WORD B_IntegralD_u32D_real(B_IntegralD_u32 wit, B_u32 a, B_Real wit2);
$WORD B_IntegralD_u32D_imag(B_IntegralD_u32 wit, B_u32 a, B_Real wit2);
$WORD B_IntegralD_u32D___abs__(B_IntegralD_u32 wit, B_u32 a, B_Real wit2);
B_u32 B_IntegralD_u32D_conjugate(B_IntegralD_u32 wit,  B_u32 a);
double B_IntegralD_u32D___float__ (B_IntegralD_u32 wit, B_u32 n);
$WORD B_IntegralD_u32D___trunc__ (B_IntegralD_u32 wit, B_u32 n, B_Integral wit2);
$WORD B_IntegralD_u32D___floor__ (B_IntegralD_u32 wit, B_u32 n, B_Integral wit2);
$WORD B_IntegralD_u32D___ceil__ (B_IntegralD_u32 wit, B_u32 n, B_Integral wit2);
B_u32 B_IntegralD_u32D___round__ (B_IntegralD_u32 wit, B_u32 n, B_int p);
$WORD B_IntegralD_u32D_numerator (B_IntegralD_u32 wit, B_u32 n, B_Integral wit2);
$WORD B_IntegralD_u32D_denominator (B_IntegralD_u32 wit, B_u32 n, B_Integral wit2);
int64_t B_IntegralD_u32D___int__ (B_IntegralD_u32 wit, B_u32 n);
int64_t B_IntegralD_u32D___index__(B_IntegralD_u32 wit, B_u32 n);
B_tuple B_IntegralD_u32D___divmod__(B_IntegralD_u32 wit, B_u32 a, B_u32 b);
B_u32 B_IntegralD_u32D___floordiv__(B_IntegralD_u32 wit, B_u32 a, B_u32 b);
B_u32 B_IntegralD_u32D___mod__(B_IntegralD_u32 wit, B_u32 a, B_u32 b);
B_u32 B_IntegralD_u32D___lshift__(B_IntegralD_u32 wit,  B_u32 a, int64_t b);
B_u32 B_IntegralD_u32D___rshift__(B_IntegralD_u32 wit,  B_u32 a, int64_t b);
B_u32 B_IntegralD_u32D___invert__(B_IntegralD_u32 wit,  B_u32 a);
B_u32 B_LogicalD_IntegralD_u32D___and__(B_LogicalD_IntegralD_u32 wit,  B_u32 a, B_u32 b);
B_u32 B_LogicalD_IntegralD_u32D___or__(B_LogicalD_IntegralD_u32 wit,  B_u32 a, B_u32 b);
B_u32 B_LogicalD_IntegralD_u32D___xor__(B_LogicalD_IntegralD_u32 wit,  B_u32 a, B_u32 b);
B_u32 B_MinusD_IntegralD_u32D___sub__(B_MinusD_IntegralD_u32 wit,  B_u32 a, B_u32 b);
B_float B_DivD_u32D___truediv__ (B_DivD_u32 wit, B_u32 a, B_u32 b);
B_bool B_OrdD_u32D___eq__ (B_OrdD_u32 wit, B_u32 a, B_u32 b);
B_bool B_OrdD_u32D___ne__ (B_OrdD_u32 wit, B_u32 a, B_u32 b);
B_bool B_OrdD_u32D___lt__ (B_OrdD_u32 wit, B_u32 a, B_u32 b);
B_bool B_OrdD_u32D___le__ (B_OrdD_u32 wit, B_u32 a, B_u32 b);
B_bool B_OrdD_u32D___gt__ (B_OrdD_u32 wit, B_u32 a, B_u32 b);
B_bool B_OrdD_u32D___ge__ (B_OrdD_u32 wit, B_u32 a, B_u32 b);
B_bool B_HashableD_u32D___eq__(B_HashableD_u32 wit, B_u32 a, B_u32 b);
B_bool B_HashableD_u32D___ne__(B_HashableD_u32 wit, B_u32 a, B_u32 b);
B_NoneType B_HashableD_u32D_hash(B_HashableD_u32 wit, B_u32 a, B_hasher h);
