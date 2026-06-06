struct B_u64 {
    struct B_u64G_class *$class;
    uint64_t val;
};

B_u64 toB_u64(uint64_t n);
uint64_t fromB_u64(B_u64 n);

uint64_t B_u64G_new(B_atom a, B_int base);

#define u64_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u64 truediv: division by zero"))); (double)a/(double)b;} )
#define u64_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u64 floordiv: division by zero")));  a/b;} )
#define u64_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u64 mod: division by zero"))); a%b;} )

uint64_t u64_pow(uint64_t a, uint64_t b);

B_u64 B_IntegralD_u64D___add__(B_IntegralD_u64 wit,  B_u64 a, B_u64 b);
B_u64 B_IntegralD_u64D___zero__(B_IntegralD_u64 wit);
B_complex B_IntegralD_u64D___complex__(B_IntegralD_u64 wit, B_u64 a);
B_u64 B_IntegralD_u64D___fromatom__(B_IntegralD_u64 wit, B_atom a);
B_u64 B_IntegralD_u64D___mul__(B_IntegralD_u64 wit,  B_u64 a, B_u64 b);
B_u64 B_IntegralD_u64D___pow__(B_IntegralD_u64 wit,  B_u64 a, B_u64 b);
B_u64 B_IntegralD_u64D___neg__(B_IntegralD_u64 wit,  B_u64 a);
B_u64 B_IntegralD_u64D___pos__(B_IntegralD_u64 wit,  B_u64 a);
$WORD B_IntegralD_u64D_real(B_IntegralD_u64 wit, B_u64 a, B_Real wit2);
$WORD B_IntegralD_u64D_imag(B_IntegralD_u64 wit, B_u64 a, B_Real wit2);
$WORD B_IntegralD_u64D___abs__(B_IntegralD_u64 wit, B_u64 a, B_Real wit2);
B_u64 B_IntegralD_u64D_conjugate(B_IntegralD_u64 wit,  B_u64 a);
double B_IntegralD_u64D___float__ (B_IntegralD_u64 wit, B_u64 n);
$WORD B_IntegralD_u64D___trunc__ (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2);
$WORD B_IntegralD_u64D___floor__ (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2);
$WORD B_IntegralD_u64D___ceil__ (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2);
B_u64 B_IntegralD_u64D___round__ (B_IntegralD_u64 wit, B_u64 n, B_int p);
$WORD B_IntegralD_u64D_numerator (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2);
$WORD B_IntegralD_u64D_denominator (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2);
int64_t B_IntegralD_u64D___int__ (B_IntegralD_u64 wit, B_u64 n);
int64_t B_IntegralD_u64D___index__(B_IntegralD_u64 wit, B_u64 n);
B_tuple B_IntegralD_u64D___divmod__(B_IntegralD_u64 wit, B_u64 a, B_u64 b);
B_u64 B_IntegralD_u64D___floordiv__(B_IntegralD_u64 wit, B_u64 a, B_u64 b);
B_u64 B_IntegralD_u64D___mod__(B_IntegralD_u64 wit, B_u64 a, B_u64 b);
B_u64 B_IntegralD_u64D___lshift__(B_IntegralD_u64 wit,  B_u64 a, int64_t b);
B_u64 B_IntegralD_u64D___rshift__(B_IntegralD_u64 wit,  B_u64 a, int64_t b);
B_u64 B_IntegralD_u64D___invert__(B_IntegralD_u64 wit,  B_u64 a);
B_u64 B_LogicalD_IntegralD_u64D___and__(B_LogicalD_IntegralD_u64 wit,  B_u64 a, B_u64 b);
B_u64 B_LogicalD_IntegralD_u64D___or__(B_LogicalD_IntegralD_u64 wit,  B_u64 a, B_u64 b);
B_u64 B_LogicalD_IntegralD_u64D___xor__(B_LogicalD_IntegralD_u64 wit,  B_u64 a, B_u64 b);
B_u64 B_MinusD_IntegralD_u64D___sub__(B_MinusD_IntegralD_u64 wit,  B_u64 a, B_u64 b);
B_float B_DivD_u64D___truediv__ (B_DivD_u64 wit, B_u64 a, B_u64 b);
B_bool B_OrdD_u64D___eq__ (B_OrdD_u64 wit, B_u64 a, B_u64 b);
B_bool B_OrdD_u64D___ne__ (B_OrdD_u64 wit, B_u64 a, B_u64 b);
B_bool B_OrdD_u64D___lt__ (B_OrdD_u64 wit, B_u64 a, B_u64 b);
B_bool B_OrdD_u64D___le__ (B_OrdD_u64 wit, B_u64 a, B_u64 b);
B_bool B_OrdD_u64D___gt__ (B_OrdD_u64 wit, B_u64 a, B_u64 b);
B_bool B_OrdD_u64D___ge__ (B_OrdD_u64 wit, B_u64 a, B_u64 b);
B_bool B_HashableD_u64D___eq__(B_HashableD_u64 wit, B_u64 a, B_u64 b);
B_bool B_HashableD_u64D___ne__(B_HashableD_u64 wit, B_u64 a, B_u64 b);
B_NoneType B_HashableD_u64D_hash(B_HashableD_u64 wit, B_u64 a, B_hasher h);
