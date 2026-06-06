struct B_i32 {
    struct B_i32G_class *$class;
    int val;
};

 
B_i32 toB_i32(int32_t n);
int32_t fromB_i32(B_i32 n);

int32_t B_i32G_new(B_atom a, B_int base);

#define i32_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 truediv: division by zero"))); (double)a/(double)b;} )
#define i32_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 floordiv: division by zero")));  a/b;} )
#define i32_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 mod: division by zero"))); a%b;} )

int32_t i32_pow(int32_t a, int32_t b);

B_i32 B_IntegralD_i32D___add__(B_IntegralD_i32 wit,  B_i32 a, B_i32 b);
B_i32 B_IntegralD_i32D___zero__(B_IntegralD_i32 wit);
B_complex B_IntegralD_i32D___complex__(B_IntegralD_i32 wit, B_i32 a);
B_i32 B_IntegralD_i32D___fromatom__(B_IntegralD_i32 wit, B_atom a);
B_i32 B_IntegralD_i32D___mul__(B_IntegralD_i32 wit,  B_i32 a, B_i32 b);
B_i32 B_IntegralD_i32D___pow__(B_IntegralD_i32 wit,  B_i32 a, B_i32 b);
B_i32 B_IntegralD_i32D___neg__(B_IntegralD_i32 wit,  B_i32 a);
B_i32 B_IntegralD_i32D___pos__(B_IntegralD_i32 wit,  B_i32 a);
$WORD B_IntegralD_i32D_real(B_IntegralD_i32 wit, B_i32 a, B_Real wit2);
$WORD B_IntegralD_i32D_imag(B_IntegralD_i32 wit, B_i32 a, B_Real wit2);
$WORD B_IntegralD_i32D___abs__(B_IntegralD_i32 wit, B_i32 a, B_Real wit2);
B_i32 B_IntegralD_i32D_conjugate(B_IntegralD_i32 wit,  B_i32 a);
double B_IntegralD_i32D___float__ (B_IntegralD_i32 wit, B_i32 n);
$WORD B_IntegralD_i32D___trunc__ (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2);
$WORD B_IntegralD_i32D___floor__ (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2);
$WORD B_IntegralD_i32D___ceil__ (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2);
B_i32 B_IntegralD_i32D___round__ (B_IntegralD_i32 wit, B_i32 n, B_int p);
$WORD B_IntegralD_i32D_numerator (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2);
$WORD B_IntegralD_i32D_denominator (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2);
int64_t B_IntegralD_i32D___int__ (B_IntegralD_i32 wit, B_i32 n);
int64_t B_IntegralD_i32D___index__(B_IntegralD_i32 wit, B_i32 n);
B_tuple B_IntegralD_i32D___divmod__(B_IntegralD_i32 wit, B_i32 a, B_i32 b);
B_i32 B_IntegralD_i32D___floordiv__(B_IntegralD_i32 wit, B_i32 a, B_i32 b);
B_i32 B_IntegralD_i32D___mod__(B_IntegralD_i32 wit, B_i32 a, B_i32 b);
B_i32 B_IntegralD_i32D___lshift__(B_IntegralD_i32 wit,  B_i32 a, int64_t b);
B_i32 B_IntegralD_i32D___rshift__(B_IntegralD_i32 wit,  B_i32 a, int64_t b);
B_i32 B_IntegralD_i32D___invert__(B_IntegralD_i32 wit,  B_i32 a);
B_i32 B_LogicalD_IntegralD_i32D___and__(B_LogicalD_IntegralD_i32 wit,  B_i32 a, B_i32 b);
B_i32 B_LogicalD_IntegralD_i32D___or__(B_LogicalD_IntegralD_i32 wit,  B_i32 a, B_i32 b);
B_i32 B_LogicalD_IntegralD_i32D___xor__(B_LogicalD_IntegralD_i32 wit,  B_i32 a, B_i32 b);
B_i32 B_MinusD_IntegralD_i32D___sub__(B_MinusD_IntegralD_i32 wit,  B_i32 a, B_i32 b);
B_float B_DivD_i32D___truediv__ (B_DivD_i32 wit, B_i32 a, B_i32 b);
B_bool B_OrdD_i32D___eq__ (B_OrdD_i32 wit, B_i32 a, B_i32 b);
B_bool B_OrdD_i32D___ne__ (B_OrdD_i32 wit, B_i32 a, B_i32 b);
B_bool B_OrdD_i32D___lt__ (B_OrdD_i32 wit, B_i32 a, B_i32 b);
B_bool B_OrdD_i32D___le__ (B_OrdD_i32 wit, B_i32 a, B_i32 b);
B_bool B_OrdD_i32D___gt__ (B_OrdD_i32 wit, B_i32 a, B_i32 b);
B_bool B_OrdD_i32D___ge__ (B_OrdD_i32 wit, B_i32 a, B_i32 b);
B_bool B_HashableD_i32D___eq__(B_HashableD_i32 wit, B_i32 a, B_i32 b);
B_bool B_HashableD_i32D___ne__(B_HashableD_i32 wit, B_i32 a, B_i32 b);
B_NoneType B_HashableD_i32D_hash(B_HashableD_i32 wit, B_i32 a, B_hasher h);

