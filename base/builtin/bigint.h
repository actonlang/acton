#include <bsdnt/zz.h>

struct B_bigint {
    struct B_bigintG_class *$class;
    zz_struct val;
};

// B_int zz$to$int(zz_ptr val);

long fromB_bigint(B_bigint n);
B_bigint toB_bigint(long n);
B_bigint toB_bigint2(char *str);

char *get_str(zz_ptr n);

B_bigint B_bigintG_new(B_atom a, B_int base);

B_bigint B_gcd(B_bigint, B_bigint);
B_tuple B_xgcd(B_bigint, B_bigint);

extern struct B_bigint B_bigint_strs[256];


#define ORD_B_bigint__eq__(a,b)  (zz_equal(&a->val,&b->val))
#define ORD_B_bigint__ne__(a,b)  (1-zz_equal(&a->val,&b->val))
#define ORD_B_bigint__lt__(a,b)  (zz_cmp(&a->val,&b->val) < 0)
#define ORD_B_bigint__le__(a,b)  (zz_cmp(&a->val,&b->val) <= 0)
#define ORD_B_bigint__gt__(a,b)  (zz_cmp(&a->val,&b->val) > 0)
#define ORD_B_bigint__ge__(a,b)  (zz_cmp(&a->val,&b->val) >= 0)


// Witness-free numeric protocol workers; result-conversion witnesses remain explicit.
B_bigint $bigintD_U__add__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__zero__(void);
B_complex $bigintD_U__complex__(B_bigint a);
B_bigint $bigintD_U__fromatom__(B_atom a);
B_bigint $bigintD_U__mul__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__pow__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__neg__(B_bigint a);
B_bigint $bigintD_U__pos__(B_bigint a);
$WORD $bigintD_U_real(B_bigint a, B_Real wit2);
$WORD $bigintD_U_imag(B_bigint a, B_Real wit2);
$WORD $bigintD_U__abs__(B_bigint a, B_Real wit2);
B_bigint $bigintD_U_conjugate(B_bigint a);
double $bigintD_U__float__(B_bigint n);
$WORD $bigintD_U__trunc__(B_bigint n, B_Integral wit2);
$WORD $bigintD_U__floor__(B_bigint n, B_Integral wit2);
$WORD $bigintD_U__ceil__(B_bigint n, B_Integral wit2);
B_bigint $bigintD_U__round__(B_bigint n, B_int p);
$WORD $bigintD_U_numerator(B_bigint n, B_Integral wit2);
$WORD $bigintD_U_denominator(B_bigint n, B_Integral wit2);
int64_t $bigintD_U__int__(B_bigint n);
int64_t $bigintD_U__index__(B_bigint n);
B_tuple $bigintD_U__divmod__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__floordiv__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__mod__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__lshift__(B_bigint a, int64_t b);
B_bigint $bigintD_U__rshift__(B_bigint a, int64_t b);
B_bigint $bigintD_U__invert__(B_bigint a);
B_bigint $bigintD_U__and__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__or__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__xor__(B_bigint a, B_bigint b);
B_bigint $bigintD_U__sub__(B_bigint a, B_bigint b);
double $bigintD_U__truediv__(B_bigint a, B_bigint b);
bool $bigintD_U__eq__(B_bigint a, B_bigint b);
bool $bigintD_U__ne__(B_bigint a, B_bigint b);
bool $bigintD_U__lt__(B_bigint a, B_bigint b);
bool $bigintD_U__le__(B_bigint a, B_bigint b);
bool $bigintD_U__gt__(B_bigint a, B_bigint b);
bool $bigintD_U__ge__(B_bigint a, B_bigint b);
B_NoneType $bigintD_U_hash(B_bigint a, B_hasher h);
