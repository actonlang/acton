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
B_bigint B_bigint_add(B_bigint a, B_bigint b);
B_bigint B_bigint_zero(void);
B_complex B_bigint_complex(B_bigint a);
B_bigint B_bigint_fromatom(B_atom a);
B_bigint B_bigint_mul(B_bigint a, B_bigint b);
B_bigint B_bigint_pow(B_bigint a, B_bigint b);
B_bigint B_bigint_neg(B_bigint a);
B_bigint B_bigint_pos(B_bigint a);
$WORD B_bigint_real(B_bigint a, B_Real wit2);
$WORD B_bigint_imag(B_bigint a, B_Real wit2);
$WORD B_bigint_abs(B_bigint a, B_Real wit2);
B_bigint B_bigint_conjugate(B_bigint a);
double B_bigint_float(B_bigint n);
$WORD B_bigint_trunc(B_bigint n, B_Integral wit2);
$WORD B_bigint_floor(B_bigint n, B_Integral wit2);
$WORD B_bigint_ceil(B_bigint n, B_Integral wit2);
B_bigint B_bigint_round(B_bigint n, B_int p);
$WORD B_bigint_numerator(B_bigint n, B_Integral wit2);
$WORD B_bigint_denominator(B_bigint n, B_Integral wit2);
int64_t B_bigint_int(B_bigint n);
int64_t B_bigint_index(B_bigint n);
B_tuple B_bigint_divmod(B_bigint a, B_bigint b);
B_bigint B_bigint_floordiv(B_bigint a, B_bigint b);
B_bigint B_bigint_mod(B_bigint a, B_bigint b);
B_bigint B_bigint_lshift(B_bigint a, int64_t b);
B_bigint B_bigint_rshift(B_bigint a, int64_t b);
B_bigint B_bigint_invert(B_bigint a);
B_bigint B_bigint_and(B_bigint a, B_bigint b);
B_bigint B_bigint_or(B_bigint a, B_bigint b);
B_bigint B_bigint_xor(B_bigint a, B_bigint b);
B_bigint B_bigint_sub(B_bigint a, B_bigint b);
double B_bigint_truediv(B_bigint a, B_bigint b);
bool B_bigint_eq(B_bigint a, B_bigint b);
bool B_bigint_ne(B_bigint a, B_bigint b);
bool B_bigint_lt(B_bigint a, B_bigint b);
bool B_bigint_le(B_bigint a, B_bigint b);
bool B_bigint_gt(B_bigint a, B_bigint b);
bool B_bigint_ge(B_bigint a, B_bigint b);
B_NoneType B_bigint_hash(B_bigint a, B_hasher h);
