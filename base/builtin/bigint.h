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

B_bigint $gcd(B_bigint, B_bigint);
B_tuple $xgcd(B_bigint, B_bigint);

extern struct B_bigint B_bigint_strs[256];

/*
#define ORD_B_int__eq__(a,b)  (zz_equal(&a->val,&b->val))
#define ORD_B_int__ne__(a,b)  (1-zz_equal(&a->val,&b->val))
#define ORD_B_int__lt__(a,b)  (zz_cmp(&a->val,&b->val) < 0)
#define ORD_B_int__le__(a,b)  (zz_cmp(&a->val,&b->val) <= 0)
#define ORD_B_int__gt__(a,b)  (zz_cmp(&a->val,&b->val) > 0)
#define ORD_B_int__ge__(a,b)  (zz_cmp(&a->val,&b->val) >= 0)
*/
