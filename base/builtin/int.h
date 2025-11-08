#include <bsdnt/zz.h>

struct B_int {
    struct B_intG_class *$class;
    zz_struct val;
};

B_int zz$to$int(zz_ptr val);

long from$int(B_int n);
B_int to$int(long n);
B_int to$int2(char *str);

char *get_str(zz_ptr n);

B_int B_intG_new(B_atom a, B_int base);

B_int $gcd(B_int, B_int);
B_tuple $xgcd(B_int, B_int);

extern struct B_int B_int_strs[384];

#define ORD_B_int__eq__(a,b)  (zz_equal(&a->val,&b->val))
#define ORD_B_int__ne__(a,b)  (1-zz_equal(&a->val,&b->val))
#define ORD_B_int__lt__(a,b)  (zz_cmp(&a->val,&b->val) < 0)
#define ORD_B_int__le__(a,b)  (zz_cmp(&a->val,&b->val) <= 0)
#define ORD_B_int__gt__(a,b)  (zz_cmp(&a->val,&b->val) > 0)
#define ORD_B_int__ge__(a,b)  (zz_cmp(&a->val,&b->val) >= 0)
