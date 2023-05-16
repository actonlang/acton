#include <bsdnt/zz.h>

struct B_int {
    struct B_intG_class *$class;
    zz_struct val;
};
extern GC_word B_intD_gcbm[GC_BITMAP_SIZE(struct B_int)];

B_int zz$to$int(zz_ptr val);

long from$int(B_int n);
B_int to$int(long n);
B_int to$int2(char *str);

B_int B_intG_new(B_atom a);

B_int $gcd(B_int, B_int);
B_tuple $xgcd(B_int, B_int);

extern struct B_int B_int_strs[256];

#define ORD_INT__eq__(a,b)  (zz_equal(&a->val,&b->val))
#define ORD_INT__ne__(a,b)  (1-zz_equal(&a->val,&b->val))
#define ORD_INT__lt__(a,b)  (zz_cmp(&a->val,&b->val) < 0)
#define ORD_INT__le__(a,b)  (zz_cmp(&a->val,&b->val) <= 0)
#define ORD_INT__gt__(a,b)  (zz_cmp(&a->val,&b->val) > 0)
#define ORD_INT__ge__(a,b)  (zz_cmp(&a->val,&b->val) >= 0)
