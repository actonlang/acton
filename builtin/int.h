#include <bsdnt/zz.h>

struct B_int {
    struct B_intG_class *$class;
    zz_struct val;
};

B_int zz$to$int(zz_ptr val);

long from$int(B_int n);
B_int to$int(long n);
B_int to$int2(char *str);

B_int B_intG_new(B_atom a);

B_int $gcd(B_int, B_int);
B_tuple $xgcd(B_int, B_int);

extern struct B_int B_int_strs[256];
