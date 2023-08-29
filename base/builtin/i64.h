struct B_i64 {
    struct B_i64G_class *$class;
    long val;
};

 
B_i64 toB_i64(long n);
long fromB_i64(B_i64 n);

B_i64 B_i64G_new(B_atom a);

// only called with e>=0.
long longpow(long a, long e); // used also for ndarrays

#define ORD_B_i64__eq__(a,b)  (a->val == b->val)
#define ORD_B_i64__ne__(a,b)  (a->val != b->val)
#define ORD_B_i64__lt__(a,b)  (a->val < b->val)
#define ORD_B_i64__le__(a,b)  (a->val <= b->val)
#define ORD_B_i64__gt__(a,b)  (a->val > b->val)
#define ORD_B_i64__ge__(a,b)  (a->val >= b->val)
