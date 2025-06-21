struct B_i64 {
    struct B_i64G_class *$class;
    int64_t val;
};

 
B_i64 toB_i64(int64_t n);
int64_t fromB_i64(B_i64 n);

B_i64 B_i64G_new(B_atom a, B_int base);

// only called with e>=0.
long i64_pow(long a, long e); // used also for ndarrays

#define i64_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i64 truediv: division by zero"))); (double)a/(double)b;} )
#define i64_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i64 floordiv: division by zero")));  a/b;} )
#define i64_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i64 mod: division by zero"))); a%b;} )

/*
#define ORD_B_i64__eq__(a,b)  (a->val == b->val)
#define ORD_B_i64__ne__(a,b)  (a->val != b->val)
#define ORD_B_i64__lt__(a,b)  (a->val < b->val)
#define ORD_B_i64__le__(a,b)  (a->val <= b->val)
#define ORD_B_i64__gt__(a,b)  (a->val > b->val)
#define ORD_B_i64__ge__(a,b)  (a->val >= b->val)
*/
