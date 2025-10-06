struct B_int {
    struct B_intG_class *$class;
    int64_t val;
};

 
B_int toB_int(int64_t n);
int64_t fromB_int(B_int n);

B_int B_intG_new(B_atom a, B_int base);

// only called with e>=0.
long int_pow(long a, long e); // used also for ndarrays

#define int_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("int truediv: division by zero"))); (double)a/(double)b;} )
#define int_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("int floordiv: division by zero")));  a/b;} )
#define int_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("int mod: division by zero"))); a%b;} )
