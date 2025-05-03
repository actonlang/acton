struct B_u64 {
    struct B_u64G_class *$class;
    uint64_t val;
};

B_u64 toB_u64(uint64_t n);
uint64_t fromB_u64(B_u64 n);

B_u64 B_u64G_new(B_atom a, B_int base);

#define u64_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u64 truediv: division by zero"))); (double)a/(double)b;} )
#define u64_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u64 floordiv: division by zero")));  a/b;} )
#define u64_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u64 mod: division by zero"))); a%b;} )

uint64_t u64_pow(uint64_t a, uint64_t b);
