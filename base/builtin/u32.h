struct B_u32 {
    struct B_u32G_class *$class;
    unsigned int val;
};

B_u32 toB_u32(unsigned int n);
unsigned int fromB_u32(B_u32 n);

B_u32 B_u32G_new(B_atom a, B_int base);

#define u32_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u32 truediv: division by zero"))); (double)a/(double)b;} )
#define u32_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u32 floordiv: division by zero")));  a/b;} )
#define u32_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u32 mod: division by zero"))); a%b;} )

unsigned int u32_pow(unsigned int a, unsigned int b);
