struct B_u1 {
    struct B_u1G_class *$class;
    uint8_t val;
};

B_u1 toB_u1(uint8_t n);
uint8_t fromB_u1(B_u1 n);

B_u1 B_u1G_new(B_atom a, B_int base);

#define u1_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u1 truediv: division by zero"))); (double)a;} )
#define u1_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u1 floordiv: division by zero")));  a;} )
#define u1_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u1 mod: division by zero"))); a;} )

uint8_t u1_pow(uint8_t a, uint8_t b);
