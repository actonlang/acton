struct B_u8 {
    struct B_u8G_class *$class;
    uint8_t val;
};

B_u8 toB_u8(uint8_t n);
uint8_t fromB_u8(B_u8 n);

B_u8 B_u8G_new(B_atom a, B_int base);

#define u8_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u8 truediv: division by zero"))); (double)a/(double)b;} )
#define u8_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u8 floordiv: division by zero")));  a/b;} )
#define u8_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u8 mod: division by zero"))); a%b;} )

uint8_t u8_pow(uint8_t a, uint8_t b);
