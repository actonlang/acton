struct B_u16 {
    struct B_u16G_class *$class;
    uint16_t val;
};

B_u16 toB_u16(uint16_t n);
uint16_t fromB_u16(B_u16 n);

B_u16 B_u16G_new(B_atom a, B_int base);

#define u16_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 truediv: division by zero"))); (double)a/(double)b;} )
#define u16_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 floordiv: division by zero")));  a/b;} )
#define u16_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 mod: division by zero"))); a%b;} )

uint16_t u16_pow(uint16_t a, uint16_t b);

#define $u16_to_u32(a)      toB_u32(fromB_u16(a))
#define $u16_to_u64(a)      toB_u64(fromB_u16(a))

#define $u16_to_i32(a)      toB_i32(fromB_u16(a))
#define $u16_to_i64(a)      toB_i64(fromB_u16(a))
#define $u16_to_int(a)      B_intG_new(a, B_None)
