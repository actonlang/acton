struct B_u16 {
    struct B_u16G_class *$class;
    unsigned short val;
};

B_u16 toB_u16(unsigned short n);
unsigned short fromB_u16(B_u16 n);

B_u16 B_u16G_new(B_atom a, B_int base);

#define u16_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 truediv: division by zero"))); (double)a/(double)b;} )
#define u16_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 floordiv: division by zero")));  a/b;} )
#define u16_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("u16 mod: division by zero"))); a%b;} )

unsigned short u16_pow(unsigned short a, unsigned short b);
