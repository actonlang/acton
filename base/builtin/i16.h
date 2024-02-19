struct B_i16 {
    struct B_i16G_class *$class;
    short val;
};

 
B_i16 toB_i16(short n);
short fromB_i16(B_i16 n);

B_i16 B_i16G_new(B_atom a, B_int base);
 
#define i16_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 truediv: division by zero"))); (double)a/(double)b;} )
#define i16_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 floordiv: division by zero")));  a/b;} )
#define i16_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 mod: division by zero"))); a%b;} )

short i16_pow(short a, short b);
