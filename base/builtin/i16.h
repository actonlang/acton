struct B_i16 {
    struct B_i16G_class *$class;
    int16_t val;
};

 
B_i16 toB_i16(int16_t n);
int16_t fromB_i16(B_i16 n);

B_i16 B_i16G_new(B_atom a, B_int base);
 
#define i16_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 truediv: division by zero"))); (double)a/(double)b;} )
#define i16_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 floordiv: division by zero")));  a/b;} )
#define i16_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i16 mod: division by zero"))); a%b;} )

int16_t i16_pow(int16_t a, int16_t b);

#define $i16_to_i32(a)      toB_i32(fromB_i16(a))
#define $i16_to_i64(a)      toB_i64(fromB_i16(a))
#define $i16_to_int(a)      B_intG_new(a, B_None)
