struct B_i8 {
    struct B_i8G_class *$class;
    int8_t val;
};

 
B_i8 toB_i8(int8_t n);
int8_t fromB_i8(B_i8 n);

B_i8 B_i8G_new(B_atom a, B_int base);
 
#define i8_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i8 truediv: division by zero"))); (double)a/(double)b;} )
#define i8_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i8 floordiv: division by zero")));  a/b;} )
#define i8_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i8 mod: division by zero"))); a%b;} )

int8_t i8_pow(int8_t a, int8_t b);
