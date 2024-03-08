struct B_i32 {
    struct B_i32G_class *$class;
    int32_t val;
};

 
B_i32 toB_i32(int32_t n);
int32_t fromB_i32(B_i32 n);

B_i32 B_i32G_new(B_atom a, B_int base);

#define i32_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 truediv: division by zero"))); (double)a/(double)b;} )
#define i32_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 floordiv: division by zero")));  a/b;} )
#define i32_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 mod: division by zero"))); a%b;} )

int32_t i32_pow(int32_t a, int32_t b);

#define $i32_to_i64(a)      toB_i64(fromB_i32(a))
#define $i32_to_int(a)      B_intG_new(a, B_None)
