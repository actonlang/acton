struct B_i32 {
    struct B_i32G_class *$class;
    int val;
};

 
B_i32 toB_i32(int n);
int fromB_i32(B_i32 n);

B_i32 B_i32G_new(B_atom a, B_int base);

#define i32_DIV(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 truediv: division by zero"))); (double)a/(double)b;} )
#define i32_FLOORDIV(a,b)  ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 floordiv: division by zero")));  a/b;} )
#define i32_MOD(a,b)       ( {if (b==0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str("i32 mod: division by zero"))); a%b;} )

int i32_pow(int a, int b);
