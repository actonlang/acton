struct B_i32 {
    struct B_i32G_class *$class;
    int val;
};

 
B_i32 toB_i32(int32_t n);
int32_t fromB_i32(B_i32 n);

int32_t B_i32G_new(B_atom a, B_int base);

extern struct B_ZeroDivisionError B_i32_truediv_zero_error;
extern struct B_ZeroDivisionError B_i32_floordiv_zero_error;
extern struct B_ZeroDivisionError B_i32_mod_zero_error;

#define i32_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_i32_truediv_zero_error); (double)a/(double)b;} )
#define i32_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_i32_floordiv_zero_error);  a/b;} )
#define i32_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_i32_mod_zero_error); a%b;} )

int32_t i32_pow(int32_t a, int32_t b);
