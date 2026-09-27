struct B_i8 {
    struct B_i8G_class *$class;
    int8_t val;
};

 
B_i8 toB_i8(int8_t n);
int8_t fromB_i8(B_i8 n);

int8_t B_i8G_new(B_atom a, B_int base);
 
extern struct B_ZeroDivisionError B_i8_truediv_zero_error;
extern struct B_ZeroDivisionError B_i8_floordiv_zero_error;
extern struct B_ZeroDivisionError B_i8_mod_zero_error;

#define i8_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_i8_truediv_zero_error); (double)a/(double)b;} )
#define i8_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_i8_floordiv_zero_error);  a/b;} )
#define i8_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_i8_mod_zero_error); a%b;} )

int8_t i8_pow(int8_t a, int8_t b);
