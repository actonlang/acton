struct B_i16 {
    struct B_i16G_class *$class;
    short val;
};

 
B_i16 toB_i16(short n);
short fromB_i16(B_i16 n);

int16_t B_i16G_new(B_atom a, B_int base);
 
extern struct B_ZeroDivisionError B_i16_truediv_zero_error;
extern struct B_ZeroDivisionError B_i16_floordiv_zero_error;
extern struct B_ZeroDivisionError B_i16_mod_zero_error;

#define i16_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_i16_truediv_zero_error); (double)a/(double)b;} )
#define i16_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_i16_floordiv_zero_error);  a/b;} )
#define i16_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_i16_mod_zero_error); a%b;} )

int16_t i16_pow(int16_t a, int16_t b);
