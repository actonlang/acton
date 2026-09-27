struct B_u8 {
    struct B_u8G_class *$class;
    uint8_t val;
};

B_u8 toB_u8(uint8_t n);
uint8_t fromB_u8(B_u8 n);

uint8_t B_u8G_new(B_atom a, B_int base);

extern struct B_ZeroDivisionError B_u8_truediv_zero_error;
extern struct B_ZeroDivisionError B_u8_floordiv_zero_error;
extern struct B_ZeroDivisionError B_u8_mod_zero_error;

#define u8_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_u8_truediv_zero_error); (double)a/(double)b;} )
#define u8_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_u8_floordiv_zero_error);  a/b;} )
#define u8_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_u8_mod_zero_error); a%b;} )

uint8_t u8_pow(uint8_t a, uint8_t b);
