struct B_u16 {
    struct B_u16G_class *$class;
    unsigned short val;
};

B_u16 toB_u16(unsigned short n);
unsigned short fromB_u16(B_u16 n);

uint16_t B_u16G_new(B_atom a, B_int base);

extern struct B_ZeroDivisionError B_u16_truediv_zero_error;
extern struct B_ZeroDivisionError B_u16_floordiv_zero_error;
extern struct B_ZeroDivisionError B_u16_mod_zero_error;

#define u16_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_u16_truediv_zero_error); (double)a/(double)b;} )
#define u16_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_u16_floordiv_zero_error);  a/b;} )
#define u16_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_u16_mod_zero_error); a%b;} )

uint16_t u16_pow(uint16_t a, uint16_t b);
