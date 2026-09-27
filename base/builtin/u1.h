struct B_u1 {
    struct B_u1G_class *$class;
    uint8_t val;
};

B_u1 toB_u1(uint8_t n);
uint8_t fromB_u1(B_u1 n);

uint8_t B_u1G_new(B_atom a, B_int base);

extern struct B_ZeroDivisionError B_u1_truediv_zero_error;
extern struct B_ZeroDivisionError B_u1_floordiv_zero_error;
extern struct B_ZeroDivisionError B_u1_mod_zero_error;

#define u1_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_u1_truediv_zero_error); (double)a;} )
#define u1_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_u1_floordiv_zero_error);  a;} )
#define u1_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_u1_mod_zero_error); (a%b)&1;} )

uint8_t u1_pow(uint8_t a, uint8_t b);
