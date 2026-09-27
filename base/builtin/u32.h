struct B_u32 {
    struct B_u32G_class *$class;
    unsigned int val;
};

B_u32 toB_u32(unsigned int n);
unsigned int fromB_u32(B_u32 n);

uint32_t B_u32G_new(B_atom a, B_int base);

extern struct B_ZeroDivisionError B_u32_truediv_zero_error;
extern struct B_ZeroDivisionError B_u32_floordiv_zero_error;
extern struct B_ZeroDivisionError B_u32_mod_zero_error;

#define u32_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_u32_truediv_zero_error); (double)a/(double)b;} )
#define u32_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_u32_floordiv_zero_error);  a/b;} )
#define u32_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_u32_mod_zero_error); a%b;} )

uint32_t u32_pow(uint32_t a, uint32_t b);
