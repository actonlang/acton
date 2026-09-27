struct B_u64 {
    struct B_u64G_class *$class;
    uint64_t val;
};

B_u64 toB_u64(uint64_t n);
uint64_t fromB_u64(B_u64 n);

uint64_t B_u64G_new(B_atom a, B_int base);

extern struct B_ZeroDivisionError B_u64_truediv_zero_error;
extern struct B_ZeroDivisionError B_u64_floordiv_zero_error;
extern struct B_ZeroDivisionError B_u64_mod_zero_error;

#define u64_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_u64_truediv_zero_error); (double)a/(double)b;} )
#define u64_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_u64_floordiv_zero_error);  a/b;} )
#define u64_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_u64_mod_zero_error); a%b;} )

uint64_t u64_pow(uint64_t a, uint64_t b);
