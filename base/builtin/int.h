struct B_int {
    struct B_intG_class *$class;
    int64_t val;
};

 
B_int toB_int(int64_t n);
B_int to$int(int64_t n);
int64_t fromB_int(B_int n);

int64_t B_intG_new(B_atom a, B_int base);

// only called with e>=0.
long int_pow(long a, long e); // used also for ndarrays

extern struct B_ZeroDivisionError B_int_truediv_zero_error;
extern struct B_ZeroDivisionError B_int_floordiv_zero_error;
extern struct B_ZeroDivisionError B_int_mod_zero_error;

#define int_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_int_truediv_zero_error); (double)a/(double)b;} )
#define int_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_int_floordiv_zero_error);  a/b;} )
#define int_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_int_mod_zero_error); a%b;} )
