struct B_int {
    struct B_intG_class *$class;
    int64_t val;
};

 
B_int toB_int(int64_t n);
B_int to$int(int64_t n);
int64_t fromB_int(B_int n);

int64_t B_intG_new(B_atom a, B_int base);

// only called with e>=0.
int64_t int_pow(int64_t a, int64_t e); // used also for ndarrays

extern struct B_ZeroDivisionError B_int_truediv_zero_error;
extern struct B_ZeroDivisionError B_int_floordiv_zero_error;
extern struct B_ZeroDivisionError B_int_mod_zero_error;

#define int_DIV(a,b)       ( {if (b==0) RAISE_EXC(&B_int_truediv_zero_error); (double)a/(double)b;} )
#define int_FLOORDIV(a,b)  ( {if (b==0) RAISE_EXC(&B_int_floordiv_zero_error);  a/b;} )
#define int_MOD(a,b)       ( {if (b==0) RAISE_EXC(&B_int_mod_zero_error); a%b;} )

// Witness-free formatting and truth workers.
bool B_int_bool(B_int self);
B_str B_int_str(B_int self);
B_str B_int_repr(B_int self);
