/*
 * This file is deliberately named complx.h to avoid collisions under gcc or
 * clang. Details are murky, is it with the standard complex.h? (Path should be
 * different, no?) Björn should know the details..
 */
#include <complex.h>

struct B_complex {
    struct B_complexG_class *$class;
    complex double val;
};

static inline void B_complex_hash_parts(complex double value, double parts[2]) {
    parts[0] = creal(value);
    parts[1] = cimag(value);
    if (parts[0] == 0.0)
        parts[0] = 0.0;
    if (parts[1] == 0.0)
        parts[1] = 0.0;
}

B_complex toB_complex(complex double c);

// Witness-free numeric protocol workers; result-conversion witnesses remain explicit.
B_complex B_complex_add(B_complex a, B_complex b);
B_complex B_complex_zero(void);
B_complex B_complex_complex(B_complex c);
B_complex B_complex_mul(B_complex a, B_complex b);
B_complex B_complex_fromatom(B_atom a);
B_complex B_complex_pow(B_complex a, B_complex b);
B_complex B_complex_neg(B_complex c);
B_complex B_complex_pos(B_complex c);
$WORD B_complex_real(B_complex c, B_Real wit2);
$WORD B_complex_imag(B_complex c, B_Real wit2);
$WORD B_complex_abs(B_complex c, B_Real wit2);
B_complex B_complex_conjugate(B_complex c);
B_complex B_complex_truediv(B_complex a, B_complex b);
B_complex B_complex_sub(B_complex a, B_complex b);
bool B_complex_eq(B_complex a, B_complex b);
bool B_complex_ne(B_complex a, B_complex b);
B_NoneType B_complex_hash(B_complex a, B_hasher h);
