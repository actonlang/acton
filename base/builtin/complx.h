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
B_complex $complexD_U__add__(B_complex a, B_complex b);
B_complex $complexD_U__zero__(void);
B_complex $complexD_U__complex__(B_complex c);
B_complex $complexD_U__mul__(B_complex a, B_complex b);
B_complex $complexD_U__fromatom__(B_atom a);
B_complex $complexD_U__pow__(B_complex a, B_complex b);
B_complex $complexD_U__neg__(B_complex c);
B_complex $complexD_U__pos__(B_complex c);
$WORD $complexD_U_real(B_complex c, B_Real wit2);
$WORD $complexD_U_imag(B_complex c, B_Real wit2);
$WORD $complexD_U__abs__(B_complex c, B_Real wit2);
B_complex $complexD_U_conjugate(B_complex c);
B_complex $complexD_U__truediv__(B_complex a, B_complex b);
B_complex $complexD_U__sub__(B_complex a, B_complex b);
bool $complexD_U__eq__(B_complex a, B_complex b);
bool $complexD_U__ne__(B_complex a, B_complex b);
B_NoneType $complexD_U_hash(B_complex a, B_hasher h);
