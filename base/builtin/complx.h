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
