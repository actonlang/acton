#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "hash.h"

/* 
Hashing of primitive types as in 

https://github.com/python/cpython/blob/master/Python/pyhash.c

One of the design criteria for the Python hash algorithms is that e.g. 15 and 15.0 should hash to the same value.
This is not relevant in the typed context of Acton, but we still stick to this algorithm.

In Python, hash values may be negative, but -1 is used to signal an error. (When can hash computation fail in Acton?)
However, CPython uses unsigned type size_t for hash values.


*/

#ifdef __APPLE__
#  include <libkern/OSByteOrder.h>
#elif defined(HAVE_LE64TOH) && defined(HAVE_ENDIAN_H)
#  include <endian.h>
#elif defined(HAVE_LE64TOH) && defined(HAVE_SYS_ENDIAN_H)
#  include <sys/endian.h>
#endif

#define _PyHASH_BITS 61
#define _PyHASH_MODULUS (((long)1 << _PyHASH_BITS) - 1)
#define _PyHASH_INF 314159
#define _PyHASH_NAN 0
#define _PyHASH_MULTIPLIER 1000003UL  /* 0xf4243 */
#define SIZEOF_PY_UHASH_T 8

/* 
BvS 191002: For the moment, stick to little endian. In CPython, this is set in pyport.h based on info from the configure script.
*/

#define PY_BIG_ENDIAN 0
#define PY_LITTLE_ENDIAN 1

long $int_hash ($int n) {
  long u = *n, sign=1;
  if (u<0)  {
    sign=-1;
    u = -u;
  }
  long h = u % _PyHASH_MODULUS * sign;
  if (h == (long)-1)
    h = (long)-2;
  return h;
}


long $float_hash($float v) {
    int e, sign;
    double m;
    long x, y;
    double d = *(double*)v;
    
    if (!isfinite(d)) {
        if (isinf(d))
            return d > 0 ? _PyHASH_INF : -_PyHASH_INF;
        else
            return _PyHASH_NAN;
    }
    
    m = frexp(d, &e);

    sign = 1;
    if (m < 0) {
        sign = -1;
        m = -m;
    }

    /* process 28 bits at a time;  this should work well both for binary
       and hexadecimal floating point. */
    x = 0;
    while (m) {
        x = ((x << 28) & _PyHASH_MODULUS) | x >> (_PyHASH_BITS - 28);
        m *= 268435456.0;  /* 2**28 */
        e -= 28;
        y = (long)m;  /* pull out integer part */
        m -= y;
        x += y;
        if (x >= _PyHASH_MODULUS)
            x -= _PyHASH_MODULUS;
    }

    /* adjust for the exponent;  first reduce it modulo _PyHASH_BITS */
    e = e >= 0 ? e % _PyHASH_BITS : _PyHASH_BITS-1-((-1-e) % _PyHASH_BITS);
    x = ((x << e) & _PyHASH_MODULUS) | x >> (_PyHASH_BITS - e);

    x = x * sign;
    if (x == (long)-1)
        x = (long)-2;
    return x;
}

/*
size_t pointer_hash(void *p) {
    size_t x;
    size_t y = (size_t)p;
    // bottom 3 or 4 bits are likely to be 0; rotate y by 4 to avoid
    //  excessive hash collisions for dicts and sets 
    y = (y >> 4) | (y << 60);
    x = (size_t)y;
    if (x == -1)
        x = -2;
    return x;
}
*/
/* hash secret
 *
 * memory layout on 64 bit systems
 *   cccccccc cccccccc cccccccc  uc -- unsigned char[24]
 *   pppppppp ssssssss ........  fnv -- two Py_hash_t
 *   k0k0k0k0 k1k1k1k1 ........  siphash -- two uint64_t
 *   ........ ........ ssssssss  djbx33a -- 16 bytes padding + one Py_hash_t
 *   ........ ........ eeeeeeee  pyexpat XML hash salt
 */


typedef union {
    // ensure 24 bytes 
    unsigned char uc[24];
    // two uint64 for SipHash24 
    struct {
        uint64_t k0;
        uint64_t k1;
    } siphash;
    struct {
        unsigned char padding[16];
        long hashsalt;
    } expat;
} _Py_HashSecret_t;


_Py_HashSecret_t _Py_HashSecret = {{0}};

#include "csiphash.c"

static long pysiphash(void *src, long src_sz) {
    return (long)siphash24(
        _le64toh(_Py_HashSecret.siphash.k0), _le64toh(_Py_HashSecret.siphash.k1),
        src, src_sz);
}

long $string_hash(void *src, int len) {
    long x;
    /*
      We make the hash of the empty string be 0, rather than using
      (prefix ^ suffix), since this slightly obfuscates the hash secret
    */
    if (len == 0) {
        return 0;
    }
         x = pysiphash(src, len);

    if (x == -1)
        return -2;
    return x;
}

/* Hash for tuples. This is a slightly simplified version of the xxHash
   non-cryptographic hash:
   - we do not use any parallellism, there is only 1 accumulator.
   - we drop the final mixing since this is just a permutation of the
     output space: it does not help against collisions.
   - at the end, we mangle the length with a single constant.
   For the xxHash specification, see
   https://github.com/Cyan4973/xxHash/blob/master/doc/xxhash_spec.md

   Below are the official constants from the xxHash specification. Optimizing
   compilers should emit a single "rotate" instruction for the
   _PyHASH_XXROTATE() expansion. If that doesn't happen for some important
   platform, the macro could be changed to expand to a platform-specific rotate
   spelling instead.

#define _PyHASH_XXPRIME_1 ((size_t)11400714785074694791ULL)
#define _PyHASH_XXPRIME_2 ((size_t)14029467366897019727ULL)
#define _PyHASH_XXPRIME_5 ((size_t)2870177450012600261ULL)
#define _PyHASH_XXROTATE(x) ((x << 31) | (x >> 33))  // Rotate left 31 bits 
 
 Tests have shown that it's not worth to cache the hash value, see
   https://bugs.python.org/issue9685 

size_t tuple_hash(tuple_t v) {
    ssize_t i, len = v->length;

    size_t acc = _PyHASH_XXPRIME_5;
    for (i = 0; i < len; i++) {
      size_t lane = double_hash(v->item[i]);
      printf("tuple element hash is %ld\n",lane);
      if (lane == (size_t)-1) {
        return -1;
      }
      acc += lane * _PyHASH_XXPRIME_2;
      acc = _PyHASH_XXROTATE(acc);
      acc *= _PyHASH_XXPRIME_1;
    }

    //Add input length, mangled to keep the historical value of hash(()). 
    acc += len ^ (_PyHASH_XXPRIME_5 ^ 3527539UL);

    if (acc == (size_t)-1) {
        return 1546275796;
    }
    return acc;
}


 "Old" hash algorithm for tuples; used in Python versions <= 3.7. 
    From 3.8 the xxHash-based algorithm above is used.

size_t tuple_hash($WORD ht,$WORD v) {
  
  tuple_t hashes = (tuple_t)ht;
  tuple_t tup = (tuple_t)v;
  size_t x = 0x345678UL;  // Unsigned for defined overflow behavior. 
  size_t y;
  size_t len = tup->length;
  size_t mult = _PyHASH_MULTIPLIER;
  $WORD *p = tup->item;
  $WORD *h = hashes->item;
  
  for (int i = 0; i<len; i++) {
    size_t(*hfn)($WORD) = (size_t(*)($WORD))(h[i]);
    y = hfn(p[i]);
    //    printf("hash of component is %ld\n",y);
    x = (x ^ y) * mult;
    // the cast might truncate len; that doesn't change hash stability 
    mult += (size_t)(82520UL + 2*(len-i-1));
  }
  x += 97531UL;
  if (x == (size_t)-1)
    x = -2;
  return x;
}


static struct Hashable_struct long_h = {long_eq, long_hash};

Hashable long_Hashable = &long_h;

static struct Hashable_struct double_h = {double_eq, double_hash};

Hashable double_Hashable =  &double_h;

int unboxed_eq($WORD a, $WORD b) {
  return a == b;
}

size_t unboxed_hash($WORD v) {
  long u = (long)v, sign=1;
  if (u<0)  {
    sign=-1;
    u = -u;
  }
  size_t h = u % _PyHASH_MODULUS * sign;
  if (h == (size_t)-1)
    h = (size_t)-2;
  return h;
}

static struct Hashable_struct unboxed_h = {unboxed_eq, unboxed_hash};

Hashable unboxed_Hashable = &unboxed_h;
*/
