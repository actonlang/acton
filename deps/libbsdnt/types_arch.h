#include <inttypes.h>
#include <limits.h>

// Acton's bigint representation and algorithms use 64-bit limbs on every
// target, including Windows where the default BSDNT long is only 32 bits.
#define HAVE_ARCH_types

typedef uint64_t word_t;
typedef int64_t sword_t;
typedef int64_t len_t;
typedef int64_t bits_t;
typedef unsigned int dword_t __attribute__((mode(TI)));

#define LEN_MAX INT64_MAX
#define BITS_MAX INT64_MAX
#define WORD_BITS 64
#define WORD(x) UINT64_C(x)

// The default intrinsics take long, which is only 32 bits on Windows.
#define HAVE_ARCH_INTRINSICS
#define high_zero_bits __builtin_clzll
#define low_zero_bits __builtin_ctzll
#define popcount_bits __builtin_popcountll

// BSDNT appends the conversion specifier to WORD_FMT.
#if UINT64_MAX == ULONG_MAX
#define WORD_FMT "%l"
#else
#define WORD_FMT "%ll"
#endif
#define LEN_FMT "%" PRId64
#define BITS_FMT "%" PRId64
