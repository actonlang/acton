
#include <time.h>

#ifndef FASTRAND_H_
#define FASTRAND_H_

#if( defined _POSIX_THREADS && _POSIX_TIMERS >= 0 && _POSIX_MONOTONIC_CLOCK >= 0 )
#define GET_RANDSEED(seedptr, rand)                                                                             \
{                                                                                                           \
  struct timespec tp;                                                                                       \
  clock_gettime( CLOCK_MONOTONIC_RAW, &tp );                                                                \
  *(seedptr) = (unsigned int) ((tp.tv_nsec & 0xFFFFFFFF00000000) + (tp.tv_nsec & 0x00000000FFFFFFFF) + rand) ; \
}
#else
#define GET_RANDSEED(seedptr, rand)                                                                             \
{                                                                                                           \
  int64_t now = time(NULL);                                                                                         \
  *(seedptr) = (unsigned int) ((now & 0xFFFFFFFF00000000) + (now & 0x00000000FFFFFFFF) + rand) ; \
}
#endif


#define FASTRAND(seedptr, value)                \
{   *seedptr = (214013*(*seedptr)+2531011); \
    value = ((*seedptr)>>16)&0x7FFF;            \
}

static unsigned int g_seed;

inline static void fast_srand(int seed)
{
    g_seed = seed;
}

inline static int fastrand()
{
    g_seed = (214013*g_seed+2531011);

    return (g_seed>>16)&0x7FFF;
}


#endif /* FASTRAND_H_ */
