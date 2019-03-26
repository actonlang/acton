
#ifndef FASTRAND_H_
#define FASTRAND_H_

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
