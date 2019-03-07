/*
 * fastrand_test.c
 *
 *  Created on: 7 Mar 2019
 *      Author: aagapi
 */

#include <time.h>
#include <stdio.h>
#include "fastrand.h"

int main(int argc, char **argv) {

	unsigned int seed = time(NULL);
	fast_srand(seed);

	for(int i=0;i<100;i++)
		printf("%d\n", fastrand());
}

