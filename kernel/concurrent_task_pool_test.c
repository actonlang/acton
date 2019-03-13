/*
 * A concurrent, scalable, wait-free task pool implementation that performs well under high CAS contention
 *
 * This file contains testing functions for pools
 *
 *  Created on: 22 Jan 2019
 *      Author: aagapi
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdatomic.h>
#include <time.h>

#include "concurrent_task_pool.h"

#define NO_TASKS 10000

#define VERBOSE 0

// #define KEEP_TASKS

int main(int argc, char **argv) {

	int no_tasks = NO_TASKS, tree_height = 0, k_retries = 0;

	srand(time(NULL));

	if(argc > 1)
	{
		no_tasks = atoi(argv[1]);

		no_tasks = (no_tasks==0)?NO_TASKS:no_tasks;
	}

	if(argc > 2)
		tree_height = atoi(argv[2]);

	if(argc > 3)
		k_retries = atoi(argv[3]);

#ifdef KEEP_TASKS
	WORD tasks[no_tasks];
	WORD recovered_tasks[no_tasks];
#else
	WORD recovered_task;
#endif
	int status = 0;
	clock_t start_put, end_put, start_get, end_get;

#ifdef KEEP_TASKS
	for(long i=0;i<no_tasks;i++)
	{
		tasks[i]= (WORD) (i+1);
	}
#endif

	concurrent_pool * pool = (tree_height>0)?allocate_pool_with_tree_height(tree_height):allocate_pool();

	if(k_retries > 0)
		set_no_trials(pool, k_retries);

	start_put = clock() ;

	for(long i=0;i<no_tasks;i++)
	{
#ifdef KEEP_TASKS
		put(tasks[i], pool);
#else
		put((WORD) 1, pool);
#endif
	}

	end_put = clock() ;

	start_get = clock() ;

	for(int i=0;i<no_tasks;i++)
	{
#ifdef KEEP_TASKS
		status = get(recovered_tasks + i, pool);
#else
		status = get(&recovered_task, pool);
#endif

		if(status!=0)
			printf("ERROR: No tasks left on get() attempt %d\n", i+1);
	}

	end_get = clock();

	printf("no_tasks=%d, tree_height=%d, k_retries=%d, total_seconds_put=%f, total_seconds_get=%f, put_tpt=%f, put_latency_ns=%f, get_tpt=%f, get_latency_ns=%f\n",
			no_tasks, (tree_height>0)?tree_height:DEFAULT_TREE_HEIGHT, (k_retries>0)?k_retries:DEFAULT_K_NO_TRIALS,
			(end_put-start_put)/(double)CLOCKS_PER_SEC,
			(end_get-start_get)/(double)CLOCKS_PER_SEC,
			no_tasks / ((end_put-start_put)/(double)CLOCKS_PER_SEC),
			(((end_put-start_put)/((double)CLOCKS_PER_SEC / 1000000000))) / no_tasks,
			no_tasks / ((end_get-start_get)/(double)CLOCKS_PER_SEC),
			(((end_get-start_get)/((double)CLOCKS_PER_SEC / 1000000000))) / no_tasks);

	free_pool(pool);
}


