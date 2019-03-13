/*
 * A concurrent, scalable, wait-free task pool implementation that performs well under high CAS contention
 *
 * This file contains testing functions for pools
 *
 *  Created on: 22 Jan 2019
 *      Author: aagapi
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdatomic.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <sched.h>

#include "fastrand.h"

#include "concurrent_task_pool.h"

// For comparative benchmarking:
#include <liblfds711.h>

#define NO_TASKS 10000
#define NO_THREADS 1

#define RING_BUFFER_SIZE 1000

// #define VERBOSE

// #define KEEP_TASKS

#define BENCHMARK_TASKPOOL 0
#define BENCHMARK_LIBLFDS_QUEUE 1
#define BENCHMARK_LIBLFDS_RINGBUFFER 2


int no_tasks = NO_TASKS, no_threads = NO_THREADS, tree_height = DEFAULT_TREE_HEIGHT, k_retries = DEFAULT_K_NO_TRIALS;
long num_cpu, ring_buffer_size = RING_BUFFER_SIZE;
int benchmark_target = BENCHMARK_TASKPOOL;
int verbose = 0;
long * put_errs, * get_errs;


// Taskpool-related global vars:

concurrent_pool * pool;

#ifdef KEEP_TASKS
	WORD* tasks;
	WORD* recovered_tasks;
#else
	WORD recovered_task;
#endif

// Liblfds queue-related global vars:

struct lfds711_queue_umm_state qs;
struct lfds711_queue_umm_element qe_dummy;

// Liblfds ringbuffer-related global vars:

struct lfds711_ringbuffer_state rs;
struct lfds711_ringbuffer_element * re;

void *thread_main_put(void *arg)
{
    int thread_id = (int) arg;
    int status=0;
	clock_t start_put, end_put;
	struct lfds711_queue_umm_element qe;
	struct lfds711_ringbuffer_element elem;
	enum lfds711_misc_flag overwrite_occurred_flag;

	LFDS711_MISC_MAKE_VALID_ON_CURRENT_LOGICAL_CORE_INITS_COMPLETED_BEFORE_NOW_ON_ANY_OTHER_LOGICAL_CORE;

	if(verbose)
	{
		start_put = clock();

		printf("[thread %d put] start_time=%ld, no_tasks=%d\n", thread_id, start_put, no_tasks/no_threads);
	}

	for(long i=0;i<no_tasks/no_threads;i++)
	{
		if(benchmark_target == BENCHMARK_TASKPOOL)
		{
#ifdef KEEP_TASKS
			status = put(tasks[thread_id*(no_tasks/no_threads)+i], pool);
#else
			status = put((WORD) (thread_id * (no_tasks/no_threads)) + i, pool);
#endif
		}
		else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
		{
			lfds711_queue_umm_enqueue(&qs, &qe);
		}
		else if(benchmark_target == BENCHMARK_LIBLFDS_RINGBUFFER)
		{
			// (void *) (lfds711_pal_uint_t) 1

			lfds711_ringbuffer_write(&rs, NULL, NULL, &overwrite_occurred_flag, NULL, NULL);

		    if(overwrite_occurred_flag == LFDS711_MISC_FLAG_RAISED )
		    		status=1;
		}

		if(status!=0)
		{
			put_errs[thread_id]++;

			if(verbose)
				printf("ERROR (thread %d - put): put() attempt %ld failed!\n", thread_id, i+1);
		}
	}

	if(verbose)
	{
		end_put = clock() ;

		printf("[thread %d put] no_tasks=%d, total_seconds_put=%f, put_tpt=%f, put_latency_ns=%f\n",
				thread_id, no_tasks/no_threads,
				(end_put-start_put)/(double)CLOCKS_PER_SEC,
				(no_tasks/no_threads) / ((end_put-start_put)/(double)CLOCKS_PER_SEC),
				(((end_put-start_put)/((double)CLOCKS_PER_SEC / 1000000000))) / (no_tasks/no_threads));
	}

    return NULL;
}

void *thread_main_get(void *arg)
{
    int thread_id = (int) arg;
    int status=0;
	clock_t start_get, end_get;
	struct lfds711_queue_umm_element qe;
	struct lfds711_queue_umm_element * qep = &qe;
	void *buffer_read_element;

	LFDS711_MISC_MAKE_VALID_ON_CURRENT_LOGICAL_CORE_INITS_COMPLETED_BEFORE_NOW_ON_ANY_OTHER_LOGICAL_CORE;

	if(verbose)
	{
		start_get = clock() ;

		printf("[thread %d get] start_time=%ld, no_tasks=%d\n", thread_id, start_get, no_tasks/no_threads);
	}

	for(long i=0;i<no_tasks/no_threads;i++)
	{
		if(benchmark_target == BENCHMARK_TASKPOOL)
		{
#ifdef KEEP_TASKS
			status = get(recovered_tasks + thread_id*(no_tasks/no_threads) + i, pool);
#else
			status = get(&recovered_task, pool);
#endif
		}
		else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
		{
			status = !lfds711_queue_umm_dequeue(&qs, &qep);
		}
		else if(benchmark_target == BENCHMARK_LIBLFDS_RINGBUFFER)
		{
			lfds711_ringbuffer_read(&rs, &buffer_read_element, NULL);
		}

		if(status!=0)
		{
			get_errs[thread_id]++;

			if(verbose)
				printf("ERROR (thread %d - get): No tasks left on get() attempt %ld\n", thread_id, i+1);
		}
	}

	if(verbose)
	{
		end_get = clock() ;

		printf("[thread %d get] no_tasks=%d, total_seconds_get=%f, get_tpt=%f, get_latency_ns=%f\n",
				thread_id, no_tasks/no_threads,
				(end_get-start_get)/(double)CLOCKS_PER_SEC,
				(no_tasks/no_threads) / ((end_get-start_get)/(double)CLOCKS_PER_SEC),
				(((end_get-start_get)/((double)CLOCKS_PER_SEC / 1000000000))) / (no_tasks/no_threads));
	}

    return NULL;
}


int main(int argc, char **argv) {

	int status = 0, opt, n;
	long total_put_errs = 0, total_get_errs = 0;
	clock_t start_put, end_put, start_get, end_get;

	unsigned int seed = time(NULL);
	srand(seed);
	fast_srand(seed);

    while ((opt = getopt(argc, argv, "t:h:k:T:B:R:v")) != -1)
    {
        switch (opt) {
            case 't':
            {
                n = atoi(optarg);
                no_tasks = (n>0)?n:NO_TASKS;
                break;
            }
            case 'h':
            {
                n = atoi(optarg);
                tree_height = (n>0)?n:DEFAULT_TREE_HEIGHT;
                break;
            }
            case 'k':
            {
                n = atoi(optarg);
                k_retries = (n>0)?n:DEFAULT_K_NO_TRIALS;
                break;
            }
            case 'T':
            {
                n = atoi(optarg);
                no_threads = (n>0)?n:NO_THREADS;
                break;
            }
            case 'B':
            {
                if(optarg[0] == 'T')
                		benchmark_target = BENCHMARK_TASKPOOL;
                else if(optarg[0] == 'Q')
                		benchmark_target = BENCHMARK_LIBLFDS_QUEUE;
                else if(optarg[0] == 'R')
                		benchmark_target = BENCHMARK_LIBLFDS_RINGBUFFER;

                break;
            }
            case 'R':
            {
                n = atoi(optarg);
                ring_buffer_size = (n>0)?n:RING_BUFFER_SIZE;
                break;
            }
            case 'v':
            {
            		verbose = 1;
            		break;
            }
        }
    }

    num_cpu = sysconf(_SC_NPROCESSORS_ONLN);

// Detect if we have hyperthreading:

    u_int32_t registers[4];

    __asm__ __volatile__ ("cpuid " :
                          "=a" (registers[0]),
                          "=b" (registers[1]),
                          "=c" (registers[2]),
                          "=d" (registers[3])
                          : "a" (1), "c" (0));

    unsigned CPUFeatureSet = registers[3];
    unsigned char hyperthreading = CPUFeatureSet & (1 << 28);

    hyperthreading=0; // Assume no hyperthreading

	if(verbose)
		printf("Detected num_cpu=%ld, hyperthreading=%d\n", num_cpu, hyperthreading);

#ifdef KEEP_TASKS
	tasks = (WORD*) malloc(no_tasks * sizeof(WORD));
	recovered_tasks(WORD*) malloc(no_tasks * sizeof(WORD));
	for(long i=0;i<no_tasks;i++)
	{
		tasks[i]= (WORD) (i+1);
	}
#else
	WORD recovered_task;
#endif

	// Taskpool init:
	if(benchmark_target == BENCHMARK_TASKPOOL)
	{
		pool = allocate_pool_with_tree_height(tree_height);
		set_no_trials(pool, k_retries);
	}
	// Liblfds queue init:
	else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
	{
		lfds711_queue_umm_init_valid_on_current_logical_core(&qs, &qe_dummy, NULL);
	}
	else if(benchmark_target == BENCHMARK_LIBLFDS_RINGBUFFER)
	{
		re = (struct lfds711_ringbuffer_element *) malloc(sizeof(struct lfds711_ringbuffer_element) * ((ring_buffer_size) + 1));

		lfds711_ringbuffer_init_valid_on_current_logical_core(&rs, re, (ring_buffer_size) + 1, NULL);
	}

    pthread_t threads_put[no_threads];
    pthread_t threads_get[no_threads];
    pthread_attr_t attrs[no_threads];
    cpu_set_t cpu_set[no_threads];
    for(int th_id = 0; th_id < no_threads; ++th_id)
    {
        pthread_attr_init(&attrs[th_id]);
        CPU_ZERO(&cpu_set[th_id]);

        int core_id = (th_id % num_cpu) * (hyperthreading+1);

        if(verbose)
        		printf("Setting thread affinity of thread %d to core %d\n", th_id, core_id);

        CPU_SET(core_id, &cpu_set[th_id]);
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__)
        pthread_attr_setaffinity_np(&attrs[th_id], sizeof(cpu_set_t), &cpu_set[th_id]);
#else
        printf("\x1b[41;1mSetting thread affinity is not implemented for your OS\x1b[m\n");
        // __unix__
#endif
    }

    put_errs = (long *) malloc(no_threads * sizeof(long));
    get_errs = (long *) malloc(no_threads * sizeof(long));


	int last_block_start = pool->producer_tree->node_id;

    	start_put = clock() ;

    	for(int th_id = 0; th_id < no_threads; ++th_id)
    {
        pthread_create(&threads_put[th_id], &attrs[th_id], thread_main_put, (void *) th_id);
    }

    for(int th_id = 0; th_id < no_threads; ++th_id)
    {
        pthread_join(threads_put[th_id], NULL);
		if(verbose)
			printf("[%d put] exited\n", th_id);
    }

	end_put = clock() ;

	start_get = clock() ;

	for(int th_id = 0; th_id < no_threads; ++th_id)
	{
		pthread_create(&threads_get[th_id], &attrs[th_id], thread_main_get, (void *) th_id);
	}

	for(int th_id = 0; th_id < no_threads; ++th_id)
	{
		pthread_join(threads_get[th_id], NULL);
		if(verbose)
			printf("[%d get] exited\n", th_id);
	}

	end_get = clock();

	for(int th_id = 0; th_id < no_threads; ++th_id)
	{
		total_put_errs += put_errs[th_id];
		total_get_errs += get_errs[th_id];
	}

	int last_block_end = pool->producer_tree->node_id;

	printf("data_struct=%s, no_tasks=%d, no_threads=%d, tree_height=%d, k_retries=%d, ring_buffer_size=%ld, total_seconds_put=%f, total_seconds_get=%f, put_tpt=%f, put_latency_ns=%f, get_tpt=%f, get_latency_ns=%f, put_errs=%ld, get_errs=%ld, prealloc_blocks=%d, dynamicalloc_blocks=%d\n",
			(benchmark_target == BENCHMARK_TASKPOOL)?"taskpool":((benchmark_target == BENCHMARK_LIBLFDS_QUEUE)?"lfds_queue":"lfds_ringbuffer"),
			no_tasks, no_threads, tree_height, k_retries, ring_buffer_size,
			(end_put-start_put)/(double)CLOCKS_PER_SEC,
			(end_get-start_get)/(double)CLOCKS_PER_SEC,
			no_tasks / ((end_put-start_put)/(double)CLOCKS_PER_SEC),
			(((end_put-start_put)/((double)CLOCKS_PER_SEC / 1000000000))) / no_tasks,
			no_tasks / ((end_get-start_get)/(double)CLOCKS_PER_SEC),
			(((end_get-start_get)/((double)CLOCKS_PER_SEC / 1000000000))) / no_tasks,
			total_put_errs, total_get_errs,
			last_block_start + 1, last_block_end - last_block_start);

	if(benchmark_target == BENCHMARK_TASKPOOL)
		free_pool(pool);
	else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
		lfds711_queue_umm_cleanup(&qs, NULL);
	else if(benchmark_target == BENCHMARK_LIBLFDS_RINGBUFFER)
		lfds711_ringbuffer_cleanup(&rs, NULL);

	free(put_errs);
	free(get_errs);

#ifdef KEEP_TASKS
	free(tasks);
	free(recovered_tasks);
#endif
}


