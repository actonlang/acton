/*
 * Copyright (C) 2019-2021 Deutsche Telekom AG
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
#include <stdarg.h>
#include <sched.h>
#include <string.h>

#include "fastrand.h"

#include "concurrent_task_pool.h"

// For comparative benchmarking:
#include <liblfds711.h>

#define NO_TASKS 10000
#define NO_THREADS 1

#define RING_BUFFER_SIZE 1000

#define DEFAULT_MAX_BENCHMARK_DURATION_SECONDS 60

// #define VERBOSE

// #define KEEP_TASKS_PUT
// #define KEEP_TASKS_GET

#define BENCHMARK_TASKPOOL 0
#define BENCHMARK_LIBLFDS_QUEUE 1
#define BENCHMARK_LIBLFDS_RINGBUFFER 2
#define BENCHMARK_MUTEX 3
#define BENCHMARK_SPINLOCK_ATOMIC_FLAG 4
#define BENCHMARK_SPINLOCK_PTHREAD 5

int no_tasks = NO_TASKS, no_threads = NO_THREADS, tree_height = DEFAULT_TREE_HEIGHT, k_retries = DEFAULT_K_NO_TRIALS;
long num_cpu, ring_buffer_size = RING_BUFFER_SIZE;
int benchmark_target = BENCHMARK_TASKPOOL;
int verbose = 0;
long * put_errs, * dequeued_elems;
int long long unsigned * last_dequeues;
int benchmark_duration_seconds = DEFAULT_MAX_BENCHMARK_DURATION_SECONDS;

#define NUMBER_OF_NANOSECONDS_IN_ONE_SECOND         1000000000LLU

#if( defined _POSIX_THREADS && _POSIX_TIMERS >= 0 && _POSIX_MONOTONIC_CLOCK >= 0 )
  #define TIME_UNITS_PER_SECOND( pointer_to_time_units_per_second )  *(pointer_to_time_units_per_second) = NUMBER_OF_NANOSECONDS_IN_ONE_SECOND

  #define GET_HIGHRES_TIME( pointer_to_time )                          \
  {                                                                                     \
    struct timespec tp;                                                                 \
    clock_gettime( CLOCK_MONOTONIC_RAW, &tp );                                          \
    *(pointer_to_time) = tp.tv_sec * NUMBER_OF_NANOSECONDS_IN_ONE_SECOND + tp.tv_nsec;  \
  }

#define GET_LOWRES_LOWOVERHEAD_TIME_SECONDS( pointer_to_time )                          \
{                                                                                     \
  struct timespec tp;                                                                 \
  clock_gettime( CLOCK_MONOTONIC_COARSE, &tp );                                          \
  *(pointer_to_time) = tp.tv_sec;  \
}
#else
  #error Linux without high resolution timers.
#endif



// Taskpool-related global vars:

concurrent_pool * pool;

WORD* tasks;
struct lfds711_queue_umm_element *qes;

WORD* recovered_tasks;
struct lfds711_queue_umm_element *recovered_qes;


// Liblfds queue-related global vars:

struct lfds711_queue_umm_state qs;

// Liblfds ringbuffer-related global vars:

struct lfds711_ringbuffer_state rs;
struct lfds711_ringbuffer_element * re;

// Mutex-related:

pthread_mutex_t queue_mutex;

// Spinlock-related:

volatile atomic_flag queue_spinlock_af;

void inline spinlock_lock(volatile atomic_flag *f) {
    while (atomic_flag_test_and_set(f)) {
        // spin until we could set the flag
    }
}
void inline spinlock_unlock(volatile atomic_flag *f) {
    atomic_flag_clear(f);
}

// Libpthread spinlock-related:

pthread_spinlock_t queue_spinlock_pthread;

// Basic list structs for benchmarking mutexes & spinlocks:

typedef struct queue_entry
{
	WORD data;
	struct queue_entry * next;
} queue_entry;

queue_entry * queue_head, * queue_tail;
queue_entry * simple_qes, * recovered_simple_qes;

void *thread_main_put(void *arg)
{
    int thread_id = (int) arg, status=0;
	int long long unsigned start_put, end_put;
	struct lfds711_queue_umm_element qe;
	struct lfds711_ringbuffer_element elem;
	enum lfds711_misc_flag overwrite_occurred_flag;
	unsigned int seed;

	GET_RANDSEED(&seed, thread_id);

	LFDS711_MISC_MAKE_VALID_ON_CURRENT_LOGICAL_CORE_INITS_COMPLETED_BEFORE_NOW_ON_ANY_OTHER_LOGICAL_CORE;

	if(verbose)
	{
		GET_HIGHRES_TIME(&start_put);

		printf("[thread %d put] start_time=%lld, no_tasks=%d\n", thread_id, start_put, 2*no_tasks/no_threads);
	}

	int first_task_index = thread_id*(2*no_tasks/no_threads);

	for(long i=first_task_index;i<first_task_index+2*no_tasks/no_threads;i++)
	{
		if(benchmark_target == BENCHMARK_TASKPOOL)
		{
#ifdef KEEP_TASKS_PUT
			tasks[i]= (WORD) (i+1);
			status = put(tasks[i], pool, &seed);
#else
			status = put((WORD) (i + 1), pool, &seed);
#endif
		}
		else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
		{
#ifdef KEEP_TASKS_PUT
			LFDS711_QUEUE_UMM_SET_VALUE_IN_ELEMENT(qes[i],(WORD) (i + 1));
			lfds711_queue_umm_enqueue(&qs, &qes[i]);
#else
			// NOTE: Due to the way LFDS implements queue elements, this will not work correctly
			// (will corrupt next pointers and the list will end up with a single element).
			// Therefore, we use KEEP_TASKS_PUT when benchmarking the lfds711_queue:
			LFDS711_QUEUE_UMM_SET_VALUE_IN_ELEMENT(qe,(WORD) (i + 1));
			lfds711_queue_umm_enqueue(&qs, &qe);
#endif
		}
		else if(benchmark_target == BENCHMARK_LIBLFDS_RINGBUFFER)
		{
			lfds711_ringbuffer_write(&rs, (void *) (lfds711_pal_uint_t) (i+1), NULL, &overwrite_occurred_flag, NULL, NULL);

		    if(overwrite_occurred_flag == LFDS711_MISC_FLAG_RAISED )
		    		status=1;
		}
		else if(benchmark_target == BENCHMARK_MUTEX ||
				benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG ||
				benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
		{
			if(benchmark_target == BENCHMARK_MUTEX)
				pthread_mutex_lock(&queue_mutex);
			else if(benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG)
			    spinlock_lock(&queue_spinlock_af);
			else if(benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
				pthread_spin_lock(&queue_spinlock_pthread);

#ifdef KEEP_TASKS_PUT
			simple_qes[i].data = (WORD) (i + 1);
			simple_qes[i].next = NULL;

			if(queue_tail != NULL)
			{
				queue_tail->next = simple_qes + i;
				queue_tail = simple_qes + i;
			}
			else
			{
				queue_tail = simple_qes + i;
				queue_head = simple_qes + i;
			}
#endif

			if(benchmark_target == BENCHMARK_MUTEX)
				pthread_mutex_unlock(&queue_mutex);
			else if(benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG)
			    spinlock_unlock(&queue_spinlock_af);
			else if(benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
				pthread_spin_unlock(&queue_spinlock_pthread);
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
		GET_HIGHRES_TIME(&end_put);

		printf("[thread %d put] no_tasks=%d, total_seconds_put=%f, put_tpt=%f, put_latency_ns=%lld\n",
				thread_id, 2*no_tasks/no_threads,
				(end_put-start_put)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND,
				(2*no_tasks/no_threads) / ((end_put-start_put)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND),
				((end_put-start_put)) / (2*no_tasks/no_threads));
	}

    return NULL;
}

void *thread_main_get(void *arg)
{
    int thread_id = (int) arg;
    int status=0, prev_status=1;
	int long long unsigned start_get, end_get, last_dequeue=0;
	struct lfds711_queue_umm_element * qep;
	void *buffer_read_element;
	int dequeued_elements = 0;
	WORD recovered_task;
	queue_entry recovered_simple_qe;
	unsigned int seed;

	GET_RANDSEED(&seed, thread_id);

	LFDS711_MISC_MAKE_VALID_ON_CURRENT_LOGICAL_CORE_INITS_COMPLETED_BEFORE_NOW_ON_ANY_OTHER_LOGICAL_CORE;

	GET_HIGHRES_TIME(&start_get);

	if(verbose)
	{
		printf("[thread %d get] start_time=%lld, no_tasks=%d\n", thread_id, start_get, 2*no_tasks/no_threads);
	}

	do
	{
		if(benchmark_target == BENCHMARK_TASKPOOL)
		{
#ifdef KEEP_TASKS_GET
			status = get(recovered_tasks + thread_id*(no_tasks/no_threads) + dequeued_elements, pool, &seed);
#else
			status = get(&recovered_task, pool, &seed);
#endif
		}
		else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
		{
#ifdef KEEP_TASKS_GET
			status = !lfds711_queue_umm_dequeue(&qs, &qep);
			recovered_tasks[thread_id*(no_tasks/no_threads) + dequeued_elements] = (WORD) LFDS711_QUEUE_UMM_GET_VALUE_FROM_ELEMENT(*qep);
#else
			status = !lfds711_queue_umm_dequeue(&qs, &qep);
			LFDS711_QUEUE_UMM_GET_VALUE_FROM_ELEMENT(*qep);
#endif
		}
		else if(benchmark_target == BENCHMARK_LIBLFDS_RINGBUFFER)
		{
#ifdef KEEP_TASKS_GET
			status = !lfds711_ringbuffer_read(&rs, recovered_tasks + thread_id*(no_tasks/no_threads) + dequeued_elements, NULL);
#else
			status = !lfds711_ringbuffer_read(&rs, &buffer_read_element, NULL);
#endif
		}
		else if(benchmark_target == BENCHMARK_MUTEX ||
				benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG ||
				benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
		{
			if(benchmark_target == BENCHMARK_MUTEX)
				pthread_mutex_lock(&queue_mutex);
			else if(benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG)
			    spinlock_lock(&queue_spinlock_af);
			else if(benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
				pthread_spin_lock(&queue_spinlock_pthread);

			if(queue_head != NULL)
			{
#ifdef KEEP_TASKS_GET
				recovered_simple_qes[thread_id*(no_tasks/no_threads) + dequeued_elements].data = queue_head->data;
#else
				recovered_simple_qe.data=queue_head->data;
#endif
				queue_head = queue_head->next;
				if(queue_head == NULL)
					queue_tail = NULL;

				status = 0;
			}
			else
			{
				status = 1;
			}

			if(benchmark_target == BENCHMARK_MUTEX)
				pthread_mutex_unlock(&queue_mutex);
			else if(benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG)
			    spinlock_unlock(&queue_spinlock_af);
			else if(benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
				pthread_spin_unlock(&queue_spinlock_pthread);
		}

		if(status==0)
		{
			dequeued_elements++;
		}
		else
		{
			GET_HIGHRES_TIME(&end_get);

			if(prev_status==0)
				last_dequeue=end_get;

			if(((end_get - start_get) / NUMBER_OF_NANOSECONDS_IN_ONE_SECOND) > benchmark_duration_seconds)
				break;
		}

		prev_status = status;

		if(verbose)
		{
			int value=-1;

			if(status==0)
			{
				switch(benchmark_target)
				{
					case BENCHMARK_TASKPOOL:
					case BENCHMARK_LIBLFDS_QUEUE:
					case BENCHMARK_LIBLFDS_RINGBUFFER:
#ifdef KEEP_TASKS_GET
						value = (int) recovered_tasks[thread_id*(no_tasks/no_threads) + dequeued_elements];
#else
						value = (int) recovered_task;
#endif
						break;
					case BENCHMARK_MUTEX:
					case BENCHMARK_SPINLOCK_ATOMIC_FLAG:
					case BENCHMARK_SPINLOCK_PTHREAD:
#ifdef KEEP_TASKS_GET
						value = (int) recovered_simple_qes[thread_id*(no_tasks/no_threads) + dequeued_elements].data;
#else
						value = (int) recovered_simple_qe.data;
#endif
						break;
				}

				printf("[thread %d get] got task %d\n", thread_id, value);
			}
			else
			{
				printf("[thread %d get] failed to get task %f ago, last dequeue %f ago, start_get=%lld, end_get=%lld, last_dequeue=%lld, dequeued_elements=%d\n",
						thread_id,
						(end_get-start_get)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND,
						(last_dequeue>0)?((last_dequeue-start_get)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND):-1,
						start_get, end_get, last_dequeue, dequeued_elements);
			}
		}
	} while(1);

	GET_HIGHRES_TIME(&end_get);

	dequeued_elems[thread_id]=dequeued_elements;
	last_dequeues[thread_id]=last_dequeue;

	if(verbose)
	{
		printf("[thread %d get] no_tasks=%d, total_seconds_get=%f, get_tpt=%f, get_latency_ns=%lld, quit_after=%f\n",
				thread_id, dequeued_elements,
				(dequeued_elements>0)?((last_dequeue-start_get)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND):-1,
				(dequeued_elements>0)?((no_tasks/no_threads) / ((last_dequeue-start_get)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND)):-1,
				(dequeued_elements>0)?(((last_dequeue-start_get)) / (no_tasks/no_threads)):-1,
				(end_get-start_get)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND);
	}

    return NULL;
}


int main(int argc, char **argv) {

	struct lfds711_queue_umm_element qe_dummy;

	int status = 0, opt, n;
	long total_put_errs = 0, total_get_errs = 0;
	int long long unsigned start_put, end_put, start_get, end_get, start_prealloc, end_prealloc;
	char * struct_type = "NA";
	int alternate_prod_cons_placement = 0;

    while ((opt = getopt(argc, argv, "t:h:k:T:B:R:M:av")) != -1)
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
            		switch(optarg[0])
            		{
					case 'Q':
					{
							benchmark_target = BENCHMARK_LIBLFDS_QUEUE;
							struct_type = "lfds_queue";
							break;
					}
					case 'R':
					{
							benchmark_target = BENCHMARK_LIBLFDS_RINGBUFFER;
							struct_type = "lfds_ringbuffer";
							break;
					}
					case 'M':
					{
							benchmark_target = BENCHMARK_MUTEX;
							struct_type = "pthread_mutex";
							break;
					}
					case 'A':
					{
							benchmark_target = BENCHMARK_SPINLOCK_ATOMIC_FLAG;
							struct_type = "atomic_flag_spinlock";
							break;
					}
					case 'S':
					{
							benchmark_target = BENCHMARK_SPINLOCK_PTHREAD;
							struct_type = "pthread_spinlock";
							break;
					}
					case 'T':
					default:
					{
							benchmark_target = BENCHMARK_TASKPOOL;
							struct_type = "taskpool";
					}
            		}

                break;
            }
            case 'R':
            {
                n = atoi(optarg);
                ring_buffer_size = (n>0)?n:RING_BUFFER_SIZE;
                break;
            }
            case 'M':
            {
                n = atoi(optarg);
                benchmark_duration_seconds = (n>0)?n:DEFAULT_MAX_BENCHMARK_DURATION_SECONDS;
                break;
            }
            case 'a':
            {
            		alternate_prod_cons_placement = 1;
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

#ifdef KEEP_TASKS_PUT
	if(benchmark_target == BENCHMARK_TASKPOOL)
	{
		tasks = (WORD*) malloc(no_tasks * sizeof(WORD));
	}
	else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
	{
		qes = (struct lfds711_queue_umm_element *) malloc(no_tasks * sizeof(struct lfds711_queue_umm_element));
	}
	else if(benchmark_target == BENCHMARK_MUTEX || benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG || benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
	{
		simple_qes = (queue_entry *) malloc(no_tasks * sizeof(struct queue_entry));
	}
#endif

#ifdef KEEP_TASKS_GET
	if(benchmark_target == BENCHMARK_TASKPOOL)
	{
		recovered_tasks = (WORD*) malloc(no_tasks * sizeof(WORD));
	}
	else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
	{
		recovered_qes = (struct lfds711_queue_umm_element *) malloc(no_tasks * sizeof(struct lfds711_queue_umm_element));
	}
	else if(benchmark_target == BENCHMARK_MUTEX || benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG || benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
	{
		recovered_simple_qes = (queue_entry *) malloc(no_tasks * sizeof(struct queue_entry));
	}
#endif

	GET_HIGHRES_TIME(&start_prealloc);

	// Taskpool init:
	if(benchmark_target == BENCHMARK_TASKPOOL)
	{
		pool = allocate_pool_with_tree_height(tree_height, no_tasks);
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
	else if(benchmark_target == BENCHMARK_MUTEX ||
			benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG ||
			benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
	{
		queue_head=NULL;
		queue_tail=NULL;
		if(benchmark_target == BENCHMARK_MUTEX)
			pthread_mutex_init(&queue_mutex, NULL);
		else if(benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
			pthread_spin_init(&queue_spinlock_pthread, PTHREAD_PROCESS_SHARED);
	}

	GET_HIGHRES_TIME(&end_prealloc);

    pthread_t threads_put[no_threads/2];
    pthread_t threads_get[no_threads/2];
    pthread_attr_t attrs[no_threads];
    cpu_set_t cpu_set[no_threads];

    for(int th_id = 0; th_id < no_threads; ++th_id)
    {
        pthread_attr_init(&attrs[th_id]);
        CPU_ZERO(&cpu_set[th_id]);

        int core_id;

        if(alternate_prod_cons_placement)
        {
        		if(th_id<no_threads/2)
        			core_id = 2*th_id % num_cpu;
        		else
        			core_id = (2*(th_id-no_threads/2)+1) % num_cpu;
        }
        else
        {
        		core_id = (th_id % num_cpu) * (hyperthreading+1);
        }

        if(verbose)
        		printf("Setting thread affinity of thread %d (%s) to core %d\n",
        						th_id, (th_id<no_threads/2)?"put":"get", core_id);

        CPU_SET(core_id, &cpu_set[th_id]);
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__)
        pthread_attr_setaffinity_np(&attrs[th_id], sizeof(cpu_set_t), &cpu_set[th_id]);
#else
        printf("\x1b[41;1mSetting thread affinity is not implemented for your OS\x1b[m\n");
        // __unix__
#endif
    }

    put_errs = (long *) malloc(no_threads/2 * sizeof(long));
    dequeued_elems = (long *) malloc(no_threads/2 * sizeof(long));
    last_dequeues = (int long long unsigned *) malloc(no_threads/2 * sizeof(int long long unsigned));

	int last_block_start = (benchmark_target == BENCHMARK_TASKPOOL)?(get_last_block_id(pool)):-1;

	GET_HIGHRES_TIME(&start_put);

    	for(int th_id = 0; th_id < no_threads/2; ++th_id)
    {
        pthread_create(&threads_put[th_id], &attrs[th_id], thread_main_put, (void *) th_id);
    }

    	GET_HIGHRES_TIME(&start_get);

    	for(int th_id = 0; th_id < no_threads/2; ++th_id)
    	{
    		pthread_create(&threads_get[th_id], &attrs[th_id+no_threads/2], thread_main_get, (void *) th_id);
    	}

    for(int th_id = 0; th_id < no_threads/2; ++th_id)
    {
        pthread_join(threads_put[th_id], NULL);
		if(verbose)
			printf("[%d put] exited\n", th_id);
    }

    GET_HIGHRES_TIME(&end_put);

	for(int th_id = 0; th_id < no_threads/2; ++th_id)
	{
		pthread_join(threads_get[th_id], NULL);
		if(verbose)
			printf("[%d get] exited\n", th_id);
	}

	end_get=0;
	total_get_errs = no_tasks;

	for(int th_id = 0; th_id < no_threads/2; ++th_id)
	{
		total_put_errs += put_errs[th_id];
		total_get_errs -= dequeued_elems[th_id];
		if(last_dequeues[th_id] > end_get && dequeued_elems[th_id] > 0)
			end_get = last_dequeues[th_id];
	}

	int tree_degree_pool =  (benchmark_target == BENCHMARK_TASKPOOL)?pool->degree:-1;
	int tree_height_pool =  (benchmark_target == BENCHMARK_TASKPOOL)?pool->tree_height:-1;
	int k_retries_pool =  (benchmark_target == BENCHMARK_TASKPOOL)?pool->k_no_trials:-1;
	int last_block_end = (benchmark_target == BENCHMARK_TASKPOOL)?get_last_block_id(pool):-1;
	int last_used_block = (benchmark_target == BENCHMARK_TASKPOOL)?pool->producer_tree->node_id:-1;
	int block_size = (benchmark_target == BENCHMARK_TASKPOOL)?(CALCULATE_TREE_SIZE(pool->tree_height, pool->degree)):-1;
	int block_fill = (benchmark_target == BENCHMARK_TASKPOOL)?(TREE_FILL_FACTOR(pool->tree_height, pool->degree, pool->k_no_trials)):-1;

	printf("data_struct=%s, no_tasks=%d, no_threads=%d, tree_degree=%d, tree_height=%d, k_retries=%d, ring_buffer_size=%ld, total_seconds_put=%f, total_seconds_get=%f, put_tpt=%f, put_latency_ns=%lld, get_tpt=%f, get_latency_ns=%lld, put_errs=%ld, get_errs=%ld, prealloc_blocks=%d, dynamicalloc_blocks=%d, unused_blocks=%d, block_size=%d, block_fill=%d, total_seconds_prealloc=%f\n",
			struct_type, no_tasks, no_threads/2, tree_degree_pool, tree_height_pool, k_retries_pool, ring_buffer_size,
			(end_put-start_put)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND,
			(end_get-start_get)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND,
			no_tasks / ((end_put-start_put)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND),
			(end_put-start_put) / no_tasks,
			(no_tasks-total_get_errs) / ((end_get-start_get)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND),
			(no_tasks>total_get_errs)?((end_get-start_get) / (no_tasks-total_get_errs)):-1,
			total_put_errs, total_get_errs,
			last_block_start + 1, last_block_end - last_block_start, last_block_end - last_used_block,
			block_size, block_fill,
			(end_prealloc-start_prealloc)/(double)NUMBER_OF_NANOSECONDS_IN_ONE_SECOND);

	if(benchmark_target == BENCHMARK_TASKPOOL)
		free_pool(pool);
	else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
		lfds711_queue_umm_cleanup(&qs, NULL);
	else if(benchmark_target == BENCHMARK_LIBLFDS_RINGBUFFER)
		lfds711_ringbuffer_cleanup(&rs, NULL);

	free(put_errs);
	free(dequeued_elems);
	free(last_dequeues);

#ifdef KEEP_TASKS_PUT
	if(benchmark_target == BENCHMARK_TASKPOOL)
		free(tasks);
	else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
		free(qes);
	else if(benchmark_target == BENCHMARK_MUTEX || benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG || benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
		free(simple_qes);
#endif

#ifdef KEEP_TASKS_GET
	if(benchmark_target == BENCHMARK_TASKPOOL)
		free(recovered_tasks);
	else if(benchmark_target == BENCHMARK_LIBLFDS_QUEUE)
		free(recovered_qes);
	else if(benchmark_target == BENCHMARK_MUTEX || benchmark_target == BENCHMARK_SPINLOCK_ATOMIC_FLAG || benchmark_target == BENCHMARK_SPINLOCK_PTHREAD)
		free(recovered_simple_qes);
#endif
}


