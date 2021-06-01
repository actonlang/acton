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
 * This file contains functions related to managing lists of tree pools
 *
 *  Created on: 22 Jan 2019
 *      Author: aagapi
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdatomic.h>
#include <time.h>
#include <string.h>

#include "concurrent_task_pool.h"

concurrent_pool_node * allocate_pool_node(int node_id, int tree_height, int degree, int * precomputed_level_sizes, concurrent_pool_node * prealloc_mem)
{
	concurrent_pool_node * pn = NULL;
	int total_nodes, total_size;

	if(prealloc_mem != NULL)
	{
		pn=prealloc_mem;
	}
	else
	{
		if(precomputed_level_sizes!=NULL)
			total_nodes = precomputed_level_sizes[tree_height-1];
		else
			total_nodes = CALCULATE_TREE_SIZE(tree_height, degree);

		total_size = total_nodes * sizeof(struct concurrent_tree_pool_node);

		// Actual array of "total_nodes" tree nodes is allocated right after the "struct concurrent_pool_node" metadata:

		pn = (concurrent_pool_node *) malloc(sizeof(struct concurrent_pool_node) + total_size);

		if(pn == NULL)
		{
			printf("Failed to allocate tree of size %d/%d\n", total_nodes, total_size);
			return NULL;
		}

		memset(pn, 0, sizeof(struct concurrent_pool_node) + total_size);

	#ifdef TASKPOOL_DEBUG
		printf("Allocated tree of size %d/%d\n", total_nodes, total_size);
	#endif
	}

	pn->node_id = node_id;
	atomic_init(&pn->next, NULL);

	return pn;
}

void free_pool_node(concurrent_pool_node * p)
{
	free(p);
}

concurrent_pool * _allocate_pool(int tree_height, int k_no_trials, int degree, int no_prealloc)
{
	int nodes_per_tree = CALCULATE_TREE_SIZE(tree_height, degree);
	int per_tree_data_size = nodes_per_tree * sizeof(struct concurrent_tree_pool_node);
	int no_prealloced_trees = NO_PREALLOCATED_TREES(no_prealloc, tree_height, degree, k_no_trials);

	// The list of all pre-allocated tree blocks is allocated in the memchunk right after the pool's metadata.
	// In turn, for each pre-allocated tree node block, the actual array of "nodes_per_tree" tree nodes is also
	// allocated in the chunk right after the "struct concurrent_pool_node" metadata. So we use a single malloc for all these:

	concurrent_pool * p = (concurrent_pool *) malloc(sizeof(struct concurrent_pool) + no_prealloced_trees * (sizeof(struct concurrent_pool_node) + per_tree_data_size));

	if(p == NULL)
	{
		printf("Failed to allocate pool of %d blocks, each of size %d/%d\n", no_prealloced_trees, nodes_per_tree, per_tree_data_size);
		return NULL;
	}

	memset(p, 0, sizeof(struct concurrent_pool) + no_prealloced_trees * (sizeof(struct concurrent_pool_node) + per_tree_data_size));

#ifdef TASKPOOL_DEBUG
	printf("Pre-allocated tree of %d blocks, each of size %d/%d\n", no_prealloced_trees, nodes_per_tree, per_tree_data_size);
#endif

#ifdef PRECALCULATE_TREE_LEVEL_SIZES
	p->level_sizes = (int *) malloc(tree_height * sizeof(int));

	if(p->level_sizes == NULL)
	{
		free(p);
		return NULL;
	}

	for(int i=0;i<tree_height;i++)
		p->level_sizes[i]=CALCULATE_TREE_SIZE(i+1, degree);
#endif

	p->tree_height = tree_height;
	p->degree = degree;
	p->k_no_trials = k_no_trials;

	preallocate_trees(p, no_prealloced_trees, per_tree_data_size, (char *)(&p[1]));

	atomic_init(&p->producer_tree, p->head);
	concurrent_pool_node_ptr_pair cts = { .prev = NULL, .crt = p->head };
	atomic_init(&p->consumer_trees, cts); // Set consumer pointer pair to (prev=NULL, current=producer=head)

	return p;
}

concurrent_pool * allocate_pool_with_tree_height_and_degree(int tree_height, int degree, int no_prealloc)
{
	return _allocate_pool(tree_height, DEFAULT_K_NO_TRIALS, degree, MAX(no_prealloc, DEFAULT_PREALLOCATED_ELEMENTS));
}

concurrent_pool * allocate_pool_with_tree_height(int tree_height, int no_prealloc)
{
	return _allocate_pool(tree_height, DEFAULT_K_NO_TRIALS, DEFAULT_DEGREE, MAX(no_prealloc, DEFAULT_PREALLOCATED_ELEMENTS));
}

concurrent_pool * allocate_pool(int no_prealloc)
{
	return _allocate_pool(DEFAULT_TREE_HEIGHT, DEFAULT_K_NO_TRIALS, DEFAULT_DEGREE, MAX(no_prealloc, DEFAULT_PREALLOCATED_ELEMENTS));
}

void set_no_trials(concurrent_pool * pool, int no_trials)
{
	pool->k_no_trials = no_trials;
}

void free_pool(concurrent_pool * p)
{
	concurrent_pool_node_ptr pn = p->_last_prealloc_block->next, next_pn = NULL;

	while(pn != NULL)
	{
		next_pn = pn->next;
		free_pool_node(pn);
		pn = next_pn;
	}

#ifdef PRECALCULATE_TREE_LEVEL_SIZES
	free(p->level_sizes);
#endif
	free(p);
}

int get_last_block_id(concurrent_pool * p)
{
	concurrent_pool_node_ptr pn = p->head, next_pn = NULL;

	while(pn->next != NULL)
	{
		next_pn = pn->next;
		pn = next_pn;
	}

	return pn->node_id;
}

static inline int move_consumer_ptr_back(concurrent_pool* pool, concurrent_pool_node_ptr producer_tree, concurrent_pool_node_ptr_pair c_ptrs_in)
{
	atomic_fetch_add(&(pool->old_producers), 1);

	concurrent_pool_node_ptr_pair c_ptrs = c_ptrs_in;

	while(1)
	{
		concurrent_pool_node_ptr_pair new_cts = { .prev = NULL, .crt = producer_tree };

		if(LOOP_CAS(&(pool->consumer_trees), &c_ptrs, new_cts))
		{
#ifdef TASKPOOL_DEBUG
			printf("Succeeded moving back consumer ptrs to (%p/%d,%p/%d)\n",
						new_cts.prev, (new_cts.prev!=NULL)?(new_cts.prev->node_id):-1,
						new_cts.crt, (new_cts.crt!=NULL)?(new_cts.crt->node_id):-1);
#endif

			break;
		}
		else
		{
#ifdef TASKPOOL_DEBUG
			printf("Failed moving back consumer ptrs to (%p/%d,%p/%d)\n",
						new_cts.prev, (new_cts.prev!=NULL)?(new_cts.prev->node_id):-1,
						new_cts.crt, (new_cts.crt!=NULL)?(new_cts.crt->node_id):-1);
#endif
		}

		if(c_ptrs.crt->node_id <= producer_tree->node_id)
			break;
	}

	atomic_fetch_add(&(pool->old_producers), -1);

	return 0;
}


// Note: This function is NOT supposed to be thread safe. Only call in pool constructor.

int preallocate_trees(concurrent_pool* pool, int no_trees, int per_tree_data_size, char * prealloc_mem)
{
	concurrent_pool_node_ptr crt_tree = NULL;

	for(int i=0;i<no_trees;i++)
	{
#ifdef PRECALCULATE_TREE_LEVEL_SIZES
		concurrent_pool_node_ptr pool_node = allocate_pool_node(i+1, pool->tree_height, pool->degree, pool->level_sizes, (concurrent_pool_node *)(prealloc_mem + i*(sizeof(struct concurrent_pool_node) + per_tree_data_size)));
#else
		concurrent_pool_node_ptr pool_node = allocate_pool_node(i+1, pool->tree_height, pool->degree, NULL, (concurrent_pool_node *)(prealloc_mem + i*(sizeof(struct concurrent_pool_node) + per_tree_data_size)));
#endif

		if(crt_tree != NULL)
		{
			atomic_init(&crt_tree->next, pool_node);
		}
		else
		{
			pool->head = pool_node;
		}

		crt_tree = pool_node;
	}

	pool->_last_prealloc_block=crt_tree;

	return 0;
}


static inline int insert_new_tree(concurrent_pool* pool, concurrent_pool_node_ptr producer_tree_in)
{
	concurrent_pool_node_ptr producer_tree = producer_tree_in, next_ptr = LOAD(producer_tree_in->next);
#ifdef PRECALCULATE_TREE_LEVEL_SIZES
	concurrent_pool_node_ptr pool_node = allocate_pool_node(0, pool->tree_height, pool->degree, pool->level_sizes, NULL);
#else
	concurrent_pool_node_ptr pool_node = allocate_pool_node(0, pool->tree_height, pool->degree, NULL, NULL);
#endif

	if(pool_node == NULL)
		return -1;

	// Find end of tree list:

	while(next_ptr != NULL)
	{
		producer_tree = next_ptr;
		next_ptr = LOAD(producer_tree->next);
	}

	pool_node->node_id = producer_tree->node_id + 1;

	concurrent_pool_node_ptr old = NULL;

	// Attempt to chain our newly allocated tree pool to the end of the list.
	// If the CAS fails, it means a concurrent producer managed to extend the list before us,
	// in which case we free our newly allocated pool and return.

	if(!CAS(&(producer_tree->next), &old, pool_node))
	{
#ifdef TASKPOOL_DEBUG
			printf("Failed setting next ptr of %p/%d to %p/%d\n",
						producer_tree, (producer_tree!=NULL)?(producer_tree->node_id):-1,
						pool_node, (pool_node!=NULL)?(pool_node->node_id):-1);
#endif

		free_pool_node(pool_node);
	}
	else
	{
#ifdef TASKPOOL_DEBUG
			printf("Succeeded setting next ptr of %p/%d to %p/%d\n",
						producer_tree, (producer_tree!=NULL)?(producer_tree->node_id):-1,
						pool_node, (pool_node!=NULL)?(pool_node->node_id):-1);
#endif
	}

	return 0;
}

int put(WORD task, concurrent_pool* pool, unsigned int * seedptr)
{
	concurrent_pool_node_ptr producer_tree = NULL;
	int status = 0;
#ifdef PRECALCULATE_TREE_LEVEL_SIZES
	int * precalculated_level_sizes = pool->level_sizes;
#else
	int * precalculated_level_sizes = NULL;
#endif

	producer_tree = LOAD(pool->producer_tree);

	while(1)
	{
		int put_index = put_in_tree(task, TREE_PTR(producer_tree), pool->degree, pool->tree_height, pool->k_no_trials, precalculated_level_sizes, seedptr);

		if(put_index >= 0)
		{
#ifdef TASKPOOL_TRACE
			printf("Successfully put task %ld in tree %d at index %d\n", (long) task, producer_tree->node_id, put_index);
#endif
			// Put succeeded in current tree, but we may need to move back consumer pointer if it overshot us:

			concurrent_pool_node_ptr_pair c_ptrs = LOAD(pool->consumer_trees);

			if(c_ptrs.crt->node_id > producer_tree->node_id)
			{
				move_consumer_ptr_back(pool, producer_tree, c_ptrs);
			}

			return 0;
		}
		else // current producer tree is "full"
		{
#ifdef TASKPOOL_DEBUG
			printf("put_in_tree() returned status %d when attempting to put task %ld in full tree %d. Next pointer is: %d\n", status, (long) task, producer_tree->node_id, (producer_tree->next != NULL)?(producer_tree->next->node_id):(-1));
#endif

			// It might be that another producer already inserted a new tree, so first check for that:

			atomic_concurrent_pool_node_ptr producer_tree_next = LOAD(producer_tree->next);

			if(producer_tree_next == NULL)
			{
				status = insert_new_tree(pool, producer_tree);

#ifdef TASKPOOL_DEBUG
				printf("insert_new_tree() returned status %d when attempting to put task %ld in full tree %d. Next pointer is: %d\n", status, (long) task, producer_tree->node_id, producer_tree->next->node_id);
#endif

				// Note the only failure condition is failure to allocate memory, otherwise inserts s'd always succeed:

				if(status != 0)
					return status;
			}

			// At this point a new tree was successfully linked into the list (either by us or by a concurrent producer).
			// We next attempt to progress the producer pointer of the pool. If that CAS fails, it just means that
			// some other producer managed to progress it before us, which is OK: we just continue and attempt
			// to put our task in the current producer tree of the pool:

			status = CAS(&(pool->producer_tree), &producer_tree, LOAD(producer_tree->next));

#ifdef TASKPOOL_DEBUG
			if(status != 1)
				printf("CAS returned status %d when attempting to progress producer pointer (after put failed in full tree).\n", status);
#endif
		}
	}
}

int get(WORD* task, concurrent_pool* pool, unsigned int * seedptr)
{
	concurrent_pool_node_ptr producer_tree = NULL;
	concurrent_pool_node_ptr_pair consumer_ptrs = LOAD(pool->consumer_trees);
	int status;

	while(1)
	{
		// First try the previous and current consumer pointers:

		if(consumer_ptrs.prev != NULL)
		{
#ifdef TASKPOOL_TRACE
			printf("get() attempting to find a task in prev tree %d\n", consumer_ptrs.prev->node_id);
#endif

			if(get_from_tree(task, TREE_PTR(consumer_ptrs.prev), pool->degree, pool->tree_height, pool->level_sizes, seedptr)==0)
			{
#ifdef TASKPOOL_TRACE
			printf("get() found task %ld in prev tree %d\n", (long) *task, consumer_ptrs.prev->node_id);
#endif
				return 0;
			}

		}

		if(consumer_ptrs.crt != NULL)
		{
#ifdef TASKPOOL_TRACE
			printf("get() attempting to find a task in crt tree %d\n", consumer_ptrs.crt->node_id);
#endif

			if(get_from_tree(task, TREE_PTR(consumer_ptrs.crt), pool->degree, pool->tree_height, pool->level_sizes, seedptr)==0)
			{
#ifdef TASKPOOL_TRACE
				printf("get() found task %ld in crt tree %d\n", (long) *task, consumer_ptrs.crt->node_id);
#endif
				return 0;
			}
		}

		producer_tree = LOAD(pool->producer_tree);

		if(producer_tree->node_id <= consumer_ptrs.crt->node_id)
		{
#ifdef TASKPOOL_TRACE
			printf("get(): No new tasks exist (producer tree %d < consumer tree %d)\n", producer_tree->node_id, consumer_ptrs.crt->node_id);
#endif
			*task = NULL; // No new tasks exist
			return 1;
		}

		concurrent_pool_node_ptr_pair new_cts = { .prev = consumer_ptrs.crt, .crt = consumer_ptrs.crt->next };

		if(LOAD(pool->old_producers) == 0)
		{
			// Whether my CAS to update consumer pointers succeeds or not, use the most recent value
			// of the (prev, crt) pointer pair to retry grabbing tasks from the new tree pointers:

			status = CAS(&(pool->consumer_trees), &consumer_ptrs, new_cts);

#ifdef TASKPOOL_DEBUG
			printf("%s setting consumer ptrs to (%p/%d,%p/%d)\n", (status>0)?"Succeeded":"Failed",
						new_cts.prev, (new_cts.prev!=NULL)?(new_cts.prev->node_id):-1,
						new_cts.crt, (new_cts.crt!=NULL)?(new_cts.crt->node_id):-1);
#endif
		}
		else
		// If there are currently some producers attempting to move consumer pointer back, retry to read from
		// the (updated) consumer pointer again, but *do not* attempt to progress consumer pointer after that,
		// to avoid race condition:
		{
			consumer_ptrs = new_cts;

#ifdef TASKPOOL_DEBUG
			printf("Locally setting consumer ptrs to (%p/%d,%p/%d)\n",
						new_cts.prev, (new_cts.prev!=NULL)?(new_cts.prev->node_id):-1,
						new_cts.crt, (new_cts.crt!=NULL)?(new_cts.crt->node_id):-1);
#endif
		}
	}
}


