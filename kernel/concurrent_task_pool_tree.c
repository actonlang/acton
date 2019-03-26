/*
 * A concurrent, scalable, wait-free task pool implementation that performs well under high CAS contention
 *
 * This file contains functions related to managing tree pools
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
#include "fastrand.h"

static inline int has_tasks_k(concurrent_tree_pool_node node, int degree)
{
	if(node.data != NULL)
		return 1;

	for(int i=0;i< degree;i++)
		if(!IS_EMPTY_BYTE(node.child_has_tasks[i]))
			return 1;

	return 0;
}

static inline int random_in_level(int level, int degree, int * precomputed_level_sizes)
{
	int first_in_level, last_in_level;

	if(level==0)
		return 0;

	if(precomputed_level_sizes!=NULL)
	{
		first_in_level = precomputed_level_sizes[level-1];
		last_in_level = precomputed_level_sizes[level] - 1;
	}
	else
	{
		first_in_level = CALCULATE_TREE_SIZE(level, degree);
		last_in_level = CALCULATE_TREE_SIZE(level+1, degree) - 1;
	}

	return (fastrand() % (last_in_level - first_in_level + 1)) + first_in_level;
}

static inline int update_father(int index, concurrent_tree_pool_node* pool, int degree, unsigned char value)
{
	atomic_fetch_add(&(pool[index].pending), 1);
	int parent_index=PARENT_K(index, degree);
	int child_index = (index - 1) % degree;
	unsigned char old = 0, new = 0;
	int success = 0;

	old = LOAD(pool[parent_index].child_has_tasks[child_index]);
	new = (value)?(FULL_0_VERSION_BYTE):(EMPTY_0_VERSION_BYTE);
	new = NEW_VERSION_BYTE(new, VERSION_BYTE(old) + 1);

	success = CAS(&(pool[parent_index].child_has_tasks[child_index]), &old, new);

#ifdef TASKPOOL_DEBUG
	printf("update_father(): Updating parent index %d of index %d from (%u, %u, %u) to (%u, %u, %u) returned %d\n",
			parent_index, index,
			old, IS_EMPTY_BYTE(old), VERSION_BYTE(old),
			new, IS_EMPTY_BYTE(new), VERSION_BYTE(new),
			success);
#endif

	atomic_fetch_add(&(pool[index].pending), -1);

	return success;
}

static inline void update_node_metadata(int index, concurrent_tree_pool_node* pool, int degree, unsigned char value)
{
	int trials = 0, crt_index = index;

#ifdef TASKPOOL_TRACE
	printf("update_node_metadata(index=%d, value=%d)\n", index, value);
#endif

	while(HAS_PARENT(crt_index))
	{
		int have_tasks = has_tasks_k(pool[crt_index], degree);

		if(value != have_tasks)
		{
#ifdef TASKPOOL_TRACE
			printf("update_node_metadata(): Skipping because my operation has been eliminated: value=%d != has_tasks(%d)=%d\n", value, crt_index, have_tasks);
#endif
			return;
		}

		int parent_index=PARENT_K(crt_index, degree);
		concurrent_tree_pool_node parent_node = pool[parent_index];
		int child_index = (crt_index - 1) % degree;

		if(IS_EMPTY_BYTE(pool[parent_index].child_has_tasks[child_index]) == have_tasks || pool[crt_index].pending > 0)
		{
#ifdef TASKPOOL_TRACE
			printf("update_node_metadata(index=%d, value=%d), updating parent index %d\n", index, value, parent_index);
#endif

			trials++;

			if(!update_father(crt_index, pool, degree, value))
			{
#ifdef TASKPOOL_TRACE
				printf("update_node_metadata(index=%d, value=%d), update_father() failed on parent index %d, trials=%d\n", index, value, parent_index, trials);
#endif
				if(trials < 2)
					continue;
			}
		}
		else
		{
#ifdef TASKPOOL_TRACE
			printf("update_node_metadata(index=%d, value=%d), skipping update of parent index %d because pool[%d].pending=%d, child_index=%d, parent_empty_child=%d, has_tasks=%d\n",
					index, value, parent_index,
					crt_index, pool[crt_index].pending,
					child_index, IS_EMPTY(pool[parent_index].child_has_tasks[child_index]),
					have_tasks);
#endif
			}

		crt_index = parent_index;

		trials = 0;
	}
}

static inline int put_in_node_simple(int index, concurrent_tree_pool_node* pool, int degree, WORD task)
{
	int crt_index = index;

	unsigned char old = LOAD(pool[crt_index].dirty);

	if(old == 0 && CAS(&pool[crt_index].dirty, &old, 1))
	{
		pool[crt_index].data = task;

		return crt_index;
	}
	else
	{
		return -1;
	}
}

static inline int put_in_node(int index, concurrent_tree_pool_node* pool, int degree, WORD task)
{
	int crt_index = index;

	while(HAS_PARENT(crt_index) && pool[PARENT_K(crt_index, degree)].data==NULL)
		crt_index=PARENT_K(crt_index, degree);

	unsigned char old = LOAD(pool[crt_index].dirty);

	if(old == 0 && CAS(&pool[crt_index].dirty, &old, 1))
	{
		pool[crt_index].data = task;

		return crt_index;
	}
	else
	{
		return -1;
	}
}

static inline int find_node_for_put(WORD task, concurrent_tree_pool_node* pool, int tree_height, int degree, int k_no_trials, int * precomputed_level_sizes)
{
	int index;

#ifdef FAST_PUT
	int total_nodes = 0;

	if(precomputed_level_sizes!=NULL)
	{
		total_nodes = precomputed_level_sizes[tree_height-1];
	}
	else
	{
		total_nodes = CALCULATE_TREE_SIZE(tree_height, degree);
	}

	index=put_in_node_simple(fastrand() % total_nodes, pool, degree, task);

	if(index!=-1)
		return index;
#endif

	for(int level=1;level<tree_height;level++)
	{
		int no_trials = ((level==tree_height-1)?k_no_trials:1);

		for(int trials=0;trials<no_trials;trials++)
		{
			index=put_in_node(random_in_level(level, degree, precomputed_level_sizes), pool, degree, task);
			if(index!=-1)
				return index;
		}
	}

	return -1;
}

int put_in_tree(WORD task, concurrent_tree_pool_node* pool, int degree, int tree_height, int k_no_trials, int * precomputed_level_sizes)
{
	// Handle root insertions separately for higher speed:

	unsigned char old = LOAD(pool[0].dirty);

	if(old == 0)
	{
		if(CAS(&pool[0].dirty, &old, 1))
		{
			pool[0].data = task;
			return 0;
		}
	}

	if(tree_height==1)
	{
		return -1;
	}

	int index = find_node_for_put(task, pool, tree_height, degree, k_no_trials, precomputed_level_sizes);

	if(index==-1)
		return -1;

	update_node_metadata(index, pool, degree, 1);

	return index;
}

// Returns either a node with an existing, non-grabbed task, or a node with empty children:

static inline int find_node_for_get(concurrent_tree_pool_node* pool, int degree, int tree_height, int * index_p, int * precomputed_level_sizes)
{
	int index = 0;

#ifdef FAST_GET
	int total_nodes = 0;

	if(precomputed_level_sizes!=NULL)
	{
		total_nodes = precomputed_level_sizes[tree_height-1];
	}
	else
	{
		total_nodes = CALCULATE_TREE_SIZE(tree_height, degree);
	}

	index=fastrand() % total_nodes;

	if((pool[index].data != NULL))
	{
		*index_p = index;
		return 0;
	}
#endif

	while(1)
	{
#ifdef TASKPOOL_TRACE
		printf("find_node_for_get: index=%d, data=%ld, grabbed=%d, dirty=%d, left_empty=(%d, %d, %d), right_empty=(%d, %d, %d)\n",
				index, (long) pool[index].data, atomic_load(&pool[index].grabbed), atomic_load(&pool[index].dirty),
				pool[index].tasks_left, IS_EMPTY(pool[index].tasks_left), VERSION(pool[index].tasks_left),
				pool[index].tasks_right, IS_EMPTY(pool[index].tasks_right), VERSION(pool[index].tasks_right));
#endif

		// Return this node's task if it exists:

		if((pool[index].data != NULL))
		{
			*index_p = index;
			return 0;
		}

#ifdef FAST_GET_PER_LEVEL
		int rand_child = fastrand() % degree;
		if(!IS_EMPTY_BYTE(pool[index].child_has_tasks[rand_child]))
		{
			index = CHILD_K(index, rand_child, degree);
			continue;
		}
#endif

		int full_children[MAX_DEGREE], no_full_children=0;

		for(int i=0;i<degree;i++)
		{
			if(!IS_EMPTY_BYTE(pool[index].child_has_tasks[i]))
			{
				full_children[no_full_children++] = i;
			}
		}

		if(no_full_children==0)
		{
			*index_p = index;
			return -1;
		}
		else if(no_full_children==1)
		{
			index = CHILD_K(index,full_children[0], degree);
		}
		else
		{
			index = CHILD_K(index,full_children[fastrand() % no_full_children], degree);
		}
	}
}

int get_from_tree(WORD* task, concurrent_tree_pool_node* pool, int degree, int tree_height, int * precomputed_level_sizes)
{
	while(1)
	{
		// Check if tree is empty:

		if(!has_tasks_k(pool[0], degree))
		{
			return -1;
		}

		// Returns either a node with an existing, non-grabbed task, or a node with empty children:

		int index = -1;
		int status = find_node_for_get(pool, degree, tree_height, &index, precomputed_level_sizes);

		// This is the case when find_node_for_get() returned a node with empty children.
		// Update its ancestor's metadata and keep trying to get a task from the current tree:

		if(status < 0)
		{
#ifdef TASKPOOL_TRACE
			printf("find_node_for_get() found no tasks (index=%d)\n", index);
#endif

			update_node_metadata(index, pool, degree, 0);

			continue;
		}

		// We have a valid node index with a task. Attempt to grab it:

		unsigned char old = LOAD(pool[index].grabbed);

#ifdef TASKPOOL_TRACE
		printf("find_node_for_get() found a task at index=%d, task=%ld, grabbed=%d, dirty=%d\n", index, (long) pool[index].data, old, atomic_load(&pool[index].dirty));
#endif
		if(old == 0 && CAS(&pool[index].grabbed, &old, 1))
		{
			*task = pool[index].data;
			pool[index].data=(WORD) NULL;

			if(index > 0)
				update_node_metadata(index, pool, degree, 0);

			return 0;
		}
		else // Didn't manage to grab the task. Keep trying to get a task from the current tree:
		{
			continue;
		}
	}
}

concurrent_tree_pool_node * allocate_tree_pool(int tree_height, int degree, int * precomputed_level_sizes)
{
	int total_nodes, total_size;

	if(precomputed_level_sizes!=NULL)
		total_nodes = precomputed_level_sizes[tree_height-1];
	else
		total_nodes = CALCULATE_TREE_SIZE(tree_height, degree);

	total_size = total_nodes * sizeof(struct concurrent_tree_pool_node);

	concurrent_tree_pool_node * tpn = (concurrent_tree_pool_node *) malloc(total_size);

	if(tpn)
	{
		memset(tpn, 0, total_size);

#ifdef TASKPOOL_DEBUG
		printf("Allocated tree of size %d/%d\n", total_nodes, total_size);
#endif
	}
	else
	{
		printf("Failed to allocate tree of size %d/%d\n", total_nodes, total_size);
	}

	return tpn;
}

void free_tree_pool(concurrent_tree_pool_node * p)
{
	free(p);
}



