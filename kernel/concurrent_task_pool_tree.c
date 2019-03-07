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
#include "fastrand.c"

int random_in_level(int level, int * precomputed_level_sizes)
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
		first_in_level = CALCULATE_TREE_SIZE(level);
		last_in_level = CALCULATE_TREE_SIZE(level+1) - 1;
	}

	return (fastrand() % (last_in_level - first_in_level + 1)) + first_in_level;
}

void set_version(unsigned int* meta, unsigned int version)
{
	*meta = NEW_VERSION(*meta, version);
}

int has_tasks(concurrent_tree_pool_node node)
{
	return !IS_EMPTY(node.tasks_left) || !IS_EMPTY(node.tasks_right) || (node.data != NULL && !atomic_load(&node.grabbed));
}

int update_father(int index, concurrent_tree_pool_node* pool, unsigned char value)
{
	atomic_fetch_add(&(pool[index].pending), 1);
	int parent_index=PARENT(index);
	int am_left_child = index % 2;
	unsigned int old = 0, new = 0;
	int success = 0;

	old = atomic_load(am_left_child?(&pool[parent_index].tasks_left):(&pool[parent_index].tasks_right));
	new = (value)?(FULL_0_VERSION):(EMPTY_0_VERSION);
	new = NEW_VERSION(new, VERSION(old) + 1);

	success = atomic_compare_exchange_strong(am_left_child?(&pool[parent_index].tasks_left):(&pool[parent_index].tasks_right), &old, new);

#ifdef TASKPOOL_DEBUG
	printf("update_father(): Updating parent index %d of index %d from (%d, %d, %d) to (%d, %d, %d) returned %d\n",
			parent_index, index,
			old, IS_EMPTY(old), VERSION(old),
			new, IS_EMPTY(new), VERSION(new),
			success);
#endif

	atomic_fetch_add(&(pool[index].pending), -1);

	return success;
}

void update_node_metadata(int index, concurrent_tree_pool_node* pool, unsigned char value)
{
	int trials = 0, crt_index = index;

#ifdef TASKPOOL_DEBUG
	printf("update_node_metadata(index=%d, value=%d)\n", index, value);
#endif

	while(HAS_PARENT(crt_index))
	{
		int have_tasks = has_tasks(pool[crt_index]);

		if(value != have_tasks)
		{
#ifdef TASKPOOL_DEBUG
			printf("update_node_metadata(): Skipping because my operation has been eliminated: value=%d != has_tasks(%d)=%d\n", value, crt_index, have_tasks);
#endif
			return;
		}

		int parent_index=PARENT(crt_index);
		concurrent_tree_pool_node parent_node = pool[parent_index];
		int am_left_child = crt_index % 2, needs_update;

		if(am_left_child)
			needs_update = (IS_EMPTY(pool[parent_index].tasks_left) == have_tasks);
		else
			needs_update = (IS_EMPTY(pool[parent_index].tasks_right) == have_tasks);

		if(needs_update || pool[crt_index].pending > 0)
		{
#ifdef TASKPOOL_DEBUG
			printf("update_node_metadata(index=%d, value=%d), updating parent index %d\n", index, value, parent_index);
#endif

			trials++;

			if(!update_father(crt_index, pool, value))
			{
#ifdef TASKPOOL_DEBUG
				printf("update_node_metadata(index=%d, value=%d), update_father() failed on parent index %d, trials=%d\n", index, value, parent_index, trials);
#endif
				if(trials < 2)
					continue;
			}
		}
		else
		{
#ifdef TASKPOOL_DEBUG
			printf("update_node_metadata(index=%d, value=%d), skipping update of parent index %d because pool[%d].pending=%d, am_left_child=%d, parent_empty_left=%d, parent_empty_right=%d, has_tasks=%d\n",
					index, value, parent_index,
					crt_index, pool[crt_index].pending,
					am_left_child, IS_EMPTY(pool[parent_index].tasks_left), IS_EMPTY(pool[parent_index].tasks_right),
					have_tasks);
#endif
			}

		crt_index = parent_index;

		trials = 0;
	}
}

int put_in_node(int index, concurrent_tree_pool_node* pool, WORD task)
{
	int crt_index = index;

	while(HAS_PARENT(crt_index) && pool[PARENT(crt_index)].data==NULL)
		crt_index=PARENT(crt_index);

	unsigned char old = atomic_load(&pool[crt_index].dirty);

	if(old == 0 && atomic_compare_exchange_strong(&pool[crt_index].dirty, &old, 1))
	{
		pool[crt_index].data = task;

		return crt_index;
	}
	else
	{
		return -1;
	}
}

int find_node_for_put(WORD task, concurrent_tree_pool_node* pool, int tree_height, int k_no_trials, int * precomputed_level_sizes)
{
	for(int level=1;level<tree_height;level++)
	{
		int no_trials = ((level==tree_height-1)?k_no_trials:1);

		for(int trials=0;trials<no_trials;trials++)
		{
			int index=put_in_node(random_in_level(level, precomputed_level_sizes), pool, task);
			if(index!=-1)
				return index;
		}
	}

	return -1;
}

int put_in_tree(WORD task, concurrent_tree_pool_node* pool, int tree_height, int k_no_trials, int * precomputed_level_sizes)
{
	// Handle root insertions separately for higher speed:

	unsigned char old = atomic_load(&pool[0].dirty);

	if(old == 0)
	{
		if(atomic_compare_exchange_strong(&pool[0].dirty, &old, 1))
		{
			pool[0].data = task;
			return 0;
		}
	}

	if(tree_height==1)
	{
		return -1;
	}

	int index = find_node_for_put(task, pool, tree_height, k_no_trials, precomputed_level_sizes);

	if(index==-1)
		return -1;

	update_node_metadata(index, pool, 1);

	return index;
}

// Returns either a node with an existing, non-grabbed task, or a node with empty children:

int find_node_for_get(concurrent_tree_pool_node* pool, int * index_p)
{
	int index = 0;

	while(1)
	{
#ifdef TASKPOOL_DEBUG
		printf("find_node_for_get: index=%d, data=%ld, grabbed=%d, dirty=%d, left_empty=(%d, %d, %d), right_empty=(%d, %d, %d)\n",
				index, (long) pool[index].data, atomic_load(&pool[index].grabbed), atomic_load(&pool[index].dirty),
				pool[index].tasks_left, IS_EMPTY(pool[index].tasks_left), VERSION(pool[index].tasks_left),
				pool[index].tasks_right, IS_EMPTY(pool[index].tasks_right), VERSION(pool[index].tasks_right));
#endif

		// Return this node's task if it exists:

		if((pool[index].data != NULL) && (atomic_load(&pool[index].grabbed) == 0))
		{
			*index_p = index;
			return 0;
		}

		int empty_left = IS_EMPTY(pool[index].tasks_left);
		int empty_right = IS_EMPTY(pool[index].tasks_right);

		if(empty_left && empty_right)
		{
			*index_p = index;
			return -1;
		}

		if(!empty_left && empty_right)
		{
			index = LEFT_CHILD(index);
		}
		else if(empty_left && !empty_right)
		{
			index = RIGHT_CHILD(index);
		}
		else
		{
			index = (fastrand() % 2)?(LEFT_CHILD(index)):(RIGHT_CHILD(index));
		}
	}
}

int get_from_tree(WORD* task, concurrent_tree_pool_node* pool)
{
	while(1)
	{
		// Check if tree is empty:

		if(!has_tasks(pool[0]))
		{
			return -1;
		}

		// Returns either a node with an existing, non-grabbed task, or a node with empty children:

		int index = -1;
		int status = find_node_for_get(pool, &index);

		// This is the case when find_node_for_get() returned a node with empty children.
		// Update its ancestor's metadata and keep trying to get a task from the current tree:

		if(status < 0)
		{
#ifdef TASKPOOL_DEBUG
			printf("find_node_for_get() found no tasks (index=%d)\n", index);
#endif

			update_node_metadata(index, pool, 0);

			continue;
		}

		// We have a valid node index with a task. Attempt to grab it:

		unsigned char old = atomic_load(&pool[index].grabbed);

#ifdef TASKPOOL_DEBUG
		printf("find_node_for_get() found a task at index=%d, task=%ld, grabbed=%d, dirty=%d\n", index, (long) pool[index].data, old, atomic_load(&pool[index].dirty));
#endif
		if(old == 0 && atomic_compare_exchange_strong(&pool[index].grabbed, &old, 1))
		{
			*task = pool[index].data;

			if(index > 0)
				update_node_metadata(index, pool, 0);

			return 0;
		}
		else // Didn't manage to grab the task. Keep trying to get a task from the current tree:
		{
			continue;
		}
	}
}

concurrent_tree_pool_node * allocate_tree_pool(int tree_height, int * precomputed_level_sizes)
{
	int total_size;

	if(precomputed_level_sizes!=NULL)
		total_size = precomputed_level_sizes[tree_height-1] * sizeof(struct concurrent_tree_pool_node);
	else
		total_size = CALCULATE_TREE_SIZE(tree_height) * sizeof(struct concurrent_tree_pool_node);

	concurrent_tree_pool_node * tpn = (concurrent_tree_pool_node *) malloc(total_size);

	memset(tpn, 0, total_size);

	return tpn;
}

void free_tree_pool(concurrent_tree_pool_node * p)
{
	free(p);
}



