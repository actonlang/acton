/*
 * A concurrent, scalable, wait-free task pool implementation that performs well under high CAS contention
 *
 *  Created on: 22 Jan 2019
 *      Author: aagapi
 */

#ifndef KERNEL_CONCURRENT_TASK_POOL_H_
#define KERNEL_CONCURRENT_TASK_POOL_H_

#include <stdatomic.h>

#define DEFAULT_TREE_HEIGHT 5
#define DEFAULT_K_NO_TRIALS 8
#define DEFAULT_DEGREE 2
#define MAX_DEGREE 2

#define PRECALCULATE_TREE_LEVEL_SIZES
#define NO_PREALLOCATED_ELEMENTS 20000000

#define CALCULATE_TREE_SIZE(h,d) (((int) pow(d, h)) - 1)
#define _NO_PREALLOCATED_TREES(h,d) (((int) (d * NO_PREALLOCATED_ELEMENTS)) / (CALCULATE_TREE_SIZE(h,d)))
#define MAX(a, b) ((a>b)?(a):(b))
#define NO_PREALLOCATED_TREES(h,d) (MAX(_NO_PREALLOCATED_TREES(h,d),1))

// Macros for version handling (ABA etc):

#define EMPTY_0_VERSION 0
#define FULL_0_VERSION (1 << 31)
#define IS_EMPTY(n) (!((n) & (1 << 31)))
#define VERSION(n) ((n) & ~(1 << 31))
#define NEW_VERSION(n, v) ((v) | ((n) & (1 << 31)))

#define BITMASK(len) ((1 << len) - 1)
// A 'slice' of a 32 bit integer, encompassing bits [start,start+len-1]:
#define SLICE(n,start,len) ((n >> (32 - start - len)) & ((1 << len) - 1))

// Macros for binary trees:

#define LEFT_CHILD(i) (2*(i)+1)
#define RIGHT_CHILD(i) (2*(i)+2)
#define PARENT(i) (((i)-1)/2)
#define HAS_PARENT(i) ((i)>0)

#define HAS_TASKS(node) (!IS_EMPTY(node.tasks_left) || !IS_EMPTY(node.tasks_right) || (node.data != NULL))

// Macros for k-ary trees:

#define CHILD_K(p,k,d) (d*(p) + (k) + 1)
#define PARENT_K(i,d) (((i)-1)/d)

// #define TASKPOOL_DEBUG

typedef void *WORD;

typedef struct concurrent_pool_node * concurrent_pool_node_ptr;
typedef _Atomic concurrent_pool_node_ptr atomic_concurrent_pool_node_ptr;

typedef struct concurrent_pool_node_ptr_pair
{
	concurrent_pool_node_ptr prev;
	concurrent_pool_node_ptr crt;
} concurrent_pool_node_ptr_pair;

typedef _Atomic concurrent_pool_node_ptr_pair atomic_concurrent_pool_node_ptr_pair;

typedef struct concurrent_pool
{
	int tree_height;										// tree height for component complete tree pools
	int degree;											// tree degree for component complete tree pools
	int k_no_trials;										// number of retries upon a failed put (influences fill factor of component trees)

	concurrent_pool_node_ptr head; 						// head of concurrent tree list
	atomic_concurrent_pool_node_ptr producer_tree;		// pointer to current producer tree
	atomic_concurrent_pool_node_ptr_pair consumer_trees;	// pair of pointers to (previous ,current) consumer trees
	atomic_uint old_producers;							// producers that currently attempt to move producer_tree backwards

#ifdef PRECALCULATE_TREE_LEVEL_SIZES
	int * level_sizes;									// precomputed array of level sizes (used to speed up pool inserts)
#endif
} concurrent_pool;

typedef struct concurrent_tree_pool_node
{
	atomic_uint child_has_tasks[MAX_DEGREE];
//	atomic_uint * child_has_tasks; // any tasks in K-th child? (+ version)
//	atomic_uint tasks_left;
//	atomic_uint tasks_right;
	atomic_uchar dirty;		// any task was placed in this node?
	atomic_uchar grabbed;	// was this task grabbed?
	atomic_uint pending;		// pending concurrent updates to parent

//	atomic_uint * _child_has_tasks; // pointer containing all child_has_tasks regions for all nodes in a tree (to optimize malloc overhead)

	WORD data;					// actual task
} concurrent_tree_pool_node;

typedef struct concurrent_pool_node
{
	concurrent_tree_pool_node * tree;
	int node_id;
	atomic_concurrent_pool_node_ptr next;
} concurrent_pool_node;

// "Public" API:

concurrent_pool * allocate_pool();
concurrent_pool * allocate_pool_with_tree_height(int tree_height);
concurrent_pool * allocate_pool_with_tree_height_and_degree(int tree_height, int degree);
void set_tree_height(concurrent_pool * pool, int tree_height);
void set_tree_degree(concurrent_pool * pool, int degree);
void set_no_trials(concurrent_pool * pool, int no_trials);
void free_pool(concurrent_pool * p);
int put(WORD task, concurrent_pool* pool);
int get(WORD* task, concurrent_pool* pool);

// Lower level API to access tree pools directly:

concurrent_tree_pool_node * allocate_tree_pool(int tree_height, int degree, int * precomputed_level_sizes);
void free_tree_pool(concurrent_tree_pool_node * p);
int put_in_tree(WORD task, concurrent_tree_pool_node* pool, int tree_height, int degree, int k_no_trials, int * precomputed_level_sizes);
int get_from_tree(WORD* task, concurrent_tree_pool_node* pool, int degree);
int preallocate_trees(concurrent_pool* pool, int no_trees);

#endif /* KERNEL_CONCURRENT_TASK_POOL_H_ */
