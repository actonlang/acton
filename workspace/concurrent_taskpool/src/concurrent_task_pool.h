/*
 * A concurrent, scalable, wait-free task pool implementation that performs well under high CAS contention
 *
 *  Created on: 22 Jan 2019
 *      Author: aagapi
 */

#ifndef KERNEL_CONCURRENT_TASK_POOL_H_
#define KERNEL_CONCURRENT_TASK_POOL_H_

#include <stdatomic.h>
#include "fastrand.h"

#define DEFAULT_TREE_HEIGHT 3
#define DEFAULT_K_NO_TRIALS 4
#define MAX_DEGREE 64
#define DEFAULT_DEGREE MAX_DEGREE

#define DEFAULT_PREALLOCATED_ELEMENTS 1000000
#define SLACK_PREALLOCATED_ELEMENTS 1

#define PRECALCULATE_TREE_LEVEL_SIZES

#define CALCULATE_TREE_SIZE(h,d) ((((int) pow(d, h)) - 1) / (d - 1))
#define TREE_FILL_FACTOR(h,d,k) ((int)(pow((double)d,(((double)k+2)*h/(k+3))) / (d - 1))) // == (degree^^((k+2)/(k+3)*height))/(degree-1)
#define _NO_PREALLOCATED_TREES(no_elems, h,d,k) (((int) (SLACK_PREALLOCATED_ELEMENTS*(no_elems))) / (TREE_FILL_FACTOR(h,d,k)))
#define MAX(a, b) ((a>b)?(a):(b))
#define NO_PREALLOCATED_TREES(no_elems, h,d,k) (MAX(_NO_PREALLOCATED_TREES(no_elems, h,d,k),1))

// Macros for version handling (ABA etc):

#define EMPTY_0_VERSION 0
#define FULL_0_VERSION (1 << 31)
#define IS_EMPTY(n) (!((n) & (1 << 31)))
#define VERSION(n) ((n) & ~(1 << 31))
#define NEW_VERSION(n, v) ((v) | ((n) & (1 << 31)))

#define EMPTY_0_VERSION_BYTE 0
#define FULL_0_VERSION_BYTE ((unsigned char) (1 << 7))
#define IS_EMPTY_BYTE(n) (!((n) & (1 << 7)))
#define VERSION_BYTE(n) ((n) & ~(1 << 7))
#define NEW_VERSION_BYTE(n, v) ((v) | ((n) & (1 << 7)))

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

#define CHILD_K(p,k,d) ((d)*(p) + (k) + 1)
#define PARENT_K(i,d) (((i)-1)/d)

// Macro points to the array of TREE_SIZE elements, holding the actual tree data (struct concurrent_tree_pool_node *),
// allocated contiguously in the same mem chunk, right after the corresponding tree pool metadata (struct concurrent_pool_node):

#define TREE_PTR(ptr) ((struct concurrent_tree_pool_node *) (&ptr[1]))

// CAS and atomic load utility macros:

// #define USE_ATOMIC_LOAD
// #define USE_MEMORY_ORDER_RELAXED

#ifdef USE_ATOMIC_LOAD
#ifdef USE_MEMORY_ORDER_RELAXED
#define LOAD(a) (atomic_load_explicit(&(a), memory_order_relaxed))
#else
#define LOAD(a) (atomic_load_(&(a)))
#endif
#else
#define LOAD(a) (a)
#endif

#define USE_WEAK_CAS_IN_LOOPS
// #define USE_WEAK_CAS_OUT_OF_LOOPS

#ifdef USE_WEAK_CAS_IN_LOOPS
#define LOOP_CAS atomic_compare_exchange_weak
#else
#define LOOP_CAS atomic_compare_exchange_strong
#endif

#ifdef USE_WEAK_CAS_OUT_OF_LOOPS
#define CAS atomic_compare_exchange_weak
#else
#define CAS atomic_compare_exchange_strong
#endif

// #define FAST_PUT
// #define FAST_GET
// #define FAST_GET_PER_LEVEL

// #define TASKPOOL_DEBUG
// #define TASKPOOL_TRACE

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

	concurrent_pool_node_ptr _last_prealloc_block;
} concurrent_pool;

typedef struct concurrent_tree_pool_node
{
	atomic_uchar child_has_tasks[MAX_DEGREE];
	atomic_uchar dirty;		// any task was placed in this node?
	atomic_uchar grabbed;	// was this task grabbed?
	atomic_uint pending;		// pending concurrent updates to parent

	WORD data;					// actual task
} concurrent_tree_pool_node;

// Actual array of "struct concurrent_tree_pool_node" tree nodes (array representation of the complete tree)
// is allocated right after the "struct concurrent_pool_node" metadata, in the same mem chunk,
// and accessible via the TREE_PTR) macro:

typedef struct concurrent_pool_node
{
	int node_id;
	atomic_concurrent_pool_node_ptr next;
} concurrent_pool_node;

// "Public" API:

// Functions to allocate a taskpool:

// Set 'no_prealloc' if you have an idea of the typical number of items a pool will hold, to optimize mem allocations.
// Otherwise, set 'no_prealloc' to something < 0 and a default value will be used.
// Set 'degree' and 'tree_height' to choose custom values for the tree degree and height, respectively.
// Degree must be smaller than MAX_DEGREE.

concurrent_pool * allocate_pool(int no_prealloc);
concurrent_pool * allocate_pool_with_tree_height(int tree_height, int no_prealloc);
concurrent_pool * allocate_pool_with_tree_height_and_degree(int tree_height, int degree, int no_prealloc);
void free_pool(concurrent_pool * p);

// Functions to manipulate a taskpool:

// Args:
//	- task: void* allocated and managed by the app
//	- pool: ptr to taskpool
//	- fastrandstate: *Always* pass a valid pointer to the fast random generator state to each call.
//					Before starting library use in each thread, seed this state via GET_RANDSEED(&fastrandstate, apprand),
//					where apprand is an (optional) per-thread unique state. If not available, just call GET_RANDSEED(&fastrandstate, 0) to seed
//					Use the same pointer on each subsequent call to all taskpool library functions

int put(WORD task, concurrent_pool* pool, unsigned int * fastrandstate);
int get(WORD* task, concurrent_pool* pool, unsigned int * fastrandstate);
int get_last_block_id(concurrent_pool * p);
void set_no_trials(concurrent_pool * pool, int no_trials);

// Lower level API to access tree pools directly:

concurrent_tree_pool_node * allocate_tree_pool(int tree_height, int degree, int * precomputed_level_sizes);
void free_tree_pool(concurrent_tree_pool_node * p);
int put_in_tree(WORD task, concurrent_tree_pool_node* pool, int degree, int tree_height, int k_no_trials, int * precomputed_level_sizes, unsigned int * fastrandstate);
int get_from_tree(WORD* task, concurrent_tree_pool_node* pool, int degree, int tree_height, int * precomputed_level_sizes, unsigned int * fastrandstate);
int preallocate_trees(concurrent_pool* pool, int no_trees, int per_tree_data_size, char * prealloc_mem);

#endif /* KERNEL_CONCURRENT_TASK_POOL_H_ */
