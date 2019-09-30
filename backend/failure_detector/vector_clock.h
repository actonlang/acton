/*
 * vector_clock.h
 *
 * Author: aagapi
 */

#ifndef BACKEND_FAILURE_DETECTOR_VECTOR_CLOCK_H_
#define BACKEND_FAILURE_DETECTOR_VECTOR_CLOCK_H_

#define DEFAULT_SIZE 16
#define GROWTH_RATE 2.0

// Sets in found_idx index of node_id (if found), or first smallest index (if not found):
#define BINARY_SEARCH_NODEID(vc, node_id, found_idx, exact_match)					\
	   (exact_match) = 0; 			  												\
	   for(int first = 0, last = (vc)->no_nodes - 1, middle = (first+last)/2;first <= last; middle = (first + last)/2) \
	   { 																			\
		   if((vc)->node_ids[middle].node_id == (node_id)) 							\
		   { 								  										\
			   (found_idx) = middle; 			  									\
			   (exact_match) = 1; 			  										\
			   break; 						  										\
		   } 								  										\
		   else if((vc)->node_ids[middle].node_id < (node_id)) 						\
		   { 								  										\
			   (found_idx) = middle; 			  									\
			   first = middle + 1; 				  									\
		   } 								  										\
		   else 								  										\
			   last = middle - 1; 													\
	   } 								  											\

typedef struct versioned_id
{
	int node_id;
	long counter;
} versioned_id;

typedef struct vector_clock
{
	int no_nodes;
	int capacity;
	versioned_id * node_ids;
} vector_clock;

int increment(vector_clock * vc, int node_id);

int compare(vector_clock * vc1, vector_clock * vc2);

int update(vector_clock * vc_dest, vector_clock * vc_src);

int add_component(vector_clock * vc, int node_id, int initial_counter);

int remove_component(vector_clock * vc, int node_id);

vector_clock * init_vc(int init_no_nodes, int * node_ids, long * counters, int sort_node_ids);

void free_vc(vector_clock * vc);


#endif /* BACKEND_FAILURE_DETECTOR_VECTOR_CLOCK_H_ */
