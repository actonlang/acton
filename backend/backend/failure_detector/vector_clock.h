/*
 * vector_clock.h
 *
 * Author: aagapi
 */

#ifndef BACKEND_FAILURE_DETECTOR_VECTOR_CLOCK_H_
#define BACKEND_FAILURE_DETECTOR_VECTOR_CLOCK_H_

#define DEFAULT_SIZE 8
#define GROWTH_RATE 2.0

#define VC_INCOMPARABLE -2
#define VC_DISJOINT -3

#include "backend/failure_detector/db_messages.pb-c.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <inttypes.h>

// Sets in found_idx index of node_id (if found), or first smallest index (if not found):
#define BINARY_SEARCH_NODEID(vc, node_id, found_idx, exact_match)                       \
       (exact_match) = 0;                                                           \
       (found_idx) = -1;                                                                \
       int first = 0, last = (vc)->no_nodes - 1;                                    \
       int middle = (first+last)/2;                                                     \
       for(;first <= last; middle = (first + last)/2)                               \
       {                                                                            \
           if((vc)->node_ids[middle].node_id == (node_id))                          \
           {                                                                        \
               (found_idx) = middle;                                                \
               (exact_match) = 1;                                                   \
               break;                                                               \
           }                                                                        \
           else if((vc)->node_ids[middle].node_id < (node_id))                      \
           {                                                                        \
               (found_idx) = middle;                                                \
               first = middle + 1;                                                  \
           }                                                                        \
           else                                                                         \
               last = middle - 1;                                                   \
       }                                                                            \

typedef struct versioned_id
{
    int node_id;
    int64_t counter;
} versioned_id;

typedef struct vector_clock
{
    int no_nodes;
    int capacity;
    versioned_id * node_ids;
} vector_clock;

int increment_vc(vector_clock * vc, int node_id);

int compare_vc(vector_clock * vc1, vector_clock * vc2);

int update_vc(vector_clock * vc_dest, vector_clock * vc_src);

int update_or_replace_vc(vector_clock ** vc_dest, vector_clock * vc_src);

int add_component_vc(vector_clock * vc, int node_id, int initial_counter);

int64_t get_component_vc(vector_clock * vc, int node_id);

int remove_component_vc(vector_clock * vc, int node_id);

vector_clock * init_vc(int init_no_nodes, int * node_ids, int64_t * counters, int sort_node_ids);

vector_clock * init_empty_vc();

vector_clock * init_local_vc_id(int local_id);

vector_clock * init_local_vc(struct sockaddr * x);

vector_clock * copy_vc(vector_clock * vc1);

vector_clock * init_vc_from_msg(VectorClockMessage * msg);

void init_vc_msg(VectorClockMessage * msg_ptr, vector_clock * vc);

void free_vc_msg(VectorClockMessage * msg);

void free_vc(vector_clock * vc);

int grow_vc(vector_clock * vc);

// int serialize_vc(vector_clock * vc, void ** buf, unsigned * len);

// int deserialize_vc(void * buf, unsigned msg_len, vector_clock ** vc);

char * to_string_vc(vector_clock * vc, char * msg_buff);

int get_node_id(struct sockaddr * x);

#endif /* BACKEND_FAILURE_DETECTOR_VECTOR_CLOCK_H_ */
