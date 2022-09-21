/*
 * queue_groups.h
 *      Author: aagapi
 */

#ifndef BACKEND_QUEUE_GROUPS_H_
#define BACKEND_QUEUE_GROUPS_H_

#include "common.h"
#include "queue_callback.h"
#include "queue.h"
#include "skiplist.h"

#define GROUP_STATUS_ACTIVE 0
#define GROUP_STATUS_INACTIVE 1

typedef struct group_state {
	WORD group_id;
	skiplist_t * queue_tables;
	skiplist_t * consumers;
	pthread_mutex_t* group_lock;
	int status;
} group_state;

// Queue group management fctns:

group_state * get_group(WORD group_id);
int delete_group(group_state * group);
int clear_group(group_state * group);
void activate_group(group_state * group);
void deactivate_group(group_state * group);
int add_queue_to_group(group_state * group, WORD table_key, WORD queue_id, unsigned int * fastrandstate);
int remove_queue_from_group(group_state * group, WORD table_key, WORD queue_id);
int add_listener_to_group(group_state * group,
						WORD consumer_id, WORD shard_id, WORD app_id,
						queue_callback * callback,
						int * sockfd,
						unsigned int * fastrandstate);
int remove_listener_from_group(group_state * group, WORD consumer_id);
int lookup_listener_in_group(group_state * group, WORD consumer_id, consumer_state ** cs);
int is_queue_in_group(group_state * group, WORD table_key, WORD queue_id);
void free_group_state(WORD gs);
WORD get_group_state_key(WORD rs);
WORD get_group_state_live_field(WORD rs);

#endif /* BACKEND_QUEUE_GROUPS_H_ */
