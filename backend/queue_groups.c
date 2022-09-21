/*
 * queue_groups.c
 *      Author: aagapi
 */

#include "queue_groups.h"

// Queue group management fctns:

group_state * get_group(WORD group_id)
{
	group_state * group = (group_state *) malloc(sizeof(group_state) + sizeof(pthread_mutex_t));
	group->group_id = group_id;
	group->consumers = create_skiplist_long();
	group->queue_tables = create_skiplist_long();
	group->status = GROUP_STATUS_ACTIVE;
	group->group_lock = (pthread_mutex_t*) ((char*) group + sizeof(group_state));
	pthread_mutex_init(group->group_lock, NULL);
	return group;
}

void free_group_state(WORD gs)
{
	delete_group((group_state *) gs);
}

WORD get_group_state_key(WORD rs)
{
	return ((group_state *) rs)->group_id;
}
WORD get_group_state_live_field(WORD rs)
{
	return (WORD) (&(((group_state *) rs)->status));
}

int clear_group(group_state * group)
{
	skiplist_free_val(group->consumers, free_group_state);
	group->consumers = create_skiplist_long();

	skiplist_free_val(group->queue_tables, free_queue_table_state);
	group->queue_tables = create_skiplist_long();

	return 0;
}

int delete_group(group_state * group)
{
	skiplist_free_val(group->consumers, free_group_state);
	skiplist_free_val(group->queue_tables, free_queue_table_state);
	free(group);
	return 0;
}

void activate_group(group_state * group)
{
	group->status = GROUP_STATUS_ACTIVE;
}

void deactivate_group(group_state * group)
{
	group->status = GROUP_STATUS_INACTIVE;
}

int add_queue_to_group(group_state * group, WORD table_key, WORD queue_id, unsigned int * fastrandstate)
{
	skiplist_t * queue_list = NULL;
	pthread_mutex_lock(group->group_lock);
	snode_t * table_list = skiplist_search(group->queue_tables, table_key);
	if(table_list != NULL)
	{
		skiplist_t * queue_list = (skiplist_t *) (table_list->value);
	}
	else
	{
		queue_list = create_skiplist_long();
		skiplist_insert(group->queue_tables, table_key, queue_list, fastrandstate);
	}
	skiplist_insert(queue_list, queue_id, queue_id, fastrandstate);
	pthread_mutex_unlock(group->group_lock);
	return 0;
}

int remove_queue_from_group(group_state * group, WORD table_key, WORD queue_id)
{
	pthread_mutex_lock(group->group_lock);
	snode_t * table_list = skiplist_search(group->queue_tables, table_key);
	if(table_list == NULL)
	{
		pthread_mutex_unlock(group->group_lock);
		return DB_ERR_NO_TABLE;
	}
	skiplist_t * queue_list = (skiplist_t *) (table_list->value);
	if(queue_list != NULL)
	{
		snode_t * queue_entry = skiplist_delete(queue_list, queue_id);
		pthread_mutex_unlock(group->group_lock);
		if(queue_entry == NULL)
			return DB_ERR_NO_QUEUE;
	}
	else
	{
		pthread_mutex_unlock(group->group_lock);
		return DB_ERR_NO_QUEUE;
	}
	return 0;
}

int is_queue_in_group(group_state * group, WORD table_key, WORD queue_id)
{
	pthread_mutex_lock(group->group_lock);
	snode_t * table_list = skiplist_search(group->queue_tables, table_key);
	if(table_list == NULL)
	{
		pthread_mutex_unlock(group->group_lock);
		return 0;
	}
	skiplist_t * queue_list = (skiplist_t *) (table_list->value);
	if(queue_list != NULL)
	{
		snode_t * queue_entry = skiplist_search(queue_list, queue_id);
		pthread_mutex_unlock(group->group_lock);
		if(queue_entry == NULL)
			return 0;
	}
	else
	{
		pthread_mutex_unlock(group->group_lock);
	}
	return 1;
}

int add_listener_to_group(group_state * group,
						WORD consumer_id, WORD shard_id, WORD app_id,
						queue_callback * callback,
						int * sockfd,
						unsigned int * fastrandstate)
{
	consumer_state * cs = (consumer_state *) malloc(sizeof(consumer_state));
	cs->consumer_id = consumer_id;
	cs->shard_id = shard_id;
	cs->app_id = app_id;
	cs->group_id = group->group_id;
	cs->private_read_head = -1;
	cs->private_consume_head = -1;
	cs->callback = callback;
	cs->sockfd = sockfd;
	cs->notified=0;
	cs->prh_version=NULL;
	cs->pch_version=NULL;

	pthread_mutex_lock(group->group_lock);

	snode_t * consumer_node = skiplist_search(group->consumers, consumer_id);

	int ret = 0;
	if(consumer_node != NULL)
	{
		ret = DB_ERR_DUPLICATE_CONSUMER; // We allow the consumer to update his previously existing callback and meta-data, but advise him that he has done so
	}

	skiplist_insert(group->consumers, consumer_id, cs, fastrandstate);

	pthread_mutex_unlock(group->group_lock);

	return ret;
}

int remove_listener_from_group(group_state * group, WORD consumer_id)
{
	pthread_mutex_lock(group->group_lock);

	snode_t * consumer_node = skiplist_delete(group->consumers, consumer_id);

	pthread_mutex_unlock(group->group_lock);

	if(consumer_node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer didn't exist
	return 0;
}

int lookup_listener_in_group(group_state * group, WORD consumer_id, consumer_state ** cs)
{
    pthread_mutex_lock(group->group_lock);

    snode_t * consumer_node = skiplist_search(group->consumers, consumer_id);

    pthread_mutex_unlock(group->group_lock);

    if(consumer_node == NULL)
    {
        *cs = NULL;

        return DB_ERR_NO_CONSUMER; // Consumer didn't exist
    }

    *cs = (consumer_state *) consumer_node->value;

    return 0;
}



