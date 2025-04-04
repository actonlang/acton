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
 * queue_groups.c
 *      Author: aagapi
 */

#include "backend/queue_groups.h"

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
    consumer_state * cs = get_consumer_state(consumer_id, shard_id, app_id, group->group_id, callback, sockfd, 1);

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

int lookup_listener_in_group(group_state * group, WORD consumer_id, WORD queue_id, consumer_state ** cs)
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



