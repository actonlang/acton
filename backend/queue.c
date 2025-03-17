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
 * queue.c
 *
 *      Author: aagapi
 */

#include <limits.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "backend/queue.h"
#include "backend/log.h"
#include "backend/failure_detector/cells.h"
#include "backend/failure_detector/db_queries.h"

#define MIN(x, y) (((x)<(y))?(x):(y))
#define MAX(x, y) (((x)>(y))?(x):(y))

#define VERBOSITY 1
#define LOCK_VERBOSITY 0

db_table_t * get_table_by_key(WORD table_key, db_t * db)
{
    snode_t * node = skiplist_search(db->tables, table_key);
    if(node == NULL)
        return NULL;

    return (db_table_t *) (node->value);
}

int create_queue_table(WORD table_id, int no_cols, int * col_types, db_t * db, unsigned int * fastrandstate) {
    int primary_key_idx = 0; // queue_id
    int clustering_key_idx = 1; // entry_id
    int total_cols = no_cols+2;

    int * total_col_types = (int *) malloc(total_cols * sizeof(int));

    for(int i=0;i<total_cols;i++)
        total_col_types[i] = ((i<2)?(DB_TYPE_INT64):(col_types[i-2]));

    db_schema_t* db_schema = db_create_schema(total_col_types, total_cols,
                                            &primary_key_idx, 1,
                                            &clustering_key_idx, 1,
                                            NULL, 0);

    assert(db_schema != NULL && "Schema creation failed");

    // Create queue table:

    int ret = db_create_table(table_id, db_schema, db, fastrandstate);

#if (VERBOSITY > 0)
    log_info("Queue table %" PRId64 " created", (int64_t) table_id);
#endif

    return ret;
}

// Functions for handling remote notifications:

int get_queue_notification_packet(WORD table_key, WORD queue_id, WORD app_id, WORD shard_id, WORD consumer_id, WORD group_id,
                                    int64_t new_no_entries, int status,
                                    void ** snd_buf, unsigned * snd_msg_len)
{
    cell_address ca;

    copy_cell_address(&ca, (int64_t) table_key, (int64_t *) &queue_id, 1);

    queue_query_message * m = init_queue_notification(&ca, NULL, 0, (int) app_id, (int) shard_id, (int) consumer_id, (int) group_id, new_no_entries, status, NULL, -1);

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_queue_message(m, (char *) print_buff);
    log_debug("Sending queue notification message: %s", print_buff);
#endif

    int ret = serialize_queue_message(m, snd_buf, snd_msg_len, 0, NULL);

    assert(ret == 0);

    free_queue_message(m);

    return ret;
}

int notify_remote_queue_subscribers(WORD table_key, WORD queue_id, db_t * db)
{
    db_table_t * table = get_table_by_key(table_key, db);
    int status = 0;

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    // Notify remote subscribers if they haven't been notified:

    for(snode_t * cell=HEAD(db_row->consumer_state);cell!=NULL;cell=NEXT(cell))
    {
        if(cell->value != NULL)
        {
            consumer_state * cs = (consumer_state *) (cell->value);

            if(cs->callback != NULL || cs->notified > 0) // Skip local subscribers or ones that have already been notified:
                continue;

            assert(cs->sockfd != NULL);

            if(*(cs->sockfd) == 0)
            {
#if (VERBOSITY > 0)
                log_debug("SERVER: Skipping notifying disconnected subscriber %" PRId64 "", (int64_t) cs->consumer_id);
                continue;
#endif
            }

            void * snd_buf = NULL;
            unsigned snd_msg_len = -1;
            status = get_queue_notification_packet(table_key, queue_id, cs->app_id, cs->shard_id,
                                                        cs->consumer_id, cs->group_id,
                                                        db_row->no_entries, 0,
                                                        &snd_buf, &snd_msg_len);
            assert(status == 0);

            int n = write(*(cs->sockfd), snd_buf, snd_msg_len);
            if (n < 0)
            {
                    log_error("ERROR writing notification to socket!");
                    continue;
            }

            free(snd_buf);

            cs->notified=1;

#if (VERBOSITY > 0)
            log_debug("SERVER: Notified remote subscriber %" PRId64 "", (int64_t) cs->consumer_id);
#endif
        }
    }

    return status;
}

void notify_subscriber(consumer_state * cs, WORD table_key, WORD queue_id, db_row_t * db_row)
{
    int status = 0, ret = 0;

    if(cs->group_id >= 0) // Skip group subscribers (these will be notified via their group)
    {
// #if (VERBOSITY > 0)
//        log_debug("BACKEND: Skipping notifying group subscriber %" PRId64 " for queue %d because it will be notified via group notifications", (int64_t) cs->consumer_id, (int) queue_id);
// #endif
        return;
    }

    if(cs->notified > 0) // Skip already notified subscribers (whether local or remote)
    {
// #if (VERBOSITY > 0)
//        log_debug("BACKEND: Skipping notifying subscriber %" PRId64 " for queue %d because it was already notified", (int64_t) cs->consumer_id);
// #endif
        return;
    }

    if(cs->callback != NULL) // Local subscriber
    {
        assert(cs->sockfd == NULL);

        queue_callback_args * qca = get_queue_callback_args(table_key, queue_id, cs->app_id, cs->shard_id, cs->consumer_id, cs->group_id, QUEUE_NOTIF_ENQUEUED);

#if (VERBOSITY > 0)
        log_debug("BACKEND: Attempting to notify local subscriber %" PRId64 " (%p/%p/%p/%p)", (int64_t) qca->consumer_id, cs->callback, cs->callback->lock, cs->callback->signal, cs->callback->callback);
#endif

        ret = pthread_mutex_lock(cs->callback->lock);

#if (LOCK_VERBOSITY > 0)
        log_debug("BACKEND: Locked consumer lock of %" PRId64 " (%p/%p), status=%d", (int64_t) qca->consumer_id, cs->callback, cs->callback->lock, ret);
#endif

        pthread_cond_signal(cs->callback->signal);
        cs->callback->callback(qca);
        ret = pthread_mutex_unlock(cs->callback->lock);

#if (LOCK_VERBOSITY > 0)
        log_debug("BACKEND: Unlocked consumer lock of %" PRId64 " (%p/%p), status=%d", (int64_t) qca->consumer_id, cs->callback, cs->callback->lock, ret);
#endif
    }
    else // remote subscriber
    {
        assert(cs->sockfd != NULL);

        if(*(cs->sockfd) == 0)
        {
#if (VERBOSITY > 0)
            log_debug("SERVER: Skipping notifying disconnected remote subscriber %" PRId64 "", (int64_t) cs->consumer_id);
#endif
            return;
        }

        void * snd_buf = NULL;
        unsigned snd_msg_len = -1;
        status = get_queue_notification_packet(table_key, queue_id,
                                                cs->app_id, cs->shard_id, cs->consumer_id, cs->group_id,
                                                db_row->no_entries, 0,
                                                &snd_buf, &snd_msg_len);
        assert(status == 0);

        int n = write(*(cs->sockfd), snd_buf, snd_msg_len);
        if (n < 0)
        {
              fprintf(stderr, "ERROR writing notification to socket!\n");
              return;
        }

        free(snd_buf);
    }

    cs->notified=1;

#if (VERBOSITY > 0)
    log_debug("BACKEND: Notified %s subscriber %" PRId64 "", (cs->callback != NULL)?"local":"remote", (int64_t) cs->consumer_id);
#endif
}

void notify_subscribers(skiplist_t * subscriber_list, WORD table_key, WORD queue_id, db_row_t * db_row)
{
    for(snode_t * cell=HEAD(subscriber_list);cell!=NULL;cell=NEXT(cell))
    {
        if(cell->value != NULL)
        {
            consumer_state * cs = (consumer_state *) (cell->value);

            notify_subscriber(cs, table_key, queue_id, db_row);
        }
    }
}

int enqueue(WORD * column_values, int no_cols, size_t last_blob_size, WORD table_key, WORD queue_id, short use_lock, db_t * db, unsigned int * fastrandstate)
{
    db_table_t * table = get_table_by_key(table_key, db);
    int ret = 0;

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    if(use_lock)
    {
        pthread_mutex_lock(db_row->enqueue_lock);
    }

    int64_t entry_id = db_row->no_entries;
    db_row->no_entries++;

    if(use_lock)
    {
        pthread_mutex_unlock(db_row->enqueue_lock);
    }

    // Add queue_id as partition key and entry_id as clustering key:

    WORD * queue_column_values = (WORD *) malloc((no_cols + 2) * sizeof(WORD));
    queue_column_values[0]=queue_id;
    queue_column_values[1]=(WORD) entry_id;
    for(int64_t i=2;i<no_cols + 2;i++)
        queue_column_values[i]=column_values[i-2];

    int status = table_insert(queue_column_values, no_cols+2, 1, last_blob_size, NULL, table, fastrandstate);

#if (VERBOSITY > 0)
    log_debug("BACKEND: Inserted queue entry %" PRId64 " in queue %" PRId64 "/%" PRId64 ", status=%d", entry_id, (int64_t) table_key, (int64_t) queue_id, status);
#endif

    // Notify individual queue subscribers if they haven't been notified:

    notify_subscribers(db_row->consumer_state, table_key, queue_id, db_row);

    // Similarly notify all group subscribers:

    if(db_row->group_subscriptions != NULL)
    {
        if(db->queue_group_replication_factor > 1)
        {
            for(snode_t * cell=HEAD((skiplist_t *) db_row->group_subscriptions);cell!=NULL;cell=NEXT(cell))
            {
                group_state * gs = (group_state *) (cell->value);

                notify_subscribers(gs->consumers, table_key, queue_id, db_row);
            }
        }
        else
        {
            notify_subscribers(((group_state *) db_row->group_subscriptions)->consumers, table_key, queue_id, db_row);

#if (VERBOSITY > 0)
            log_debug("BACKEND: Notified group subscribers from group %d (%d subscribers)",
                            (int) ((group_state *) db_row->group_subscriptions)->group_id,
                            ((group_state *) db_row->group_subscriptions)->consumers->no_items);
#endif
        }
    }

    return status;
}

int lookup_consumer_state_in_group(WORD queue_id, WORD consumer_id, db_row_t * db_row,
                                    consumer_state ** cs, db_t * db)
{
    if(db_row->group_subscriptions == NULL) // Queue's group is already cached by 'auto_update_group_queue_subscriptions()', no need to call 'get_buckets_for_object()' again
    {
        log_debug("SERVER: lookup_consumer_state_in_group(%d), found no group state!", (int) queue_id);

        *cs = NULL;

        return DB_ERR_NO_GROUP;
    }

    if(db->queue_group_replication_factor == 1)
    {
        group_state * gs = (group_state *) db_row->group_subscriptions;

        log_debug("SERVER: lookup_consumer_state_in_group(%d, %d), found group state: group_id = %d, status = %d, consumers = %d",
                (int) queue_id, (int) consumer_id, ((gs != NULL)?((int) gs->group_id):(-1)),
                ((gs != NULL)?((int) gs->status):(-1)),
                ((gs != NULL)?((int) gs->consumers->no_items):(-1)));

        return lookup_listener_in_group(gs, consumer_id, queue_id, cs);
    }
    else
    {
        skiplist_t * gss = (skiplist_t *) db_row->group_subscriptions;

        for(snode_t * cell=HEAD(gss);cell!=NULL;cell=NEXT(cell))
        {
            if(cell->value != NULL)
            {
                group_state * gs = (group_state *) cell->value;

                int ret = lookup_listener_in_group(gs, consumer_id, queue_id, cs);

                if(ret == 0)
                {
                    log_debug("SERVER: lookup_consumer_state_in_group(%d), found group state: group_id = %d, status = %d, consumers = %d",
                            (int) queue_id, ((gs != NULL)?((int) gs->group_id):(-1)),
                            ((gs != NULL)?((int) gs->status):(-1)),
                            ((gs != NULL)?((int) gs->consumers->no_items):(-1)));

                    return 0;
                }
            }
        }

        return DB_ERR_NO_CONSUMER;
    }
}

int lookup_consumer_state_in_row_or_group(WORD queue_id, WORD consumer_id, db_row_t * db_row,
                                           consumer_state ** cs, consumer_state ** group_cs, db_t * db)
{
	*cs = NULL;
	*group_cs = NULL;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
	if(consumer_node == NULL)
		return DB_ERR_NO_CONSUMER;

	*cs = (consumer_state *) (consumer_node->value);

	lookup_consumer_state_in_group(queue_id, consumer_id, db_row, group_cs, db);

	return 0;
}

void sanity_check_consumer_read_heads(int64_t old_read_head, int64_t old_consume_head, int64_t new_read_head)
{
    assert(old_read_head <= new_read_head);

    assert(old_consume_head <= new_read_head);
}

void sanity_check_consumer_consume_heads(int64_t old_read_head, int64_t old_consume_head, int64_t new_consume_head)
{
    assert(old_read_head >= new_consume_head);

    assert(old_consume_head <= new_consume_head);

}

int is_consumer_notified(consumer_state * cs, consumer_state * gqcs)
{
	return (gqcs != NULL)?(gqcs->notified):(cs->notified);
}

void set_consumer_notified(consumer_state * cs, consumer_state * gqcs, int value)
{
    if(gqcs != NULL)
    	gqcs->notified=value;

    cs->notified=value;
}

int set_private_read_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                            int64_t new_read_head, vector_clock * version, short use_lock, db_t * db)
{
    db_table_t * table = get_table_by_key(table_key, db);
    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist
    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    int64_t no_entries = db_row->no_entries;

    consumer_state * cs = NULL, * gqcs = NULL;

    int ret = lookup_consumer_state_in_row_or_group(queue_id, consumer_id, db_row, &cs, &gqcs, db);

    if(ret != 0)
        return ret; // Consumer or group doesn't exist

    assert (cs != NULL || gqcs != NULL);

    if(use_lock)
    {
        pthread_mutex_lock(db_row->read_lock);
    }

    assert(new_read_head <= no_entries - 1);

    assert(version != NULL);

    int64_t old_read_head = READ_HEAD(cs), old_consume_head = CONSUME_HEAD(cs);

    sanity_check_consumer_read_heads(old_read_head, old_consume_head, new_read_head);

    cs->private_read_head = new_read_head;

    update_or_replace_vc(&(cs->prh_version), version);

    if(use_lock)
    {
        pthread_mutex_unlock(db_row->read_lock);
    }

    return 0;
}

int set_private_consume_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                            int64_t new_consume_head, vector_clock * version, db_t * db)
{
    db_table_t * table = get_table_by_key(table_key, db);
    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist
    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    int64_t no_entries = db_row->no_entries;

    consumer_state * cs = NULL, * gqcs = NULL;

    int ret = lookup_consumer_state_in_row_or_group(queue_id, consumer_id, db_row, &cs, &gqcs, db);

    if(ret != 0)
        return ret; // Consumer or group doesn't exist

    assert (cs != NULL || gqcs != NULL);

    assert(new_consume_head <= no_entries - 1);

    assert(version != NULL);

    int64_t old_read_head = READ_HEAD(cs), old_consume_head = CONSUME_HEAD(cs);

    sanity_check_consumer_consume_heads(old_read_head, old_consume_head, new_consume_head);

    if(old_consume_head <= new_consume_head)
    {
    	cs->private_consume_head = new_consume_head;
    	update_or_replace_vc(&(cs->pch_version), version);
    }

    return 0;
}

int read_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int max_entries, int * entries_read, int64_t * new_read_head, vector_clock ** prh_version,
        snode_t** start_row, snode_t** end_row, short use_lock,
        db_t * db)
{
    db_table_t * table = get_table_by_key(table_key, db);
    *entries_read=0;

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    int64_t no_entries = db_row->no_entries;

    consumer_state * cs = NULL, * gqcs = NULL;

    int ret = lookup_consumer_state_in_row_or_group(queue_id, consumer_id, db_row, &cs, &gqcs, db);

    if(ret != 0)
        return ret; // Consumer or group doesn't exist

    assert (cs != NULL || gqcs != NULL);

    if(use_lock)
    {
        pthread_mutex_lock(db_row->read_lock);
    }

    if(cs->private_read_head > no_entries - 1)
    {
        log_debug("BACKEND: ERROR Subscriber %" PRId64 " trying to read from queue %" PRId64 " , prev_read_head=%" PRId64 ", no_entries=%" PRId64 "",
                        (int64_t) cs->consumer_id, queue_id, cs->private_read_head, no_entries);

        assert(0);
    }

    int64_t old_read_head = READ_HEAD(cs);

    vector_clock * old_prh_vsn = READ_HEAD_VERSION(cs);

    *prh_version = (old_prh_vsn != NULL)? copy_vc(old_prh_vsn) : NULL;

    if(old_read_head == no_entries - 1)
    {
        if(use_lock)
        {
            pthread_mutex_unlock(db_row->read_lock);
        }

        *new_read_head = old_read_head;
        return QUEUE_STATUS_READ_COMPLETE; // Nothing to read
    }

    *new_read_head = MIN(old_read_head + max_entries, no_entries - 1);
    int64_t start_index = old_read_head + 1;

    cs->private_read_head = *new_read_head;

    if(use_lock)
    {
        pthread_mutex_unlock(db_row->read_lock);
    }

    int64_t no_results = (int64_t) table_range_search_clustering((WORD *) &queue_id,
                                        (WORD*) &start_index, (WORD*) new_read_head, 1,
                                        start_row, end_row, table);

    assert(no_results == (*new_read_head - start_index + 1));

#if (VERBOSITY > 0)
    log_debug("BACKEND: Subscriber %" PRId64 " read %" PRId64 " queue entries from queue %" PRId64 ", new_read_head=%" PRId64 "",
                    (int64_t) cs->consumer_id, no_results, queue_id, cs->private_read_head);
#endif

    *entries_read = (int) no_results;

    ret = ((*new_read_head) == (no_entries - 1))? QUEUE_STATUS_READ_COMPLETE : QUEUE_STATUS_READ_INCOMPLETE;

    if(ret == QUEUE_STATUS_READ_COMPLETE)
    {
    	set_consumer_notified(cs, gqcs, 0);
    }

    return ret;
}

int peek_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int max_entries, int64_t offset, int * entries_read, int64_t * new_read_head, vector_clock ** prh_version,
        snode_t** start_row, snode_t** end_row, db_t * db)
{
    db_table_t * table = get_table_by_key(table_key, db);
    *entries_read=0;

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    int64_t no_entries = db_row->no_entries;

    consumer_state * cs = NULL, * gqcs = NULL;

    int ret = lookup_consumer_state_in_row_or_group(queue_id, consumer_id, db_row, &cs, &gqcs, db);

    if(ret != 0)
        return ret; // Consumer or group doesn't exist

    assert (cs != NULL || gqcs != NULL);

    int64_t old_read_head = READ_HEAD(cs);

    vector_clock * old_prh_vsn = READ_HEAD_VERSION(cs);

    int64_t start_offset = (offset >= 0)?offset:old_read_head;
    assert(start_offset <= no_entries - 1);

    *prh_version = (old_prh_vsn != NULL)? copy_vc(old_prh_vsn) : NULL;

    if(start_offset == no_entries - 1)
    {
        *new_read_head = start_offset;
        return QUEUE_STATUS_READ_COMPLETE; // Nothing to read
    }

    *new_read_head = MIN(start_offset + max_entries, no_entries - 1);
    int64_t start_index = start_offset + 1;

    int64_t no_results = (int64_t) table_range_search_clustering((WORD *) &queue_id,
                                        (WORD*) &start_index, (WORD*) new_read_head, 1,
                                        start_row, end_row, table);

    assert(no_results == (*new_read_head - start_index + 1));

#if (VERBOSITY > 0)
    log_debug("BACKEND: Subscriber %" PRId64 " peeked %" PRId64 " / %" PRId64 " queue entries, new_read_head=%" PRId64 ", private_read_head=%" PRId64 "",
                    (int64_t) cs->consumer_id, no_results, no_entries, *new_read_head, old_read_head);
#endif

    *entries_read = (int) no_results;

    ret = ((*new_read_head) == (no_entries - 1))? QUEUE_STATUS_READ_COMPLETE : QUEUE_STATUS_READ_INCOMPLETE;

    return ret;
}

int replay_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int64_t replay_offset, int max_entries,
        int * entries_read, int64_t * new_replay_offset,
        snode_t** start_row, snode_t** end_row,
        db_t * db)
{
    db_table_t * table = get_table_by_key(table_key, db);
    *entries_read=0;

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    int64_t no_entries = db_row->no_entries;

    consumer_state * cs = NULL, * gqcs = NULL;

    int ret = lookup_consumer_state_in_row_or_group(queue_id, consumer_id, db_row, &cs, &gqcs, db);

    if(ret != 0)
        return ret; // Consumer or group doesn't exist

    assert (cs != NULL || gqcs != NULL);

//  set_consumer_notified(cs, gqcs, 0); // Replays don't count as notification consumptions

    int64_t old_read_head = READ_HEAD(cs), old_consume_head = CONSUME_HEAD(cs);

    assert(old_read_head <= no_entries - 1);
    assert(old_consume_head + replay_offset <= old_read_head);

    if(old_consume_head + replay_offset == old_read_head)
    {
        return QUEUE_STATUS_READ_COMPLETE; // // Nothing to replay
    }

    *new_replay_offset = MIN(old_consume_head + replay_offset + max_entries, old_read_head);
    int64_t start_index = old_consume_head + replay_offset;

    int64_t no_results = (int64_t) table_range_search_clustering((WORD *) &queue_id,
                                        (WORD*) &start_index, (WORD*) new_replay_offset, 1,
                                        start_row, end_row, table);
    *entries_read = (int) no_results;

    if(no_results != (*new_replay_offset) - start_index)
    {
        log_debug("table_range_search_clustering(%" PRId64 "-%" PRId64 ") returned %" PRId64 " entries!", start_index, *new_replay_offset, no_results);
        print_long_db(db);
        assert(0);
    }

#if (VERBOSITY > 0)
    log_debug("BACKEND: Subscriber %" PRId64 " replayed %" PRId64 " queue entries, new_replay_offset=%" PRId64 "",
                    (int64_t) cs->consumer_id, no_results, *new_replay_offset);
#endif

    ret = ((*new_replay_offset) == old_read_head)? QUEUE_STATUS_READ_COMPLETE : QUEUE_STATUS_READ_INCOMPLETE;

    return ret;
}

int consume_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int64_t new_consume_head, db_t * db)
{
    db_table_t * table = get_table_by_key(table_key, db);

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    consumer_state * cs = NULL, * gqcs = NULL;

    int ret = lookup_consumer_state_in_row_or_group(queue_id, consumer_id, db_row, &cs, &gqcs, db);

    if(ret != 0)
        return ret; // Consumer or group doesn't exist

    assert (cs != NULL || gqcs != NULL);

    int64_t old_read_head = READ_HEAD(cs), old_consume_head = CONSUME_HEAD(cs);

    set_consumer_notified(cs, gqcs, 0);

    assert(old_consume_head <= old_read_head);

    if(new_consume_head > old_read_head)
    {
        return DB_ERR_QUEUE_HEAD_INVALID; // // Invalid consume
    }

    if(new_consume_head == old_consume_head)
    {
        return DB_ERR_QUEUE_COMPLETE; // // Nothing to consume
    }

    cs->private_consume_head = new_consume_head;

#if (VERBOSITY > 0)
    log_debug("BACKEND: Subscriber %" PRId64 " consumed entries, new_consume_head=%" PRId64 ", read_head=%" PRId64 "",
                    (int64_t) cs->consumer_id, new_consume_head, old_read_head);
#endif

    return (int) new_consume_head;
}


int __subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, db_row_t * db_row, WORD group_id,
                    queue_callback * callback, int * sockfd, int64_t * prev_read_head, int64_t * prev_consume_head,
                    short use_lock, db_t * db, unsigned int * fastrandstate)
{
    if(use_lock)
        pthread_mutex_lock(db_row->subscribe_lock);

    *prev_read_head = -1;
    *prev_consume_head = -1;

    snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
    if(consumer_node != NULL)
    {
        consumer_state * found_cs = (consumer_state *) (consumer_node->value);

        log_debug("BACKEND: ERR: Found consumer state %" PRId64 " when searching for consumer_id %" PRId64 "!", (int64_t) found_cs->consumer_id, (int64_t) consumer_id);

        *prev_read_head = found_cs->private_read_head;
        *prev_consume_head = found_cs->private_consume_head;

        if(use_lock)
            pthread_mutex_unlock(db_row->subscribe_lock);

        return DB_ERR_DUPLICATE_CONSUMER; // Consumer already exists!
    }

    consumer_state * cs = get_consumer_state(consumer_id, shard_id, app_id, group_id, callback, sockfd, 0);

    int ret = skiplist_insert(db_row->consumer_state, consumer_id, cs, fastrandstate);

    if(use_lock)
        pthread_mutex_unlock(db_row->subscribe_lock);

#if (VERBOSITY > 0)
    log_debug("BACKEND: Subscriber %" PRId64 "/%" PRId64 "/%" PRId64 "/%" PRId64 " subscribed queue %" PRId64 " with callback %p",
                    (int64_t) cs->app_id, (int64_t) cs->shard_id, (int64_t) cs->consumer_id, (int64_t) cs->group_id,
                    (int64_t) db_row->key, cs->callback);
#endif

    return ret;
}


int _subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, WORD group_id,
                    queue_callback * callback, int * sockfd, int64_t * prev_read_head, int64_t * prev_consume_head,
                    short use_lock, db_t * db, unsigned int * fastrandstate)
{
    db_table_t * table = get_table_by_key(table_key, db);

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    int ret = __subscribe_queue(consumer_id, shard_id, app_id, db_row, group_id,
            callback, sockfd, prev_read_head, prev_consume_head,
            use_lock, db, fastrandstate);

    return ret;
}

int _subscribe_group(WORD consumer_id, WORD shard_id, WORD app_id, WORD group_id,
                    queue_callback * callback, int * sockfd,
                    short use_lock, db_t * db, unsigned int * fastrandstate)
{
    group_state * found_group = NULL;
    snode_t * group_node = lookup_bucket(db->queue_groups, group_id);
    int ret = 0;
    if(group_node == NULL) // Group doesn't exist
    {
        found_group = get_group((WORD) group_id);

        ret = add_listener_to_group(found_group, consumer_id, shard_id, app_id,
                               callback, sockfd, fastrandstate);

        log_debug("BACKEND: Bucket %d not found, adding it as %p", group_id, found_group);

        add_bucket(db->queue_groups, found_group, &get_group_state_key, &get_group_state_live_field, fastrandstate);
    }
    else
    {
        found_group = (group_state *) (group_node->value);

        ret = add_listener_to_group(found_group,
                                consumer_id, shard_id, app_id,
                                callback,
                                sockfd,
                                fastrandstate);
        if(ret == DB_ERR_DUPLICATE_CONSUMER)
        {
            log_debug("BACKEND: Warning: Found previously existing consumer state in queue group %p, group_id = %" PRId64 " when searching for consumer_id %" PRId64 "!", found_group, (int64_t) group_id, (int64_t) consumer_id);
        }
    }

#if (VERBOSITY > 0)
    log_debug("BACKEND: Subscriber %" PRId64 "/%" PRId64 "/%" PRId64 " subscribed to group %p, group_id=%" PRId64 " with callback %p, fd %d, group now has %d consumers",
                    (int64_t) app_id, (int64_t) shard_id, (int64_t) consumer_id, found_group, (int64_t) group_id, callback, *sockfd, found_group->consumers->no_items);
#endif

    return ret;
}

int _unsubscribe_group(WORD consumer_id, WORD group_id, db_t * db)
{
    snode_t * group_node = lookup_bucket(db->queue_groups, group_id);
    if(group_node == NULL)
        return DB_ERR_NO_GROUP; // Group doesn't exist

    group_state * found_group = (group_state *) (group_node->value);

    int ret = remove_listener_from_group(found_group, consumer_id);

    if(ret == DB_ERR_NO_CONSUMER)
    {
        log_debug("BACKEND: Warning: Found no previously existing consumer state in queue group %" PRId64 " when attempting to remove consumer_id %" PRId64 "!", (int64_t) group_id, (int64_t) consumer_id);
    }

#if (VERBOSITY > 0)
    log_debug("BACKEND: Subscriber %" PRId64 " unsubscribed from group %" PRId64 "", (int64_t) consumer_id, (int64_t) group_id);
#endif

    return ret;
}


int subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                    queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
                    short use_lock, db_t * db, unsigned int * fastrandstate)
{
    assert(callback != NULL);

    return _subscribe_queue(consumer_id, shard_id, app_id, table_key, queue_id, (WORD) -1,
            callback, NULL, prev_read_head, prev_consume_head,
            use_lock, db, fastrandstate);
}

int register_remote_subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, WORD group_id,
                    int * sockfd, int64_t * prev_read_head, int64_t * prev_consume_head,
                    short use_lock, db_t * db, unsigned int * fastrandstate)
{
    assert(sockfd != NULL);

#if (VERBOSITY > 0)
    log_debug("BACKEND: register_remote_subscribe_queue %" PRId64 "/%" PRId64 "/%" PRId64 " for group_id %" PRId64 ", queue_id %" PRId64 "/%" PRId64 "",
                    (int64_t) app_id, (int64_t) shard_id, (int64_t) consumer_id, (int64_t) group_id,
                    (int64_t) table_key, (int64_t) queue_id);
#endif

    if((int) group_id == -1)
    {
        return _subscribe_queue(consumer_id, shard_id, app_id, table_key, queue_id, group_id,
                NULL, sockfd, prev_read_head, prev_consume_head,
                use_lock, db, fastrandstate);
    }
    else
    {
        return _subscribe_group(consumer_id, shard_id, app_id, group_id,
                    NULL, sockfd, use_lock, db, fastrandstate);
    }
}


int _unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, WORD group_id,
                        short use_lock, db_t * db)
{
    db_table_t * table = get_table_by_key(table_key, db);

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    if(use_lock)
        pthread_mutex_lock(db_row->subscribe_lock);

    snode_t * consumer_node = skiplist_delete(db_row->consumer_state, consumer_id);

    if(use_lock)
        pthread_mutex_unlock(db_row->subscribe_lock);

    if(node == NULL)
        return DB_ERR_NO_CONSUMER; // Consumer didn't exist

#if (VERBOSITY > 0)
    log_debug("BACKEND: Subscriber %" PRId64 "/%" PRId64 "/%" PRId64 "/%" PRId64 " unsubscribed queue %" PRId64 "/%" PRId64 "",
                    (int64_t) app_id, (int64_t) shard_id, (int64_t) consumer_id, (int64_t) group_id,
                    (int64_t) table_key, (int64_t) queue_id);
#endif

    return 0;
}

int unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, WORD group_id,
                        short use_lock, db_t * db)
{
    return _unsubscribe_queue(consumer_id, shard_id, app_id, table_key, queue_id, group_id, use_lock, db);
}

int register_remote_unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, WORD group_id,
                                        short use_lock, db_t * db)
{
    if((int) group_id == -1)
    {
        return _unsubscribe_queue(consumer_id, shard_id, app_id, table_key, queue_id, group_id, use_lock, db);
    }
    else
    {
        return _unsubscribe_group(consumer_id, group_id, db);
    }
}

void _create_headers_for_group_subscribers(group_state * gs, db_row_t * row, db_t * db, unsigned int * fastrandstate)
{
	int64_t prev_read_head, prev_consume_head;

	if(gs == NULL)
		return;

    for(snode_t * crt_consumer = HEAD(gs->consumers); crt_consumer!=NULL; crt_consumer = NEXT(crt_consumer))
    {
    	consumer_state * cs = (consumer_state *) crt_consumer->value;

    	__subscribe_queue(cs->consumer_id, cs->shard_id, cs->app_id, row, gs->group_id,
    	            NULL, NULL, &prev_read_head, &prev_consume_head,
    	            1, db, fastrandstate);
    }
}

void create_headers_for_group_subscribers(db_row_t * db_row, db_t * db, unsigned int * fastrandstate)
{
	if(db_row->group_subscriptions == NULL)
		return;

    if(db->queue_group_replication_factor == 1)
    {
        group_state * gs = (group_state *) db_row->group_subscriptions;

        _create_headers_for_group_subscribers(gs, db_row, db, fastrandstate);

	    log_debug("SERVER: Updated queue group bucket for queue %d, group_pointer=%p, group_id = %d, status = %d, consumers = %d",
	    		(int) db_row->key, gs,
				((gs != NULL)?((int) gs->group_id):(-1)),
				((gs != NULL)?((int) gs->status):(-1)),
				((gs != NULL)?((int) gs->consumers->no_items):(-1)));
    }
    else
    {
    	skiplist_t * groups = (skiplist_t *) db_row->group_subscriptions;

    	for(snode_t * crt_group = HEAD(groups); crt_group!=NULL; crt_group = NEXT(crt_group))
    	{
    		group_state * gs = (group_state *) crt_group->value;

    		_create_headers_for_group_subscribers(gs, db_row, db, fastrandstate);

    	    log_debug("SERVER: Updated queue group bucket for queue %d, group_pointer=%p, group_id = %d, status = %d, consumers = %d",
    	    		(int) db_row->key, gs,
    				((gs != NULL)?((int) gs->group_id):(-1)),
    				((gs != NULL)?((int) gs->status):(-1)),
    				((gs != NULL)?((int) gs->consumers->no_items):(-1)));
    	}
    }
}

int create_queue(WORD table_key, WORD queue_id, vector_clock * version, short use_lock, db_t * db, unsigned int * fastrandstate)
{
    db_table_t * table = get_table_by_key(table_key, db);

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    if(use_lock)
        pthread_mutex_lock(table->lock);

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node != NULL)
    {
        if(use_lock)
            pthread_mutex_unlock(table->lock);

        return DB_ERR_DUPLICATE_QUEUE; // Queue already exists!
    }

    db_schema_t* schema = table->schema;

    // Create sentinel queue entry:

    WORD * queue_column_values = (WORD *) malloc(3 * sizeof(WORD)); // schema->no_cols
    queue_column_values[0]=queue_id;
    queue_column_values[1]=(WORD) - 2;
    queue_column_values[2]=0;

    int status = table_insert(queue_column_values, 3, 1, 0, NULL, table, fastrandstate); // version? // schema->no_cols

    if(status)
    {
        if(use_lock)
            pthread_mutex_unlock(table->lock);

        return status;
    }

    // Get queue row:

    snode_t * qr_node = skiplist_search(table->rows, queue_id);
    if(qr_node == NULL)
    {
        if(use_lock)
            pthread_mutex_unlock(table->lock);

        return DB_ERR_NO_QUEUE; // Queue creation error
    }

    db_row_t * db_row = (db_row_t *) (qr_node->value);

    db_row->consumer_state = create_skiplist_long();

    if(!db_row->consumer_state)
    {
        if(use_lock)
            pthread_mutex_unlock(table->lock);

        return DB_ERR_NO_QUEUE; // Queue creation error
    }

    db_row->enqueue_lock = (pthread_mutex_t*) malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(db_row->enqueue_lock, NULL);
    db_row->read_lock = (pthread_mutex_t*) malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(db_row->read_lock, NULL);
    db_row->subscribe_lock = (pthread_mutex_t*) malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(db_row->subscribe_lock, NULL);

    db_row->group_subscriptions = get_buckets_for_object(db->queue_groups, (int) db_row->key, db->queue_group_replication_factor,
                                                            &get_group_state_key, &get_group_state_live_field,
                                                            fastrandstate);

    if(db_row->group_subscriptions != NULL)
    	create_headers_for_group_subscribers(db_row, db, fastrandstate);

    if(version != NULL)
        update_or_replace_vc(&(db_row->version), version);

    skiplist_insert(table->queues, queue_id, (WORD) db_row, fastrandstate);

    if(use_lock)
        pthread_mutex_unlock(table->lock);

#if (VERBOSITY > 0)
    log_debug("BACKEND: Queue %" PRId64 "/%" PRId64 " created", (int64_t) table_key, (int64_t) queue_id);
#endif

    return 0;
}

int delete_queue(WORD table_key, WORD queue_id, vector_clock * version, short use_lock, db_t * db, unsigned int * fastrandstate)
{
    db_table_t * table = get_table_by_key(table_key, db);
    int ret = 0;

    if(table == NULL)
        return DB_ERR_NO_TABLE; // Table doesn't exist

    snode_t * node = skiplist_search(table->rows, queue_id);
    if(node == NULL)
        return DB_ERR_NO_QUEUE; // Queue doesn't exist

    db_row_t * db_row = (db_row_t *) (node->value);

    if(use_lock)
        pthread_mutex_lock(table->lock);

    // Remove pointer to queue DB row from queue cache table:

    skiplist_delete(table->queues, queue_id);

    // Notify consumers of queue deletion:

    for(snode_t * cell=HEAD(db_row->consumer_state);cell!=NULL;cell=NEXT(cell))
    {
        if(cell->value != NULL)
        {
            consumer_state * cs = (consumer_state *) (cell->value);

            if(cs->callback == NULL || cs->notified > 0)
                continue;

            queue_callback_args * qca = get_queue_callback_args(table_key, queue_id, cs->app_id, cs->shard_id, cs->consumer_id, cs->group_id, QUEUE_NOTIF_DELETED);

#if (VERBOSITY > 0)
            log_debug("BACKEND: Attempting to notify subscriber %" PRId64 " (%p/%p/%p/%p)", (int64_t) qca->consumer_id, cs->callback, cs->callback->lock, cs->callback->signal, cs->callback->callback);
#endif

            ret = pthread_mutex_lock(cs->callback->lock);

#if (VERBOSITY > 0)
            log_debug("BACKEND: Locked consumer lock of %" PRId64 " (%p/%p), status=%d", (int64_t) qca->consumer_id, cs->callback, cs->callback->lock, ret);
#endif

            pthread_cond_signal(cs->callback->signal);
            cs->callback->callback(qca);
            ret = pthread_mutex_unlock(cs->callback->lock);

#if (VERBOSITY > 0)
            log_debug("BACKEND: Unlocked consumer lock of %" PRId64 " (%p/%p), status=%d", (int64_t) qca->consumer_id, cs->callback, cs->callback->lock, ret);
#endif

//          cs->notified=1;

#if (VERBOSITY > 0)
            log_debug("BACKEND: Notified subscriber %" PRId64 " (%p/%p/%p/%p)", (int64_t) qca->consumer_id, cs->callback, cs->callback->lock, cs->callback->signal, cs->callback->callback);
#endif

        }
    }

    skiplist_free_val(db_row->consumer_state, free_consumer_state_sl);

    ret = table_delete_row((WORD*) &(queue_id), version, table, db, fastrandstate);

    if(use_lock)
        pthread_mutex_unlock(table->lock);

#if (VERBOSITY > 0)
    log_debug("BACKEND: Queue %" PRId64 "/%" PRId64 " deleted", (int64_t) table_key, (int64_t) queue_id);
#endif

    return ret;
}

consumer_state * get_consumer_state(WORD consumer_id, WORD shard_id, WORD app_id, WORD group_id, queue_callback* callback, int * sockfd,
                                    int is_group_subscription)
{
    consumer_state * cs = (consumer_state *) malloc(sizeof(consumer_state));
    cs->consumer_id = consumer_id;
    cs->shard_id = shard_id;
    cs->app_id = app_id;
    cs->group_id = group_id;
    cs->private_read_head = -1;
    cs->private_consume_head = -1;
    cs->callback = callback;
    cs->sockfd = sockfd;
    cs->notified=0;
    cs->prh_version=NULL;
    cs->pch_version=NULL;

    return cs;
}

void free_consumer_state(consumer_state * cs)
{
    if(cs->prh_version != NULL)
        free_vc(cs->prh_version);

    if(cs->pch_version != NULL)
        free_vc(cs->pch_version);

    if(cs->callback != NULL)
        free_queue_callback(cs->callback);

    free(cs);
}

void free_consumer_state_sl(void * cs)
{
    free_consumer_state((consumer_state *) cs);
}

int add_consumer_state_to_group(WORD queue_id, consumer_state * cs, group_state *gs, unsigned int * fastrandstate)
{
    snode_t * consumer_node = skiplist_search(gs->consumers, queue_id);
    if(consumer_node != NULL)
    {
        return 1;
    }

    return skiplist_insert(gs->consumers, queue_id, cs, fastrandstate);
}

int get_consumer_state_from_group(WORD queue_id, group_state * gs, consumer_state ** cs)
{
    snode_t * consumer_node = skiplist_search(gs->consumers, queue_id);
    if(consumer_node != NULL)
    {
        *cs = (consumer_state *) (consumer_node->value);
        return 0;
    }
    return 1;
}

int pop_consumer_state_from_group(WORD queue_id, group_state * gs, consumer_state ** cs)
{
    *cs = (consumer_state *) skiplist_delete(gs->consumers, queue_id);
    if(*cs != NULL)
    {
        return 0;
    }
    return 1;
}


void free_queue_table_state(WORD queue_table_state)
{
    skiplist_free(queue_table_state);
}








