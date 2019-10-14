/*
 * queue.c
 *
 *      Author: aagapi
 */

#include "queue.h"
#include <limits.h>
#include <assert.h>
#include <stdlib.h>

#include <stdio.h>

#define MIN(x, y) (((x)<(y))?(x):(y))
#define MAX(x, y) (((x)>(y))?(x):(y))

#define VERBOSITY 1

db_table_t * get_table_by_key(WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, table_key);
	if(node == NULL)
		return NULL;

	return (db_table_t *) (node->value);
}

int create_queue_table(WORD table_id, int no_cols, int * col_types, db_t * db, unsigned int * fastrandstate) {
	int primary_key_idx = 0; // queue_id
	int clustering_key_idx = 1;	// entry_id
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
	printf("BACKEND: Queue table %ld created\n", (long) table_id);
#endif

	return ret;
}

int enqueue(WORD * column_values, int no_cols, WORD table_key, WORD queue_id, short use_lock, db_t * db, unsigned int * fastrandstate)
{
	db_table_t * table = get_table_by_key(table_key, db);

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

	long entry_id = db_row->no_entries;
	db_row->no_entries++;

	if(use_lock)
	{
		pthread_mutex_unlock(db_row->enqueue_lock);
	}

	// Add queue_id as partition key and entry_id as clustering key:

	WORD * queue_column_values = (WORD *) malloc((no_cols + 2) * sizeof(WORD));
	queue_column_values[0]=queue_id;
	queue_column_values[1]=(WORD) entry_id;
	for(long i=2;i<no_cols + 2;i++)
		queue_column_values[i]=column_values[i-2];

	int status = table_insert(queue_column_values, no_cols+2, NULL, table, fastrandstate);

#if (VERBOSITY > 0)
	printf("BACKEND: Inserted queue entry %ld in queue %ld/%ld, status=%d\n", entry_id, (long) table_key, (long) queue_id, status);
#endif

	// Notify subscribers if they haven't been notified:

	for(snode_t * cell=HEAD(db_row->consumer_state);cell!=NULL;cell=NEXT(cell))
	{
		if(cell->value != NULL)
		{
			consumer_state * cs = (consumer_state *) (cell->value);

			if(cs->callback == NULL || cs->notified > 0)
				continue;

			queue_callback_args * qca = (queue_callback_args *) malloc(sizeof(queue_callback_args));
			qca->table_key = table_key;
			qca->queue_id = queue_id;

			qca->app_id = cs->app_id;
			qca->shard_id = cs->shard_id;
			qca->consumer_id = cs->consumer_id;

			qca->status = QUEUE_NOTIF_ENQUEUED;

#if (VERBOSITY > 0)
			printf("BACKEND: Attempting to notify subscriber %ld\n", (long) qca->consumer_id);
#endif

			pthread_mutex_lock(cs->callback->lock);
			pthread_cond_signal(cs->callback->signal);
			cs->callback->callback(qca);
			pthread_mutex_unlock(cs->callback->lock);

			cs->notified=1;

#if (VERBOSITY > 0)
			printf("BACKEND: Notified subscriber %ld\n", (long) qca->consumer_id);
#endif
		}
	}

	return status;
}

int set_private_read_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
							long new_read_head, vector_clock * version, short use_lock, db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);
	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist
	snode_t * node = skiplist_search(table->rows, queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	long no_entries = db_row->no_entries;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	if(use_lock)
	{
		pthread_mutex_lock(db_row->read_lock);
	}

	consumer_state * cs = (consumer_state *) (consumer_node->value);

	assert(new_read_head <= no_entries - 1);

	assert(cs->private_read_head <= new_read_head);

	assert(cs->private_consume_head <= new_read_head);

	cs->private_read_head = new_read_head;

	assert(version != NULL);

	update_or_replace_vc(&(cs->prh_version), version);

	if(use_lock)
	{
		pthread_mutex_unlock(db_row->read_lock);
	}

	return 0;
}

int set_private_consume_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
							long new_consume_head, vector_clock * version, db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);
	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist
	snode_t * node = skiplist_search(table->rows, queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	long no_entries = db_row->no_entries;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	consumer_state * cs = (consumer_state *) (consumer_node->value);

	assert(new_consume_head <= no_entries - 1);

	assert(cs->private_read_head >= new_consume_head);

	assert(cs->private_consume_head <= new_consume_head);

	cs->private_consume_head = new_consume_head;

	assert(version != NULL);

	update_or_replace_vc(&(cs->pch_version), version);

	return 0;
}

int read_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, long * new_read_head, vector_clock ** prh_version,
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

	long no_entries = db_row->no_entries;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	if(use_lock)
	{
		pthread_mutex_lock(db_row->read_lock);
	}

	consumer_state * cs = (consumer_state *) (consumer_node->value);

	assert(cs->private_read_head <= no_entries - 1);

	*prh_version = (cs->prh_version != NULL)? copy_vc(cs->prh_version) : NULL;

	if(cs->private_read_head == no_entries - 1)
	{
		if(use_lock)
		{
			pthread_mutex_unlock(db_row->read_lock);
		}

		*new_read_head = cs->private_read_head;
		return QUEUE_STATUS_READ_COMPLETE; // Nothing to read
	}

	*new_read_head = MIN(cs->private_read_head + max_entries, no_entries - 1);
	long start_index = cs->private_read_head + 1;

	cs->private_read_head = *new_read_head;

	if(use_lock)
	{
		pthread_mutex_unlock(db_row->read_lock);
	}

	long no_results = (long) table_range_search_clustering((WORD *) &queue_id,
										(WORD*) &start_index, (WORD*) new_read_head, 1,
										start_row, end_row, table);

	assert(no_results == (*new_read_head - start_index + 1));

#if (VERBOSITY > 0)
	printf("BACKEND: Subscriber %ld read %ld queue entries, new_read_head=%ld\n",
					(long) cs->consumer_id, no_results, cs->private_read_head);
#endif

	*entries_read = (int) no_results;

	int ret = ((*new_read_head) == (no_entries - 1))? QUEUE_STATUS_READ_COMPLETE : QUEUE_STATUS_READ_INCOMPLETE;

	cs->notified=(ret==QUEUE_STATUS_READ_INCOMPLETE);

	return ret;
}

int peek_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, long offset, int * entries_read, long * new_read_head, vector_clock ** prh_version,
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

	long no_entries = db_row->no_entries;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	consumer_state * cs = (consumer_state *) (consumer_node->value);

	long start_offset = (offset >= 0)?offset:cs->private_read_head;
	assert(start_offset <= no_entries - 1);

	*prh_version = (cs->prh_version != NULL)? copy_vc(cs->prh_version) : NULL;

	if(start_offset == no_entries - 1)
	{
		*new_read_head = start_offset;
		return QUEUE_STATUS_READ_COMPLETE; // Nothing to read
	}

	*new_read_head = MIN(start_offset + max_entries, no_entries - 1);
	long start_index = start_offset + 1;

	long no_results = (long) table_range_search_clustering((WORD *) &queue_id,
										(WORD*) &start_index, (WORD*) new_read_head, 1,
										start_row, end_row, table);

	assert(no_results == (*new_read_head - start_index + 1));

#if (VERBOSITY > 0)
	printf("BACKEND: Subscriber %ld peeked %ld queue entries, new_read_head=%ld, private_read_head=%ld\n",
					(long) cs->consumer_id, no_results, *new_read_head, cs->private_read_head);
#endif

	*entries_read = (int) no_results;

	int ret = ((*new_read_head) == (no_entries - 1))? QUEUE_STATUS_READ_COMPLETE : QUEUE_STATUS_READ_INCOMPLETE;

	return ret;
}

int replay_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		long replay_offset, int max_entries,
		int * entries_read, long * new_replay_offset,
		snode_t** start_row, snode_t** end_row,
		db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	long no_entries = db_row->no_entries;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	consumer_state * cs = (consumer_state *) (consumer_node->value);

//	cs->notified=0; // Replays don't count as notification consumptions

	assert(cs->private_read_head <= no_entries - 1);
	assert(cs->private_consume_head + replay_offset <= cs->private_read_head);

	if(cs->private_consume_head + replay_offset == cs->private_read_head)
	{
		return QUEUE_STATUS_READ_COMPLETE; // // Nothing to replay
	}

	*new_replay_offset = MIN(cs->private_consume_head + replay_offset + max_entries, cs->private_read_head);
	long start_index = cs->private_consume_head + replay_offset;

	long no_results = (long) table_range_search_clustering((WORD *) &queue_id,
										(WORD*) &start_index, (WORD*) new_replay_offset, 1,
										start_row, end_row, table);

	assert(no_results == (*new_replay_offset) - start_index);

#if (VERBOSITY > 0)
	printf("BACKEND: Subscriber %ld replayed %ld queue entries, new_replay_offset=%ld\n",
					(long) cs->consumer_id, no_results, *new_replay_offset);
#endif

	int ret = ((*new_replay_offset) == cs->private_read_head)? QUEUE_STATUS_READ_COMPLETE : QUEUE_STATUS_READ_INCOMPLETE;

	return ret;
}

int consume_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		long new_consume_head, db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	consumer_state * cs = (consumer_state *) (consumer_node->value);

	cs->notified=0;

	assert(cs->private_consume_head <= cs->private_read_head);

	if(new_consume_head > cs->private_read_head)
	{
		return DB_ERR_QUEUE_HEAD_INVALID; // // Invalid consume
	}

	if(new_consume_head == cs->private_consume_head)
	{
		return DB_ERR_QUEUE_COMPLETE; // // Nothing to consume
	}

	cs->private_consume_head = new_consume_head;

#if (VERBOSITY > 0)
	printf("BACKEND: Subscriber %ld consumed entries, new_consume_head=%ld, read_head=%ld\n",
					(long) cs->consumer_id, cs->private_consume_head, cs->private_read_head);
#endif

	return (int) new_consume_head;
}

int subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					queue_callback * callback, long * prev_read_head, long * prev_consume_head,
					short use_lock, db_t * db, unsigned int * fastrandstate)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE;	// Table doesn't exist

	snode_t * node = skiplist_search(table->rows, queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	if(use_lock)
		pthread_mutex_lock(db_row->subscribe_lock);

	*prev_read_head = -1;
	*prev_consume_head = -1;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, consumer_id);
	if(consumer_node != NULL)
	{
		consumer_state * found_cs = (consumer_state *) (consumer_node->value);

		printf("BACKEND: ERR: Found consumer state %ld when searching for consumer_id %ld!\n", (long) found_cs->consumer_id, (long) consumer_id);

		*prev_read_head = found_cs->private_read_head;
		*prev_consume_head = found_cs->private_consume_head;

		if(use_lock)
			pthread_mutex_unlock(db_row->subscribe_lock);

		return DB_ERR_DUPLICATE_CONSUMER; // Consumer already exists!
	}

	consumer_state * cs = (consumer_state *) malloc(sizeof(consumer_state));
	cs->consumer_id = consumer_id;
	cs->shard_id = shard_id;
	cs->app_id = app_id;
	cs->private_read_head = -1;
	cs->private_consume_head = -1;
	cs->callback = callback;
	cs->notified=0;

	int ret = skiplist_insert(db_row->consumer_state, consumer_id, cs, fastrandstate);

	if(use_lock)
		pthread_mutex_unlock(db_row->subscribe_lock);

#if (VERBOSITY > 0)
	printf("BACKEND: Subscriber %ld/%ld/%ld subscribed queue %ld/%ld with callback %p\n",
					(long) cs->app_id, (long) cs->shard_id, (long) cs->consumer_id,
					(long) table_key, (long) queue_id, cs->callback);
#endif

	return ret;
}

int unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
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
	printf("BACKEND: Subscriber %ld/%ld/%ld unsubscribed queue %ld/%ld\n",
					(long) app_id, (long) shard_id, (long) consumer_id,
					(long) table_key, (long) queue_id);
#endif

	return 0;
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

	WORD * queue_column_values = (WORD *) malloc(schema->no_cols * sizeof(WORD));
	queue_column_values[0]=queue_id;
	queue_column_values[1]=(WORD) LONG_MAX;
	for(long i=2;i<schema->no_cols;i++)
		queue_column_values[i]=0;

	int status = table_insert(queue_column_values, schema->no_cols, NULL, table, fastrandstate); // version?

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

	if(version != NULL)
		update_or_replace_vc(&(db_row->version), version);

	if(use_lock)
		pthread_mutex_unlock(table->lock);

#if (VERBOSITY > 0)
	printf("BACKEND: Queue %ld/%ld created\n", (long) table_key, (long) queue_id);
#endif

	return 0;
}

int delete_queue(WORD table_key, WORD queue_id, vector_clock * version, short use_lock, db_t * db, unsigned int * fastrandstate)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	if(use_lock)
		pthread_mutex_lock(table->lock);

	// Notify consumers of queue deletion:

	for(snode_t * cell=HEAD(db_row->consumer_state);cell!=NULL;cell=NEXT(cell))
	{
		if(cell->value != NULL)
		{
			consumer_state * cs = (consumer_state *) (cell->value);
			queue_callback_args * qca = (queue_callback_args *) malloc(sizeof(queue_callback_args));
			qca->table_key = table_key;
			qca->queue_id = queue_id;

			qca->app_id = cs->app_id;
			qca->shard_id = cs->shard_id;
			qca->consumer_id = cs->consumer_id;

			qca->status = QUEUE_NOTIF_DELETED;

			pthread_mutex_lock(cs->callback->lock);
			pthread_cond_signal(cs->callback->signal);
			cs->callback->callback(qca);
			pthread_mutex_unlock(cs->callback->lock);

//			cs->notified=1;
		}
	}

	skiplist_free(db_row->consumer_state);

	int ret = table_delete_row((WORD*) &(queue_id), version, table, fastrandstate);

	if(use_lock)
		pthread_mutex_unlock(table->lock);

#if (VERBOSITY > 0)
	printf("BACKEND: Queue %ld/%ld deleted\n", (long) table_key, (long) queue_id);
#endif

	return ret;
}






