/*
 * queue.c
 *
 *      Author: aagapi
 */

#include "queue.h"
#include <limits.h>
#include <assert.h>
#include <stdlib.h>

#define MIN(x, y) (((x)<(y))?(x):(y))
#define MAX(x, y) (((x)>(y))?(x):(y))

db_table_t * get_table_by_key(WORD table_key, db_t * db)
{
	snode_t * node = skiplist_search(db->tables, (long) table_key);
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

	return db_create_table(table_id, db_schema, db, fastrandstate);;
}

int enqueue(WORD * column_values, int no_cols, WORD table_key, WORD queue_id, db_t * db, unsigned int * fastrandstate)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, (long) queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	long entry_id = db_row->no_entries;
	db_row->no_entries++;

	// Add queue_id as partition key and entry_id as clustering key:

	WORD * queue_column_values = (WORD *) malloc((no_cols + 2) * sizeof(WORD));
	queue_column_values[0]=queue_id;
	queue_column_values[1]=(WORD) entry_id;
	for(long i=2;i<no_cols + 2;i++)
		queue_column_values[i]=column_values[i-2];

	int status = table_insert(queue_column_values, no_cols+2, table, fastrandstate);

	// Notify subscribers if they haven't been notified:

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

			qca->status = QUEUE_NOTIF_ENQUEUED;

			pthread_mutex_lock(cs->callback->lock);
			pthread_cond_signal(cs->callback->signal);
			cs->callback->callback(qca);
			pthread_mutex_unlock(cs->callback->lock);

			cs->notified=1;
		}
	}

	return status;
}

int read_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, long * new_read_head,
		snode_t* start_row, snode_t* end_row,
		db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, (long) queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	long no_entries = db_row->no_entries;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, (long) consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	consumer_state * cs = (consumer_state *) (consumer_node->value);

	cs->notified=0;

	assert(cs->private_read_head <= no_entries - 1);

	if(cs->private_read_head == no_entries - 1)
		return 0; // DB_ERR_QUEUE_COMPLETE; // // Nothing to read

	*new_read_head = MIN(cs->private_read_head + max_entries, no_entries - 1);
	long start_index = cs->private_read_head + 1;

	long no_results = (long) table_range_search_clustering((WORD *) &queue_id,
										(WORD*) &start_index, (WORD*) new_read_head, 1,
										&start_row, &end_row, table);

	assert(no_results == *new_read_head);

	cs->private_read_head = *new_read_head;

	return (int) no_results;
}

int replay_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		long replay_offset, int max_entries,
		int * entries_read, long * new_replay_offset,
		snode_t* start_row, snode_t* end_row,
		db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, (long) queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	long no_entries = db_row->no_entries;

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, (long) consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	consumer_state * cs = (consumer_state *) (consumer_node->value);

//	cs->notified=0; // Replays don't count as notification consumptions

	assert(cs->private_read_head <= no_entries - 1);
	assert(cs->private_consume_head + replay_offset <= cs->private_read_head);

	if(cs->private_consume_head + replay_offset == cs->private_read_head)
		return 0; // DB_ERR_QUEUE_COMPLETE; // // Nothing to read

	*new_replay_offset = MIN(cs->private_consume_head + replay_offset + max_entries, cs->private_read_head);
	long start_index = cs->private_consume_head + replay_offset;

	long no_results = (long) table_range_search_clustering((WORD *) &queue_id,
										(WORD*) &start_index, (WORD*) new_replay_offset, 1,
										&start_row, &end_row, table);

	assert(no_results == *new_replay_offset);

	return (int) no_results;
}

int consume_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		long new_consume_head, db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, (long) queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, (long) consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer doesn't exist

	consumer_state * cs = (consumer_state *) (consumer_node->value);

	cs->notified=0;

	assert(cs->private_consume_head <= cs->private_read_head);

	if(new_consume_head > cs->private_read_head)
		return DB_ERR_QUEUE_HEAD_INVALID; // // Invalid consume

	if(new_consume_head == cs->private_consume_head)
		return DB_ERR_QUEUE_COMPLETE; // // Nothing to consume

	cs->private_consume_head = new_consume_head;

	return (int) new_consume_head;
}

int subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, queue_callback * callback, db_t * db, unsigned int * fastrandstate)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE;	// Table doesn't exist

	snode_t * node = skiplist_search(table->rows, (long) queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	snode_t * consumer_node = skiplist_search(db_row->consumer_state, (long) consumer_id);
	if(node != NULL)
		return DB_ERR_DUPLICATE_CONSUMER; // Consumer already exists!

	consumer_state * cs = (consumer_state *) malloc(sizeof(consumer_state));
	cs->consumer_id = consumer_id;
	cs->shard_id = shard_id;
	cs->app_id = app_id;
	cs->private_read_head = 0;
	cs->private_consume_head = 0;
	cs->callback = callback;
	cs->notified=0;

	return skiplist_insert(db_row->consumer_state, (long) consumer_id, callback, fastrandstate);
}

int unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, (long) queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

	snode_t * consumer_node = skiplist_delete(db_row->consumer_state, (long) consumer_id);
	if(node == NULL)
		return DB_ERR_NO_CONSUMER; // Consumer didn't exist

	return 0;
}

int create_queue(WORD table_key, WORD queue_id, db_t * db, unsigned int * fastrandstate)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, (long) queue_id);
	if(node != NULL)
		return DB_ERR_DUPLICATE_QUEUE; // Queue already exists!

	db_schema_t* schema = table->schema;

	// Create sentinel queue entry:

	WORD * queue_column_values = (WORD *) malloc(schema->no_cols * sizeof(WORD));
	queue_column_values[0]=queue_id;
	queue_column_values[1]=(WORD) LONG_MAX;
	for(long i=2;i<schema->no_cols;i++)
		queue_column_values[i]=0;

	int status = table_insert(queue_column_values, schema->no_cols, table, fastrandstate);

	if(status)
		return status;

	// Get queue row:

	snode_t * qr_node = skiplist_search(table->rows, (long) queue_id);
	if(qr_node == NULL)
		return DB_ERR_NO_QUEUE; // Queue creation error

	db_row_t * db_row = (db_row_t *) (qr_node->value);

	db_row->consumer_state = create_skiplist();

	if(!db_row->consumer_state)
		return DB_ERR_NO_QUEUE; // Queue creation error

	return 0;
}

int delete_queue(WORD table_key, WORD queue_id, db_t * db)
{
	db_table_t * table = get_table_by_key(table_key, db);

	if(table == NULL)
		return DB_ERR_NO_TABLE; // Table doesn't exist

	snode_t * node = skiplist_search(table->rows, (long) queue_id);
	if(node == NULL)
		return DB_ERR_NO_QUEUE; // Queue doesn't exist

	db_row_t * db_row = (db_row_t *) (node->value);

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

	return table_delete_row((WORD*) &(queue_id), table);
}






