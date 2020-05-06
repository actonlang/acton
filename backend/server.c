/*
 * server.c
 *
 *      Author: aagapi
 */

// Server:

#include "db.h"
#include "failure_detector/db_queries.h"
#include "comm.h"
#include "fastrand.h"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <sys/select.h>
#include <errno.h>

#define SERVER_BUFSIZE 8 * 1024 // (1024 * 1024)

char in_buf[SERVER_BUFSIZE];
char out_buf[SERVER_BUFSIZE];

int no_state_cols = 4;
int no_state_primary_keys = 1;
int min_state_clustering_keys = 1;
int no_state_index_keys = 1;

int no_queue_cols = 2;

WORD state_table_key = (WORD) 0;
WORD queue_table_key = (WORD) 1;


void error(char *msg) {
  perror(msg);
  exit(1);
}

#define SERVER_VERBOSITY 1

/*
int create_state_schema(db_t * db, unsigned int * fastrandstate) {
	int primary_key_idx = 0;
	int clustering_key_idxs[2];
	clustering_key_idxs[0]=1;
	clustering_key_idxs[1]=2;
	int index_key_idx=3;

	int * col_types = (int *) malloc(no_cols * sizeof(int));

	for(int i=0;i<no_cols;i++)
		col_types[i] = DB_TYPE_INT32;

	db_schema_t* db_schema = db_create_schema(col_types, no_cols, &primary_key_idx, no_primary_keys, clustering_key_idxs, no_clustering_keys, &index_key_idx, no_index_keys);

	assert(db_schema != NULL && "Schema creation failed");

	// Create table:

	return db_create_table((WORD) 0, db_schema, db, fastrandstate);;
}

int create_queue_schema(db_t * db, unsigned int * fastrandstate)
{
	int no_queue_cols = 2;

	int * col_types = (int *) malloc(no_queue_cols * sizeof(int));
	col_types[0] = DB_TYPE_INT64;
	col_types[1] = DB_TYPE_INT32;

	int ret = create_queue_table((WORD) 1, no_queue_cols, col_types, db,  fastrandstate);
	printf("Test %s - %s (%d)\n", "create_queue_table", ret==0?"OK":"FAILED", ret);

	return ret;
}
*/

int create_state_schema(db_t * db, unsigned int * fastrandstate)
{
	int primary_key_idx = 0;
	int clustering_key_idxs[2];
	clustering_key_idxs[0]=1;
	clustering_key_idxs[1]=2;
	int index_key_idx=3;

	int * col_types = NULL;

//	Col types are not enforced:

/*
	col_types = (int *) malloc((no_state_cols+1) * sizeof(int));

	for(int i=0;i<no_state_cols;i++)
		col_types[i] = DB_TYPE_INT32;

	col_types[no_state_cols] = DB_TYPE_BLOB; // Include blob
*/

	db_schema_t* db_schema = db_create_schema(col_types, no_state_cols + 1, &primary_key_idx, no_state_primary_keys, clustering_key_idxs, min_state_clustering_keys, &index_key_idx, no_state_index_keys);

	assert(db_schema != NULL && "Schema creation failed");

	// Create table:

	int ret = db_create_table((WORD) 0, db_schema, db, fastrandstate);

	printf("Test %s - %s (%d)\n", "create_state_table", ret==0?"OK":"FAILED", ret);

	return ret;
}

int create_queue_schema(db_t * db, unsigned int * fastrandstate)
{
	assert(no_queue_cols == 2);

	int * col_types = (int *) malloc((no_queue_cols + 1) * sizeof(int));
	col_types[0] = DB_TYPE_INT64;
	col_types[1] = DB_TYPE_INT32;

	col_types[no_queue_cols] = DB_TYPE_BLOB; // Include blob

	int ret = create_queue_table(queue_table_key, no_queue_cols + 1, col_types, db,  fastrandstate);
	printf("Test %s - %s (%d)\n", "create_queue_table", ret==0?"OK":"FAILED", ret);

	return ret;
}



db_schema_t * get_schema(db_t * db, WORD table_key)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return NULL;

	db_table_t * table = (db_table_t *) (node->value);

	return table->schema;
}

// Write message handlers:

int get_ack_packet(int status, write_query * q,
					void ** snd_buf, unsigned * snd_msg_len)
{
	ack_message * ack = init_ack_message(get_cell_address(q->cell), status, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
	char print_buff[1024];
	to_string_ack_message(ack, (char *) print_buff);
	printf("Sending ack message: %s\n", print_buff);
#endif

	int ret = serialize_ack_message(ack, snd_buf, snd_msg_len);

	free_ack_message(ack);

	return ret;
}

int handle_write_query(write_query * wq, db_t * db, unsigned int * fastrandstate)
{
	int total_cols = wq->cell->no_keys + wq->cell->no_columns;
	int total_cols_plus_blob = total_cols + ((wq->cell->last_blob_size > 0)?(1):(0));

	db_schema_t * schema = get_schema(db, (WORD) wq->cell->table_key);

	int no_clustering_keys = wq->cell->no_keys - schema->no_primary_keys;

	switch(wq->msg_type)
	{
		case RPC_TYPE_WRITE:
		{
			assert(wq->cell->no_columns > 0 || (wq->cell->last_blob != NULL && wq->cell->last_blob_size > 0));

			WORD * column_values = (WORD *) malloc(total_cols_plus_blob * sizeof(WORD));

			int j = 0;
			for(;j<wq->cell->no_keys;j++)
				column_values[j] = (WORD) wq->cell->keys[j];
			for(;j<total_cols;j++)
				column_values[j] = (WORD) wq->cell->columns[j-wq->cell->no_keys];

			if(wq->cell->last_blob_size > 0)
			{
				assert(total_cols_plus_blob == total_cols + 1);
				column_values[total_cols] = malloc(wq->cell->last_blob_size);
				memcpy(column_values[total_cols], wq->cell->last_blob, wq->cell->last_blob_size);
			}


			if(wq->txnid == NULL) // Write out of txn
				return db_insert_transactional(column_values, total_cols_plus_blob, no_clustering_keys, wq->cell->last_blob_size, wq->cell->version, (WORD) wq->cell->table_key, db, fastrandstate);
			else // Write in txn
				return db_insert_in_txn(column_values, total_cols_plus_blob, schema->no_primary_keys, no_clustering_keys, wq->cell->last_blob_size, (WORD) wq->cell->table_key, wq->txnid, db, fastrandstate);
		}
		case RPC_TYPE_DELETE:
		{
			if(wq->txnid == NULL) // Delete out of txn
			{
				if(wq->cell->no_keys == schema->no_primary_keys)
					return db_delete_row_transactional((WORD *) wq->cell->keys, wq->cell->version, (WORD) wq->cell->table_key, db, fastrandstate);
				else
					assert(0); // db_delete_cell not implemented yet
			}
			else
			{
				if(wq->cell->no_keys == schema->no_primary_keys)
					return db_delete_row_in_txn((WORD *) wq->cell->keys, wq->cell->no_keys, (WORD) wq->cell->table_key, wq->txnid, db, fastrandstate);
				else
					return db_delete_cell_in_txn((WORD *) wq->cell->keys, schema->no_primary_keys, no_clustering_keys, (WORD) wq->cell->table_key, wq->txnid, db, fastrandstate);
				// TO DO: To support db_delete_by_index_in_txn (in RPCs and backend)
			}
		}
	}

	return 1;
}

vector_clock * get_empty_vc()
{
	return init_vc(0, NULL, NULL, 0);
}

vector_clock * get_local_vc(int my_id)
{
	int node_ids[] = {my_id};
	long counters[] = {0};
	return init_vc(1, node_ids, counters, 0);
}

// Read (regular and range) message handlers:

int count_cells(db_row_t* result, int * max_depth)
{
	if(result == NULL)
		return 0;

	if(result->cells == NULL || result->cells->no_items == 0)
		return 1;

	*max_depth = *max_depth + 1;

	int no_cells = 0;
	for(snode_t * crt_cell = HEAD(result->cells); crt_cell != NULL; crt_cell = NEXT(crt_cell))
	{
		db_row_t * child = (db_row_t*) crt_cell->value;
		no_cells += count_cells(child, max_depth);
	}

	return no_cells;
}

cell * serialize_cells(db_row_t* result, cell * cells, long table_key, long * key_path, int depth, int no_schema_keys)
{
	if(result == NULL)
		return cells;

	key_path[depth-1] = (long) result->key;

	if(result->cells == NULL || result->cells->no_items == 0)
	{
		assert(result->no_columns > 0);
		assert(depth >= no_schema_keys);

		if(result->last_blob_size <= 0)
			copy_cell(cells, table_key,
					key_path, depth,
					(long *) result->column_array, result->no_columns,
					NULL, 0,
					result->version);
		else
			copy_cell(cells, table_key,
					key_path, depth,
					(long *) result->column_array, result->no_columns - 1,
					result->column_array[result->no_columns - 1], result->last_blob_size,
					result->version);

		return cells + 1;
	}

//	printf("serialize_cells:\n");
//	print_long_row(result);

	cell * cells_ptr = cells;
	for(snode_t * crt_cell = HEAD(result->cells); crt_cell != NULL; crt_cell = NEXT(crt_cell))
	{
		db_row_t * child = (db_row_t*) crt_cell->value;
		cells_ptr = serialize_cells(child, cells_ptr, table_key, key_path, depth+1, no_schema_keys);
	}

	return cells_ptr;
}

int get_read_response_packet(db_row_t* result, read_query * q, db_schema_t * schema, void ** snd_buf, unsigned * snd_msg_len)
{
	range_read_response_message * m = NULL;

	if(result == NULL)
	{
		m = init_range_read_response_message(NULL, 0, q->txnid, q->nonce);
	}
	else if(result->cells == NULL || result->cells->no_items == 0)
	// Return a single cell read result
	{
		assert(result->no_columns > 0);
		assert(result->no_columns >= schema->min_no_cols);
		assert(q->cell_address->keys[q->cell_address->no_keys - 1] == (long) result->key);

		cell * c = NULL;
		if(result->last_blob_size <= 0)
		{
			c = init_cell(q->cell_address->table_key,
							(long *) &result->key, 1, // Result cell always points to last (inner-most) key of the query
							(long *) result->column_array, result->no_columns,
							NULL, 0,
							result->version);
		}
		else
		{
			c = init_cell(q->cell_address->table_key,
							(long *) &result->key, 1, // Result cell always points to last (inner-most) key of the query
							(long *) result->column_array, result->no_columns - 1,
							result->column_array[result->no_columns - 1], result->last_blob_size,
							result->version);
		}

		m = init_range_read_response_message(c, 1, q->txnid, q->nonce);
	}
	else
	// Return a multi-cell read result; traverse db_row downwards and get all child cells recursively:
	{
		int schema_keys = schema->no_primary_keys + schema->min_no_clustering_keys; // We only use this schema data for sanity checking of read back results
//		int no_keys = schema_keys - q->cell_address->no_keys + 1;

		int max_depth = 1;

		int no_results = count_cells(result, &max_depth);

		cell * cells = malloc(no_results * sizeof(cell));

		long * key_path = (long *) malloc(max_depth * sizeof(long));

		cell * last_cell_ptr = serialize_cells(result, cells, q->cell_address->table_key, key_path, 1, schema_keys); // no_keys

		assert(last_cell_ptr - cells == no_results);

		m = init_range_read_response_message(cells, no_results, q->txnid, q->nonce);
	}

#if (VERBOSE_RPC > 0)
	char print_buff[4096];
	to_string_range_read_response_message(m, (char *) print_buff);
	printf("Sending range read response message: %s\n", print_buff);
#endif

	int ret = serialize_range_read_response_message(m, snd_buf, snd_msg_len);

	free_range_read_response_message(m);

	return ret;
}

db_row_t* handle_read_query(read_query * q, db_schema_t ** schema, db_t * db, unsigned int * fastrandstate)
{
	int i=0;

	*schema = get_schema(db, (WORD) q->cell_address->table_key);
	int no_clustering_keys = q->cell_address->no_keys - (*schema)->no_primary_keys;

	if(no_clustering_keys == 0)
	{
		return db_search((WORD *) q->cell_address->keys, (WORD) q->cell_address->table_key, db);
	}
	else
	{
		return db_search_clustering((WORD *) q->cell_address->keys,
									(WORD *) (q->cell_address->keys + (*schema)->no_primary_keys),
									no_clustering_keys, (WORD) q->cell_address->table_key, db);
	}
}

int get_range_read_response_packet(snode_t* start_row, snode_t* end_row, int no_results, range_read_query * q, db_schema_t * schema, void ** snd_buf, unsigned * snd_msg_len)
{
	int schema_keys = schema->no_primary_keys + schema->min_no_clustering_keys; // We only use this schema data for sanity checking of read back results
//	int no_keys = schema_keys - q->start_cell_address->no_keys + 1;
	range_read_response_message * m = NULL;

	if(no_results == 0)
	{
		m = init_range_read_response_message(NULL, 0, q->txnid, q->nonce);
	}
	else
	{
		assert(start_row != NULL);

		int max_range_depth = 0;
		int no_cells = 0, i=0;
		for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
		{
			db_row_t* result = (db_row_t* ) crt_row->value;
//			print_long_row(result);
			int max_depth = 1;
			no_cells += count_cells(result, &max_depth);
			max_range_depth = (max_depth > max_range_depth)?max_depth:max_range_depth;
		}

		cell * cells = malloc(no_cells * sizeof(cell));

		long * key_path = (long *) malloc(max_range_depth * sizeof(long));

		i=0;
		cell * last_cell_ptr = cells;
		for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
		{
			db_row_t* result = (db_row_t* ) crt_row->value;
			last_cell_ptr = serialize_cells(result, last_cell_ptr, q->start_cell_address->table_key, key_path, 1, schema_keys); // no_keys
		}

		assert(last_cell_ptr - cells == no_cells);

		m = init_range_read_response_message(cells, no_cells, q->txnid, q->nonce);
	}

#if (VERBOSE_RPC > 0)
		char print_buff[4096];
		to_string_range_read_response_message(m, (char *) print_buff);
		printf("Sending range read response message: %s\n", print_buff);
#endif

		int ret = serialize_range_read_response_message(m, snd_buf, snd_msg_len);

		free_range_read_response_message(m);

		return ret;
}

int handle_range_read_query(range_read_query * q,
							snode_t** start_row, snode_t** end_row, db_schema_t ** schema,
							db_t * db, unsigned int * fastrandstate)
{
	int i=0;

	assert(q->start_cell_address->table_key == q->end_cell_address->table_key);
	assert(q->start_cell_address->no_keys == q->end_cell_address->no_keys);

	*schema = get_schema(db, (WORD) q->start_cell_address->table_key);

	int no_clustering_keys = q->start_cell_address->no_keys - (*schema)->no_primary_keys;

	if(no_clustering_keys == 0)
	{
		return db_range_search((WORD *) q->start_cell_address->keys, (WORD *) q->end_cell_address->keys, start_row, end_row, (WORD) q->start_cell_address->table_key, db);
	}
	else
	{
		return db_range_search_clustering((WORD *) q->start_cell_address->keys,
										(WORD *) (q->start_cell_address->keys + (*schema)->no_primary_keys),
										(WORD *) (q->end_cell_address->keys + (*schema)->no_primary_keys),
										no_clustering_keys, start_row, end_row, (WORD) q->start_cell_address->table_key, db);
	}
}

// Queue message handlers:

int get_queue_ack_packet(int status, queue_query_message * q,
					void ** snd_buf, unsigned * snd_msg_len)
{
	ack_message * ack = init_ack_message(q->cell_address, status, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
	char print_buff[1024];
	to_string_ack_message(ack, (char *) print_buff);
	printf("Sending queue ack message: %s\n", print_buff);
#endif

	int ret = serialize_ack_message(ack, snd_buf, snd_msg_len);

	free_ack_message(ack);

	return ret;
}

int get_queue_read_response_packet(snode_t* start_row, snode_t* end_row, int no_results,
									long new_read_head, int status, db_schema_t * schema,
									queue_query_message * q,
									void ** snd_buf, unsigned * snd_msg_len)
{
	queue_query_message * m = NULL;

	if(no_results == 0)
	{
		m = init_read_queue_response(q->cell_address, NULL, 0, q->app_id, q->shard_id, q->consumer_id, new_read_head, (short) status, q->txnid, q->nonce);
	}
	else
	{
		assert(start_row != NULL);

		cell * cells = malloc(no_results * sizeof(cell));

		int i=0;
		long prev_id = -1;
		for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
		{
			db_row_t* result = (db_row_t* ) crt_row->value;
			long id = (long) result->key;
			assert(i==0 || prev_id == (id - 1));
			prev_id = id;
			if(result->last_blob_size <= 0)
				copy_cell(cells+i, q->cell_address->table_key,
						(long *) &result->key, 1,
						(long *) result->column_array, result->no_columns,
						NULL, 0,
						result->version);
			else
				copy_cell(cells+i, q->cell_address->table_key,
						(long *) &result->key, 1,
						(long *) result->column_array, result->no_columns - 1,
						result->column_array[result->no_columns - 1], result->last_blob_size,
						result->version);
		}

		m = init_read_queue_response(q->cell_address, cells, no_results, q->app_id, q->shard_id, q->consumer_id, new_read_head, (short) status, q->txnid, q->nonce);
	}

#if (VERBOSE_RPC > 0)
		char print_buff[1024];
		to_string_queue_message(m, (char *) print_buff);
		printf("Sending read queue response message: %s\n", print_buff);
#endif

		int ret = serialize_queue_message(m, snd_buf, snd_msg_len, 0);

		free_queue_message(m);

		return ret;
}

int handle_create_queue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
	if(q->txnid == NULL) // Create queue out of txn
		return create_queue((WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], NULL, 1, db, fastrandstate);
	else // Create queue in txn
		return create_queue_in_txn((WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], q->txnid, db, fastrandstate);
}

int handle_delete_queue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
	if(q->txnid == NULL)
		return delete_queue((WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], NULL, 1, db, fastrandstate);
	else
		return delete_queue_in_txn((WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], q->txnid, db, fastrandstate);
}

int handle_subscribe_queue(queue_query_message * q, int * clientfd, long * prev_read_head, long * prev_consume_head, db_t * db, unsigned int * fastrandstate)
{
	if(q->txnid != NULL) // Create queue out of txn
	{
		assert(0); // Subscriptions in txns are not supported yet
		return 1;
	}
	else
		return register_remote_subscribe_queue((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id, (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0],
										clientfd, prev_read_head, prev_consume_head, 1, db, fastrandstate);
}

int handle_unsubscribe_queue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
	if(q->txnid != NULL) // Create queue out of txn
	{
		assert(0); // Unsubscriptions in txns are not supported yet
		return 1;
	}
	else
		return unsubscribe_queue((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id, (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], 1, db);
}

int handle_enqueue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
	assert(q->no_cells > 0 && q->cells != NULL);

	int status = -1;

	for(int i=0;i<q->no_cells;i++)
	{
		int total_cols = q->cells[i].no_keys + q->cells[i].no_columns;
		int total_cols_plus_blob = total_cols + ((q->cells[i].last_blob_size > 0)?(1):(0));

		long * column_values = (long *) malloc(total_cols_plus_blob * sizeof(long));

		int j = 0;
		for(;j<q->cells[i].no_keys;j++)
			column_values[j] = q->cells[i].keys[j];
		for(;j<total_cols;j++)
			column_values[j] = q->cells[i].columns[j-q->cells[i].no_keys];

		if(q->cells[i].last_blob_size > 0)
		{
			assert(total_cols_plus_blob == total_cols + 1);
			column_values[total_cols] = (long) malloc(q->cells[i].last_blob_size);
			memcpy((WORD) column_values[total_cols], q->cells[i].last_blob, q->cells[i].last_blob_size);
		}

		// Below will automatically trigger remote consumer notifications on the queue, either immediately or upon txn commit:
		if(q->txnid == NULL) // Enqueue out of txn
			status = enqueue((WORD *) column_values, total_cols_plus_blob, q->cells[i].last_blob_size, (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], 1, db, fastrandstate);
		else // Enqueue in txn
			status = enqueue_in_txn((WORD *) column_values, total_cols_plus_blob, q->cells[i].last_blob_size, (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], q->txnid, db, fastrandstate);

		if(status != 0)
			break;
	}

	return status;
}

int handle_read_queue(queue_query_message * q,
						int * entries_read, long * new_read_head, vector_clock ** prh_version,
						snode_t** start_row, snode_t** end_row, db_schema_t ** schema,
						db_t * db, unsigned int * fastrandstate)
{
	*schema = get_schema(db, (WORD) q->cell_address->table_key);

	if(q->txnid == NULL) // Read queue out of txn
		return read_queue((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id,
							(WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], q->queue_index,
							entries_read, new_read_head, prh_version,
							start_row, end_row, 1, db);
	else // Read queue in txn
		return read_queue_in_txn((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id,
									(WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0],
									(int) q->queue_index, entries_read, new_read_head,
									start_row, end_row, q->txnid, db, fastrandstate);
}

int handle_consume_queue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
	if(q->txnid == NULL) // Consume queue out of txn
		return consume_queue((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id,
							(WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0],
							q->queue_index, db);
	else // Consume queue in txn
		return consume_queue_in_txn((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id,
									(WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0],
									q->queue_index, q->txnid, db, fastrandstate);
}

// Txn messages handlers:

int get_txn_ack_packet(int status, txn_message * q,
					void ** snd_buf, unsigned * snd_msg_len)
{
	ack_message * ack = init_ack_message(NULL, status, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
	char print_buff[1024];
	to_string_ack_message(ack, (char *) print_buff);
	printf("Sending txn ack message: %s\n", print_buff);
#endif

	int ret = serialize_ack_message(ack, snd_buf, snd_msg_len);

	free_ack_message(ack);

	return ret;
}


int handle_new_txn(txn_message * q, db_t * db, unsigned int * fastrandstate)
{
	assert(q->txnid != NULL);

	txn_state * ts = get_txn_state(q->txnid, db);

	if(ts != NULL)
		return -2; // txnid already exists on server

	ts = init_txn_state();

	memcpy(&ts->txnid, q->txnid, sizeof(uuid_t));

	skiplist_insert(db->txn_state, (WORD) &(ts->txnid), (WORD) ts, fastrandstate);

	return 0;
}

int handle_validate_txn(txn_message * q, db_t * db, unsigned int * fastrandstate)
{
	assert(q->txnid != NULL);
	assert(q->version != NULL);

	return validate_txn(q->txnid, q->version, db);
}

int handle_commit_txn(txn_message * q, db_t * db, unsigned int * fastrandstate)
{
	assert(q->txnid != NULL);

	txn_state * ts = get_txn_state(q->txnid, db);

	// Make sure the txn has the right commit stamp (it c'd be that the current server missed the previous validation packet so the version was not set then):

	set_version(ts, q->version);

	if(ts == NULL)
		return -2; // txnid doesn't exist on server

	return persist_txn(ts, db, fastrandstate);
}

int handle_abort_txn(txn_message * q, db_t * db, unsigned int * fastrandstate)
{
	assert(q->txnid != NULL);

	return abort_txn(q->txnid, db);
}

int handle_socket_close(int * childfd)
{
	struct sockaddr_in address;
	int addrlen;
	getpeername(*childfd, (struct sockaddr*)&address,
				(socklen_t*)&addrlen);
	printf("Host disconnected , ip %s , port %d \n" ,
      inet_ntoa(address.sin_addr) , ntohs(address.sin_port));

	//Close the socket and mark as 0 for reuse:
	close(*childfd);
	*childfd = 0;

	return 0;
}

int add_peer_to_membership(char *hostname, int portno, skiplist_t * peers, unsigned int * seedptr)
{
    remote_server * rs = get_remote_server(hostname, portno);

    if(rs == NULL)
    {
		printf("ERROR: Failed joining server %s:%d (it looks down)!\n", hostname, portno);
    		return 1;
    }

    if(skiplist_search(peers, &rs->serveraddr) != NULL)
    {
		fprintf(stderr, "ERROR: Server address %s:%d was already added to membership!\n", hostname, portno);
		free_remote_server(rs);
		return -1;
    }

    int status = skiplist_insert(peers, &rs->serveraddr, rs, seedptr);

    if(status != 0)
    {
		fprintf(stderr, "ERROR: Error adding server address %s:%d to membership!\n", hostname, portno);
		free_remote_server(rs);
		return -2;
    }

    return 0;
}

typedef struct client_descriptor
{
	struct sockaddr_in addr;
	int sockfd;
	char id[256];
} client_descriptor;

client_descriptor * get_client_descriptor(struct sockaddr_in addr, int sockfd, char *hostname, int portno)
{
	client_descriptor * cd = (client_descriptor *) malloc(sizeof(struct client_descriptor));
	memcpy(&(cd->addr), &addr, sizeof(struct sockaddr_in));
	cd->sockfd = sockfd;
    snprintf((char *) &cd->id, 256, "%s:%d", hostname, portno);
	return cd;
}

void free_client_descriptor(client_descriptor * cd)
{
	free(cd);
}

int add_client_to_membership(struct sockaddr_in addr, int sockfd, char *hostname, int portno, skiplist_t * clients, unsigned int * seedptr)
{
	client_descriptor * cd = get_client_descriptor(addr, sockfd, hostname, portno);

    if(skiplist_search(clients, &(cd->addr)) != NULL)
    {
		fprintf(stderr, "ERROR: Client address %s:%d was already added to membership!\n", hostname, portno);
		free_client_descriptor(cd);
		return -1;
    }

    int status = skiplist_insert(clients, &(cd->addr), cd, seedptr);

    if(status != 0)
    {
		fprintf(stderr, "ERROR: Error adding client address %s:%d to membership!\n", hostname, portno);
		free_client_descriptor(cd);
		return -2;
    }

    return 0;
}

int handle_client_message(int childfd, int msg_len, db_t * db, unsigned int * fastrandstate)
{
    void * tmp_out_buf = NULL, * q = NULL;
    unsigned snd_msg_len;
    short msg_type;
	db_schema_t * schema;
	long nonce = -1;

    int status = parse_message(in_buf + sizeof(int), msg_len, &q, &msg_type, &nonce, 1);

    if(status != 0)
    {
//    		error("ERROR decoding client request");
    		fprintf(stderr, "ERROR decoding client request");
    		return -1;
    }

    switch(msg_type)
    {
    		case RPC_TYPE_WRITE:
    		{
    			status = handle_write_query((write_query *) q, db, fastrandstate);
    			if(status != 0)
    			{
    				printf("ERROR: handle_write_query returned %d!", status);
    				assert(0);
    			}
    			status = get_ack_packet(status, (write_query *) q, &tmp_out_buf, &snd_msg_len);
    			break;
    		}
    		case RPC_TYPE_READ:
    		{
    			db_row_t* result = handle_read_query((read_query *) q, &schema, db, fastrandstate);
    			status = get_read_response_packet(result, (read_query *) q, schema, &tmp_out_buf, &snd_msg_len);
    			break;
    		}
    		case RPC_TYPE_RANGE_READ:
    		{
    			snode_t * start_row = NULL, * end_row = NULL;
    			int no_results = handle_range_read_query((range_read_query *) q, &start_row, &end_row, &schema, db, fastrandstate);
    			status = get_range_read_response_packet(start_row, end_row, no_results, (range_read_query *) q, schema, &tmp_out_buf, &snd_msg_len);
    			break;
    		}
    		case RPC_TYPE_QUEUE:
    		{
    			queue_query_message * qm = (queue_query_message *) q;

    			switch(qm->msg_type)
    			{
    				case QUERY_TYPE_CREATE_QUEUE:
    				{
    					status = handle_create_queue(qm, db, fastrandstate);
    					assert(status == 0);
    					status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len);
    					break;
    				}
    				case QUERY_TYPE_DELETE_QUEUE:
    				{
    					status = handle_delete_queue(qm, db, fastrandstate);
    					assert(status == 0);
    					status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len);
    					break;
    				}
    				case QUERY_TYPE_SUBSCRIBE_QUEUE:
    				{
    					long prev_read_head = -1, prev_consume_head = -1;
    					status = handle_subscribe_queue(qm, &childfd, &prev_read_head, &prev_consume_head, db, fastrandstate);
    					assert(status == 0);
    					status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len);
    					break;
    				}
    				case QUERY_TYPE_UNSUBSCRIBE_QUEUE:
    				{
    					status = handle_unsubscribe_queue(qm, db, fastrandstate);
    					assert(status == 0);
    					status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len);
    					break;
    				}
    				case QUERY_TYPE_ENQUEUE:
    				{
    					status = handle_enqueue(qm, db, fastrandstate);
    					assert(status == 0);
    					status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len);

    					break;
    				}
    				case QUERY_TYPE_READ_QUEUE:
    				{
    					int entries_read = 0;
    					long new_read_head = -1;
    					vector_clock * prh_version = NULL;
    					snode_t * start_row = NULL, * end_row = NULL;
    					db_schema_t * schema = NULL;
    					status = handle_read_queue(qm, &entries_read, &new_read_head, &prh_version,
    													&start_row, &end_row, &schema, db, fastrandstate);
    					assert(status == QUEUE_STATUS_READ_COMPLETE || status == QUEUE_STATUS_READ_INCOMPLETE);
    					status = get_queue_read_response_packet(start_row, end_row, entries_read, new_read_head, status, schema, qm, &tmp_out_buf, &snd_msg_len);

    					break;
    				}
    				case QUERY_TYPE_CONSUME_QUEUE:
    				{
    					status = handle_consume_queue(qm, db, fastrandstate);
//    					assert(status == (int) qm->queue_index);
    					status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len);
    					break;
    				}
    				default:
    				{
    					assert(0);
    				}
    			}

    			break;
    		}
    		case RPC_TYPE_TXN:
    		{
    			txn_message * tm = (txn_message *) q;

    			switch(tm->type)
    			{
    				case DB_TXN_BEGIN:
    				{
    					status = handle_new_txn(tm, db, fastrandstate);
    					assert(status == 0 || status == -2);
    					status = get_txn_ack_packet(status, tm, &tmp_out_buf, &snd_msg_len);

    					break;
    				}
    				case DB_TXN_VALIDATION:
    				{
    					status = handle_validate_txn(tm, db, fastrandstate);
    					assert(status == VAL_STATUS_COMMIT || status == VAL_STATUS_ABORT);
    					status = get_txn_ack_packet(status, tm, &tmp_out_buf, &snd_msg_len);

    					break;
    				}
    				case DB_TXN_COMMIT:
    				{
    					status = handle_commit_txn(tm, db, fastrandstate);
    					assert(status == 0);
    					status = get_txn_ack_packet(status, tm, &tmp_out_buf, &snd_msg_len);

    					break;
    				}
    				case DB_TXN_ABORT:
    				{
    					status = handle_abort_txn(tm, db, fastrandstate);
    					assert(status == 0);
    					status = get_txn_ack_packet(status, tm, &tmp_out_buf, &snd_msg_len);

    					break;
    				}
    			}

    			break;
    		}
    		case RPC_TYPE_ACK:
    		{
    			assert(0); // S'dn't happen currently
    			break;
    		}
		default:
		{
			assert(0);
		}
    }

    assert(status == 0);

    int n = write(childfd, tmp_out_buf, snd_msg_len);
    if (n < 0)
      error("ERROR writing to socket");

    free(tmp_out_buf);

    return 0;
}

int handle_server_message(int childfd, int msg_len, db_t * db, unsigned int * fastrandstate)
{
	return 0;
}

int read_full_packet(int * sockfd, int * msg_len)
{
	int announced_msg_len = -1;
	int read_buf_offset = 0;
	int status = 0;

	while(1) // Loop until reading complete packet:
	{
		assert(read_buf_offset < SERVER_BUFSIZE - sizeof(int));

		if(read_buf_offset == 0)
		{
			// Read msg len header from packet:

			bzero(in_buf, SERVER_BUFSIZE);
			*msg_len = -1;

			int size_len = read(*sockfd, in_buf, sizeof(int));

			if (size_len < 0)
			{
				fprintf(stderr, "ERROR reading from socket\n");
				continue;
			}
			else if (size_len == 0)
			{
				handle_socket_close(sockfd);
				status = 1;
				break;
			}

			announced_msg_len = *((int *)in_buf);

			*((int *)in_buf) = 0; // 0 back buffer

			read_buf_offset = 0;
		}

		if(announced_msg_len <= 0)
		{
			read_buf_offset = 0;
			continue;
		}

	    *msg_len = read(*sockfd, in_buf + sizeof(int) + read_buf_offset, announced_msg_len - read_buf_offset);

#if SERVER_VERBOSITY > 1
		printf("announced_msg_len=%d, msg_len=%d, read_buf_offset=%d\n", announced_msg_len, *msg_len, read_buf_offset);
#endif

	    if (*msg_len < 0)
	    {
	    		fprintf(stderr, "ERROR reading from socket\n");
			continue;
	    }
		else if(*msg_len == 0) // client closed socket
	    {
			handle_socket_close(sockfd);
			status = 1;
	        break;
	    }
		else if(*msg_len < announced_msg_len - read_buf_offset)
		{
			read_buf_offset += *msg_len;
			continue; // Continue reading socket until full packet length
		}

	    break;
	}

    assert(status != 0 || announced_msg_len == *msg_len);

//    read_buf_offset = 0; // Reset

#if SERVER_VERBOSITY > 1
    printf("server received %d / %d bytes\n", announced_msg_len, *msg_len);
#endif

	return status;
}


int main(int argc, char **argv) {
  int parentfd;
  int childfd;
  int portno;
  int clientlen;
  struct sockaddr_in serveraddr;
  struct sockaddr_in clientaddr;
  struct hostent *hostp;
  char *hostaddrp;
  int optval; /* flag value for setsockopt */
  int msg_len; /* message byte size */
  fd_set readfds;
  struct timeval timeout;
  timeout.tv_sec = 3;
  timeout.tv_usec = 0;
  unsigned int seed;
  int ret = 0;
  char msg_buf[256];

  skiplist_t * clients = create_skiplist(&sockaddr_cmp); // List of remote clients
  skiplist_t * peers = create_skiplist(&sockaddr_cmp); // List of peers

  if (argc != 2) {
    fprintf(stderr, "usage: %s <port>\n", argv[0]);
    exit(1);
  }
  portno = atoi(argv[1]);

  GET_RANDSEED(&seed, 0); // thread_id

  // Get db pointer:
  db_t * db = get_db();

  // Create schema:
  ret = create_state_schema(db, &seed);
  printf("Test %s - %s\n", "create_state_schema", ret==0?"OK":"FAILED");

  ret = create_queue_schema(db, &seed);
  printf("Test %s - %s\n", "create_queue_schema", ret==0?"OK":"FAILED");

  parentfd = socket(AF_INET, SOCK_STREAM, 0);
  if (parentfd < 0)
    error("ERROR opening socket");

  optval = 1;
  setsockopt(parentfd, SOL_SOCKET, SO_REUSEADDR, (const void *)&optval , sizeof(int));

  bzero((char *) &serveraddr, sizeof(serveraddr));
  serveraddr.sin_family = AF_INET;
  serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
  serveraddr.sin_port = htons((unsigned short)portno);

  vector_clock * my_lc = init_local_vc((struct sockaddr *) &serveraddr);

  printf("SERVER: Started %s:%d, my_lc = %s\n", inet_ntoa(serveraddr.sin_addr), serveraddr.sin_port, to_string_vc(my_lc, msg_buf));

  if (bind(parentfd, (struct sockaddr *) &serveraddr, sizeof(serveraddr)) < 0)
    error("ERROR on binding");

  if (listen(parentfd, 100) < 0) /* allow 100 requests to queue up */
    error("ERROR on listen");

  clientlen = sizeof(clientaddr);

  while(1)
  {
		FD_ZERO(&readfds);

		// Add parent socket to read set:

		FD_SET(parentfd, &readfds);
		int max_fd = parentfd;

		// Add active clients to read set:

		for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt))
		{
			client_descriptor * rs = (client_descriptor *) crt->value;
			if(rs->sockfd > 0)
			{
//				printf("SERVER: Listening to client socket %s..\n", rs->id);
				FD_SET(rs->sockfd, &readfds);
				max_fd = (rs->sockfd > max_fd)? rs->sockfd : max_fd;
			}
			else
			{
//				printf("SERVER: Not listening to disconnected client socket %s..\n", rs->id);
			}
		}

		// Add active peers to read set:

		for(snode_t * crt = HEAD(peers); crt!=NULL; crt = NEXT(crt))
		{
			remote_server * rs = (remote_server *) crt->value;
			if(rs->sockfd > 0)
			{
//				printf("SERVER: Listening to peer socket %s..\n", rs->id);
				FD_SET(rs->sockfd, &readfds);
				max_fd = (rs->sockfd > max_fd)? rs->sockfd : max_fd;
			}
			else
			{
//				printf("SERVER: Not listening to disconnected peer socket %s..\n", rs->id);
			}
		}

		int status = select(max_fd + 1 , &readfds , NULL , NULL , &timeout);

		if ((status < 0) && (errno!=EINTR))
		{
			printf("select error!\n");
			assert(0);
		}

		// Check if there's a new connection attempt from a client:

		if (FD_ISSET(parentfd, &readfds))
		{
			  childfd = accept(parentfd, (struct sockaddr *) &clientaddr, &clientlen);
			  if (childfd < 0)
			    error("ERROR on accept");

			  hostp = gethostbyaddr((const char *)&clientaddr.sin_addr.s_addr,
						  sizeof(clientaddr.sin_addr.s_addr), AF_INET);
			  if (hostp == NULL)
			    error("ERROR on gethostbyaddr");
			  hostaddrp = inet_ntoa(clientaddr.sin_addr);
			  if (hostaddrp == NULL)
			    error("ERROR on inet_ntoa\n");

#if SERVER_VERBOSITY > 0
			  printf("SERVER: accepted connection from client: %s (%s:%d)\n", hostp->h_name, hostaddrp, clientaddr.sin_port);
#endif
			  ret = add_client_to_membership(clientaddr, childfd, inet_ntoa(clientaddr.sin_addr), clientaddr.sin_port, clients, &seed); // hostp->h_name

//			  assert(ret == 0);
		}

		// Check if there are messages from existing clients:

		for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt))
		{
			client_descriptor * rs = (client_descriptor *) crt->value;
			if(rs->sockfd > 0 && FD_ISSET(rs->sockfd , &readfds))
			// Received a msg from this client:
			{
				if(read_full_packet(&(rs->sockfd), &msg_len))
					continue;

			    if(handle_client_message(childfd, msg_len, db, &seed))
			    		continue;
			}
		}

		// Check if there are messages from peer servers:

		for(snode_t * crt = HEAD(peers); crt!=NULL; crt = NEXT(crt))
		{
			remote_server * rs = (remote_server *) crt->value;

			if(rs->sockfd > 0 && FD_ISSET(rs->sockfd , &readfds))
			// Received a msg from this server:
			{
				if(read_full_packet(&(rs->sockfd), &msg_len))
					continue;

			    if(handle_server_message(childfd, msg_len, db, &seed))
			    		continue;
			}
		}
  }

  // Close sockets to clients and peers:

	for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt))
	{
		client_descriptor * rs = (client_descriptor *) crt->value;
		if(rs->sockfd > 0)
		{
			close(rs->sockfd);
		}
	}

	for(snode_t * crt = HEAD(peers); crt!=NULL; crt = NEXT(crt))
	{
		remote_server * rs = (remote_server *) crt->value;
		if(rs->sockfd > 0)
		{
			close(rs->sockfd);
		}
	}
}



