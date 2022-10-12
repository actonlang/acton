/*
 * db.h
 *      Author: aagapi
 */

#ifndef BACKEND_DB_H_
#define BACKEND_DB_H_

#include "skiplist.h"
#include "fastrand.h"
#include "failure_detector/vector_clock.h"

#include <pthread.h>
#include <unistd.h>
#include <inttypes.h>

typedef void *WORD;

// High level API:

#define DB_TYPE_CHAR 0
#define DB_TYPE_INT16 1
#define DB_TYPE_INT32 2
#define DB_TYPE_INT64 3
#define DB_TYPE_FLOAT32 4
#define DB_TYPE_FLOAT64 5
#define DB_TYPE_BLOB 6

// Query types:

#define QUERY_TYPE_UPDATE 0
#define QUERY_TYPE_DELETE 1
#define QUERY_TYPE_READ_COLS 2
#define QUERY_TYPE_READ_CELL 3
#define QUERY_TYPE_READ_CELL_RANGE 4
#define QUERY_TYPE_READ_ROW 5
#define QUERY_TYPE_READ_ROW_RANGE 6
#define QUERY_TYPE_READ_INDEX 7
#define QUERY_TYPE_READ_INDEX_RANGE 8

#define QUERY_TYPE_ENQUEUE 9
#define QUERY_TYPE_READ_QUEUE 10
#define QUERY_TYPE_CONSUME_QUEUE 11
#define QUERY_TYPE_CREATE_QUEUE 12
#define QUERY_TYPE_DELETE_QUEUE 13
#define QUERY_TYPE_SUBSCRIBE_QUEUE 14
#define QUERY_TYPE_UNSUBSCRIBE_QUEUE 15

#define QUERY_TYPE_READ_QUEUE_RESPONSE 16
#define QUERY_TYPE_QUEUE_NOTIFICATION 17

// Return statuses:

#define DB_ERR_NO_TABLE -1
#define DB_ERR_NO_QUEUE -2
#define DB_ERR_NO_CONSUMER -3
#define DB_ERR_QUEUE_COMPLETE -4
#define DB_ERR_QUEUE_HEAD_INVALID -5
#define DB_ERR_DUPLICATE_QUEUE -6
#define DB_ERR_DUPLICATE_CONSUMER -7
#define VAL_STATUS_ABORT_SCHEMA -8

#define QUEUE_STATUS_READ_INCOMPLETE 0
#define QUEUE_STATUS_READ_COMPLETE 1

#define QUEUE_NOTIF_ENQUEUED 0
#define QUEUE_NOTIF_DELETED 1

#define VERBOSE_BACKEND 0
#define MAX_PRINT_BUFF 128 * 1024

#define MULTI_THREADED 0

typedef struct db_schema {
	int * col_types;
	int min_no_cols;

	int * primary_key_idxs;
	int no_primary_keys;

	int * clustering_key_idxs;
	int min_no_clustering_keys;

	int * index_key_idxs;
	int no_index_keys;
} db_schema_t;

typedef struct db_table {
	WORD table_key;
	db_schema_t * schema;
	skiplist_t * rows;
	skiplist_t ** indexes;

	skiplist_t * row_tombstones;

	pthread_mutex_t* lock;
} db_table_t;

// Queues:

typedef struct queue_callback_args
{
	WORD table_key;
	WORD queue_id;

	WORD consumer_id;
	WORD shard_id;
	WORD app_id;

	int status;
} queue_callback_args;

typedef struct queue_callback
{
	void (*callback)(queue_callback_args *);
	pthread_mutex_t * lock;
	pthread_cond_t * signal;
} queue_callback;

typedef struct consumer_state {
	WORD consumer_id;
	WORD shard_id;
	WORD app_id;

	int64_t private_read_head;
	int64_t private_consume_head;

	vector_clock * prh_version;
	vector_clock * pch_version;

	short notified;

	queue_callback* callback; // For local subscribers
	int * sockfd; // For remote subscribers
} consumer_state;

// Cells:

typedef struct db_cell {
	WORD key;
	skiplist_t * cells;
	WORD * column_array;
	int no_columns;
	int last_blob_size;

	// Queue metadata:
	skiplist_t * consumer_state; // TO DO: Change to hash table, add indexing by shard_id and app_id
	int64_t no_entries;
	pthread_mutex_t* enqueue_lock;
	pthread_mutex_t* read_lock;
	pthread_mutex_t* subscribe_lock;

	vector_clock * version;

	struct db_cell_t * _next;
} db_cell_t;

typedef db_cell_t db_row_t;

typedef struct db {
    skiplist_t * tables;
    skiplist_t * txn_state;
#if (MULTI_THREADED == 1)
    pthread_mutex_t* txn_state_lock;
#endif
} db_t;

// DB high level API:

// DB, schema and table manipulation:

db_t * get_db();
int db_delete_db(db_t * db);
int db_dump_db(db_t * db);

db_schema_t* db_create_schema(int * col_types, int no_cols, int * primary_key_idxs, int no_primary_keys, int * clustering_key_idxs, int no_clustering_keys, int * index_key_idxs, int no_index_keys);
void free_schema(db_schema_t * schema);
int db_create_table(WORD table_key, db_schema_t* schema, db_t * db, unsigned int * fastrandstate);
int db_create_index(int new_index, WORD table_key, db_t * db, unsigned int * fastrandstate);
int db_delete_table(WORD table_key, db_t * db);

// DB queries:

int db_insert(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, WORD table_key, db_t * db, unsigned int * fastrandstate);
int db_insert_transactional(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, vector_clock * version, WORD table_key, db_t * db, unsigned int * fastrandstate);
int db_update(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, int * col_idxs, WORD table_key, db_t * db);
int db_update_transactional(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, int * col_idxs, vector_clock * version, WORD table_key, db_t * db);
db_row_t* db_search(WORD* primary_keys, WORD table_key, db_t * db);
int db_range_search(WORD* start_primary_keys, WORD* end_primary_keys, snode_t** start_row, snode_t** end_row, WORD table_key, db_t * db);
int db_range_search_copy(WORD* start_primary_keys, WORD* end_primary_keys, db_row_t** rows, WORD table_key, db_t * db);
db_row_t* db_search_clustering(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, db_t * db);
int db_range_search_clustering(WORD* primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, snode_t** start_row, snode_t** end_row, WORD table_key, db_t * db);
WORD* db_search_columns(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, int* column_idxs, int no_columns, WORD table_key, db_t * db);
db_row_t* db_search_index(WORD index_key, int idx_idx, WORD table_key, db_t * db);
int db_range_search_index(int idx_idx, WORD start_idx_key, WORD end_idx_key, snode_t** start_row, snode_t** end_row, WORD table_key, db_t * db);
int db_delete_row(WORD* primary_keys, WORD table_key, db_t * db, unsigned int * fastrandstate);
int db_delete_row_transactional(WORD* primary_keys, vector_clock * version, WORD table_key, db_t * db, unsigned int * fastrandstate);
// TO DO: int db_delete_cell(WORD* keys, int no_primary_keys, int no_clustering_keys, WORD table_key, db_t * db);
int db_delete_by_index(WORD index_key, int idx_idx, WORD table_key, db_t * db);
int db_verify_cell_version(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, vector_clock * version, db_t * db);
int db_verify_row_range_version(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys, WORD table_key,
									int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_t * db);
int db_verify_cell_range_version(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, WORD table_key,
									int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_t * db);
int db_verify_index_version(WORD index_key, int idx_idx, WORD table_key, vector_clock * version, db_t * db);
int db_verify_index_range_version(int idx_idx, WORD start_idx_key, WORD end_idx_key,
									int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, WORD table_key, db_t * db);

// Lower level API:

db_row_t * create_db_row(WORD * column_values, db_schema_t * schema, size_t last_blob_size, unsigned int * fastrandstate);
db_row_t * create_db_row_schemaless(WORD * column_values, int * primary_key_idxs, int no_primary_keys,
									int * clustering_key_idxs, int no_clustering_keys, int no_schema_clustering_keys,
									int no_cols, size_t last_blob_size, unsigned int * fastrandstate);
// Assumes key indexes are in order (rartition keys, followed by clustering keys, followed by columns). Also assumes a single partition key
db_row_t * create_db_row_schemaless2(WORD * keys, int no_keys, WORD * cols, int no_cols, WORD last_blob, size_t last_blob_size, unsigned int * fastrandstate);
void free_db_row(db_row_t * row, db_schema_t * schema);
void long_row_to_string(db_row_t* row, char * to_string, int * len, char * orig_offset);
void print_long_db(db_t * db);
void print_long_table(db_table_t * table);
void print_long_row(db_row_t* row);

int table_insert(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, vector_clock * version, db_table_t * table, unsigned int * fastrandstate);
int table_update(WORD * column_values, int no_cols, int no_clustering_keys, size_t last_blob_size, int * col_idxs, vector_clock * version, db_table_t * table);
db_row_t* table_search(WORD* primary_keys, db_table_t * table);
int table_range_search(WORD* start_primary_keys, WORD* end_primary_keys, snode_t** start_row, snode_t** end_row, db_table_t * table);
int table_range_search_copy(WORD* start_primary_keys, WORD* end_primary_keys, db_row_t** rows, db_table_t * table);
db_row_t* table_search_clustering(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, db_table_t * table);
int table_range_search_clustering(WORD* primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, snode_t** start_row, snode_t** end_row, db_table_t * table);
WORD* table_search_columns(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, int* column_idxs, int no_columns, db_table_t * table);
db_row_t* table_search_index(WORD index_key, int idx_idx, db_table_t * table);
int table_range_search_index(int idx_idx, WORD start_idx_key, WORD end_idx_key, snode_t** start_row, snode_t** end_row, db_table_t * table);
int table_delete_row(WORD* primary_keys, vector_clock * version, db_table_t * table, unsigned int * fastrandstate);
int table_delete_by_index(WORD index_key, int idx_idx, db_table_t * table);
int table_verify_cell_version(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, vector_clock * version, db_table_t * table);
int table_verify_row_range_version(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
										int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_table_t * table);
int table_verify_cell_range_version(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
										int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_table_t * table);
int table_verify_index_version(WORD index_key, int idx_idx, vector_clock * version, db_table_t * table);
int table_verify_index_range_version(int idx_idx, WORD start_idx_key, WORD end_idx_key,
										int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results, db_table_t * table);

queue_callback_args * get_queue_callback_args(WORD table_key, WORD queue_id, WORD app_id, WORD shard_id, WORD consumer_id, int status);
void free_queue_callback_args(queue_callback_args * qca);
queue_callback * get_queue_callback(void (*callback)(queue_callback_args *));
int wait_on_queue_callback(queue_callback *);
void free_queue_callback(queue_callback * qc);

#endif /* BACKEND_DB_H_ */
