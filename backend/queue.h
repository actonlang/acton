/*
 * queue.h
 *
 *      Author: aagapi
 */

#include "db.h"

#ifndef BACKEND_QUEUE_H_
#define BACKEND_QUEUE_H_

int enqueue(WORD * column_values, int no_cols, size_t last_blob_size, WORD table_key, WORD queue_id, short use_lock, db_t * db, unsigned int * fastrandstate);
int read_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, int64_t * new_read_head, vector_clock ** prh_version,
		snode_t** start_row, snode_t** end_row, short use_lock,
		db_t * db);
int peek_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int64_t offset, int * entries_read, int64_t * new_read_head, vector_clock ** prh_version,
		snode_t** start_row, snode_t** end_row, db_t * db);
int replay_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int64_t replay_offset, int max_entries,
		int * entries_read, int64_t * new_replay_offset,
		snode_t** start_row, snode_t** end_row,
		db_t * db);
int consume_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					int64_t new_consume_head, db_t * db);
int subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
						short use_lock, db_t * db, unsigned int * fastrandstate);
int register_remote_subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					int * sockfd, int64_t * prev_read_head, int64_t * prev_consume_head,
					short use_lock, db_t * db, unsigned int * fastrandstate);
int unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						short use_lock, db_t * db);
int create_queue(WORD table_key, WORD queue_id, vector_clock * version, short use_lock,
					db_t * db, unsigned int * fastrandstate);
int delete_queue(WORD table_key, WORD queue_id, vector_clock * version, short use_lock, db_t * db, unsigned int * fastrandstate);
int create_queue_table(WORD table_id, int no_cols, int * col_types,
						db_t * db, unsigned int * fastrandstate);
int set_private_read_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
							int64_t new_read_head, vector_clock * version, short use_lock, db_t * db);
int set_private_consume_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
							int64_t new_consume_head, vector_clock * version, db_t * db);

#endif /* BACKEND_QUEUE_H_ */
