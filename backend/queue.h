/*
 * queue.h
 *
 *      Author: aagapi
 */

#include "db.h"

#ifndef BACKEND_QUEUE_H_
#define BACKEND_QUEUE_H_

#define DB_ERR_NO_TABLE -1
#define DB_ERR_NO_QUEUE -2
#define DB_ERR_NO_CONSUMER -3
#define DB_ERR_QUEUE_COMPLETE -4
#define DB_ERR_QUEUE_HEAD_INVALID -5
#define DB_ERR_DUPLICATE_QUEUE -6
#define DB_ERR_DUPLICATE_CONSUMER -7

#define QUEUE_STATUS_READ_INCOMPLETE 0
#define QUEUE_STATUS_READ_COMPLETE 1

#define QUEUE_NOTIF_ENQUEUED 0
#define QUEUE_NOTIF_DELETED 1

int enqueue(WORD * column_values, int no_cols, WORD table_key, WORD queue_id, short use_lock, db_t * db, unsigned int * fastrandstate);
int read_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, long * new_read_head,
		snode_t** start_row, snode_t** end_row, short use_lock,
		db_t * db);
int peek_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, long offset, int * entries_read, long * new_read_head,
		snode_t** start_row, snode_t** end_row, db_t * db);
int replay_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		long replay_offset, int max_entries,
		int * entries_read, long * new_replay_offset,
		snode_t** start_row, snode_t** end_row,
		db_t * db);
int consume_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					long new_consume_head, db_t * db);
int subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
						short use_lock, db_t * db, unsigned int * fastrandstate);
int unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						short use_lock, db_t * db);
int create_queue(WORD table_key, WORD queue_id, short use_lock,
					db_t * db, unsigned int * fastrandstate);
int delete_queue(WORD table_key, WORD queue_id, short use_lock, db_t * db);
int create_queue_table(WORD table_id, int no_cols, int * col_types,
						db_t * db, unsigned int * fastrandstate);
int set_private_read_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
							long new_read_head, short use_lock, db_t * db);
int set_private_consume_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
							long new_consume_head, db_t * db);

#endif /* BACKEND_QUEUE_H_ */
