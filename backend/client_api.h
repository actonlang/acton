/*
 * client_api.h
 *
 *      Author: aagapi
 */

#ifndef BACKEND_CLIENT_API_H_
#define BACKEND_CLIENT_API_H_

#include "db.h"
#include "failure_detector/db_queries.h"
#include "fastrand.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <uuid/uuid.h>

#define BUFSIZE 4096

#define CLIENT_VERBOSITY 1

// Remote DB API:

typedef struct remote_db {
    int db_id;
    skiplist_t * servers; // List of remote servers
    skiplist_t * txn_state; // Client cache of txn state
    skiplist_t * queue_subscriptions; // Client cache of txn state
    pthread_mutex_t* subscribe_lock;
	int quorum_size;
} remote_db_t;

remote_db_t * get_remote_db(int quorum_size);
int add_server_to_membership(char *hostname, int portno, remote_db_t * db, unsigned int * seedptr);
int free_remote_db(remote_db_t * db);
int close_remote_db(remote_db_t * db);
int sockaddr_cmp(WORD a1, WORD a2);
int queue_callback_cmp(WORD e1, WORD e2);

typedef struct remote_server
{
	char * hostname;
	int portno;
	int sockfd;
	struct sockaddr_in serveraddr;
	struct hostent *server;
	char id[256];
	char in_buf[BUFSIZE];
//	char out_buf[BUFSIZE];
} remote_server;

remote_server * get_remote_server(char *hostname, int portno);
void free_remote_server(remote_server * rs);

// Write ops:

int remote_insert_in_txn(WORD * column_values, int no_cols, WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_update_in_txn(int * col_idxs, int no_cols, WORD * column_values, WORD table_key, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_delete_row_in_txn(WORD * column_values, int no_cols, WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_delete_cell_in_txn(WORD * column_values, int no_cols, int no_clustering_keys, db_schema_t * schema, WORD table_key, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_delete_by_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, long nonce, remote_db_t * db);

// Read ops:

db_row_t* remote_search_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key,
		uuid_t * txnid, long nonce, remote_db_t * db,
		unsigned int * fastrandstate);
db_row_t* remote_search_clustering_in_txn(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys,
														WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce,
														remote_db_t * db, unsigned int * fastrandstate);
db_row_t* remote_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
									WORD* col_keys, int no_columns, WORD table_key,
									uuid_t * txnid, long nonce, remote_db_t * db);
db_row_t* remote_search_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_range_search_in_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
							snode_t** start_row, snode_t** end_row,
							WORD table_key, uuid_t * txnid, long nonce, remote_db_t * db,
							unsigned int * fastrandstate);
int remote_range_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys,
									 WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
									 snode_t** start_row, snode_t** end_row,
									 WORD table_key, uuid_t * txnid, long nonce, remote_db_t * db,
									 unsigned int * fastrandstate);
int remote_range_search_index_in_txn(int idx_idx, WORD start_idx_key, WORD end_idx_key,
								snode_t** start_row, snode_t** end_row,
								WORD table_key, uuid_t * txnid, long nonce, remote_db_t * db);

// Queue ops:

int remote_create_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_delete_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_enqueue_in_txn(WORD * column_values, int no_cols, WORD table_key, WORD queue_id, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_read_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, long * new_read_head,
		snode_t** start_row, snode_t** end_row, uuid_t * txnid, long nonce,
		remote_db_t * db, unsigned int * fastrandstate);
int remote_consume_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					long new_consume_head, uuid_t * txnid, long nonce, remote_db_t * db);
int remote_subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
						long nonce, remote_db_t * db, unsigned int * fastrandstate);
int remote_unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						long nonce, remote_db_t * db);
int remote_subscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
						uuid_t * txnid, long nonce, remote_db_t * db, unsigned int * fastrandstate);
int remote_unsubscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
								uuid_t * txnid, long nonce, remote_db_t * db);

// Subscription handling client-side:

int subscribe_queue_client(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					queue_callback * callback, short use_lock, remote_db_t * db, unsigned int * fastrandstate);
int unsubscribe_queue_client(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						short use_lock, remote_db_t * db);


// Txn mgmt:

uuid_t * remote_new_txn(long nonce, remote_db_t * db, unsigned int * seedptr);
int remote_validate_txn(uuid_t * txnid, vector_clock * version, long nonce, remote_db_t * db);
int remote_abort_txn(uuid_t * txnid, long nonce, remote_db_t * db);
int remote_commit_txn(uuid_t * txnid, vector_clock * version, long nonce, remote_db_t * db);

// Txn state handling client-side:

txn_state * get_client_txn_state(uuid_t * txnid, remote_db_t * db);
uuid_t * new_client_txn(remote_db_t * db, unsigned int * seedptr);
int close_client_txn(uuid_t * txnid, remote_db_t * db);


#endif /* BACKEND_CLIENT_API_H_ */
