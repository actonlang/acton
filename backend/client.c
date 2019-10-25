/*
 * client.c
 *
 *      Author: aagapi
 */


#include "db.h"
#include "failure_detector/db_queries.h"
#include "fastrand.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <uuid/uuid.h>

#define BUFSIZE 4096

#define CLIENT_VERBOSITY 1

long requests=0;
char out_buf[BUFSIZE];
char in_buf[BUFSIZE];

typedef struct actor_collection_item {
	int actor_id;
	int collection_id;
	int item_id;
	int item_value;
} actor_collection_item_t;

int no_cols = 4;
int no_primary_keys = 1;
int no_clustering_keys = 2;
int no_index_keys = 1;

int no_actors = 2;
int no_collections = 2;
int no_items = 2;


/*
 * error - wrapper for perror
 */
void error(char *msg) {
    perror(msg);
    exit(0);
}

db_schema_t * create_schema() {
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

	return db_schema;
}

int send_packet(void * buf, unsigned len, int sockfd)
{
    int n = write(sockfd, buf, len);
    if (n < 0)
    {
    		error("ERROR writing to socket");
    }
    else
    {
#if CLIENT_VERBOSITY > 0
		printf("Wrote %d bytes to socket\n", n);
#endif
    }

    return 0;
}

int send_packet_wait_reply(void * out_buf, unsigned out_len, int sockfd, void * in_buf, unsigned in_buf_size, int * in_len)
{
	int ret = send_packet(out_buf, out_len, sockfd);

	if(ret != 0)
		return ret;

    bzero(in_buf, in_buf_size);
    *in_len = -1;
    while(*in_len < 0)
    {
    		*in_len = read(sockfd, in_buf, BUFSIZE);
		if (*in_len < 0)
			error("ERROR reading from socket");
		else
			printf("Read %d bytes from socket\n", *in_len);
    }

    return 0;
}

// Write ops:

int remote_insert_in_txn(WORD * column_values, int no_cols, WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	write_query * wq = build_insert_in_txn(column_values, no_cols, schema->no_primary_keys, schema->no_clustering_keys, table_key, txnid, nonce);
	void * tmp_out_buf = NULL;
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending write query: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(in_buf, n, &ack);

	return success;
}

int remote_update_in_txn(int * col_idxs, int no_cols, WORD * column_values, WORD table_key, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}

int remote_delete_row_in_txn(WORD * column_values, int no_cols, WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	write_query * wq = build_delete_row_in_txn(column_values, schema->no_primary_keys, table_key, txnid, nonce);
	void * tmp_out_buf = NULL;
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending delete row query: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;

    success = deserialize_ack_message(in_buf, n, &ack);

	return success;
}

int remote_delete_cell_in_txn(WORD * column_values, int no_cols, int no_clustering_keys, db_schema_t * schema, WORD table_key, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	write_query * wq = build_delete_cell_in_txn(column_values, schema->no_primary_keys, no_clustering_keys, table_key, txnid, nonce);
	void * tmp_out_buf = NULL;
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending delete cell query: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;

    success = deserialize_ack_message(in_buf, n, &ack);

	return success;

}

int remote_delete_by_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}


// Read ops:

db_row_t* remote_search_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	read_query * q = build_search_in_txn(primary_keys, no_primary_keys, table_key, txnid, nonce);
	int success = serialize_read_query(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_read_query(q, (char *) print_buff);
	printf("Sending read row query: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    read_response_message * response;
    success = deserialize_write_query(in_buf, n, &response);

	return response;
}

read_response_message* remote_search_clustering_in_txn(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys,
														WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce,
														int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	read_query * q = build_search_clustering_in_txn(primary_keys, schema->no_primary_keys, clustering_keys, schema->no_clustering_keys, table_key, txnid, nonce);
	int success = serialize_read_query(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_read_query(q, (char *) print_buff);
	printf("Sending read cell query: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    read_response_message * response;
    success = deserialize_write_query(in_buf, n, &response);

	return response;
}

db_row_t* remote_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
									WORD* col_keys, int no_columns, WORD table_key,
									uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}

db_row_t* remote_search_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	assert (0); // Not supported; TO DO
	return 0;
}

int remote_range_search_in_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
							snode_t** start_row, snode_t** end_row,
							WORD table_key, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	range_read_query * q = build_range_search_in_txn(start_primary_keys, end_primary_keys, no_primary_keys, table_key, txnid, nonce);
	int success = serialize_range_read_query(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_range_read_query(q, (char *) print_buff);
	printf("Sending range read row query: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    range_read_response_message * response;
    success = deserialize_range_read_response_message(in_buf, n, &response);

    // TO DO: Parse range_read_response_message to row list

	return response;
}

int remote_range_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys,
									 WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
									 snode_t** start_row, snode_t** end_row,
									 WORD table_key, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	range_read_query * q = build_range_search_clustering_in_txn(primary_keys, no_primary_keys,
															start_clustering_keys, end_clustering_keys, no_clustering_keys,
															table_key, txnid, nonce);
	int success = serialize_range_read_query(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_range_read_query(q, (char *) print_buff);
	printf("Sending range read cell query: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    range_read_response_message * response;
    success = deserialize_range_read_response_message(in_buf, n, &response);

    // TO DO: Parse range_read_response_message to row list

	return response;
}

int remote_range_search_index_in_txn(int idx_idx, WORD start_idx_key, WORD end_idx_key,
								snode_t** start_row, snode_t** end_row,
								WORD table_key, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	assert (0); // Not supported; TO DO
	return 0;
}

// Queue ops:

int remote_create_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_create_queue_in_txn(table_key, queue_id, txnid, nonce);
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(in_buf, n, &ack);

	return success;
}

int remote_delete_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_delete_queue_in_txn(table_key, queue_id, txnid, nonce);
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(in_buf, n, &ack);

	return success;
}

int remote_enqueue_in_txn(WORD * column_values, int no_cols, WORD table_key, WORD queue_id, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_enqueue_in_txn(column_values, no_cols, table_key, queue_id, txnid, nonce);
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(in_buf, n, &ack);

	return success;
}

int remote_read_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, long * new_read_head,
		snode_t** start_row, snode_t** end_row, uuid_t * txnid,
		db_t * db, unsigned int * fastrandstate)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_read_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, max_entries, txnid, nonce);
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

	queue_query_message * response;
    success = deserialize_queue_message(in_buf, n, &response);

    assert(response->msg_type == QUERY_TYPE_READ_QUEUE_RESPONSE);

    // TO DO: Parse queue read response message to row list:

	return success;
}

int remote_consume_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					long new_consume_head, uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_consume_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, new_consume_head, txnid, nonce);
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(in_buf, n, &ack);

	return success;
}

int remote_subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
						long nonce, int sockfd, remote_db_t * db, unsigned int * fastrandstate)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_subscribe_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, NULL, nonce); // txnid
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(in_buf, n, &ack);

    if(success == CLIENT_ERR_SUBSCRIPTION_EXISTS)
    		return CLIENT_ERR_SUBSCRIPTION_EXISTS;

    // Add local subscription on client:

    return subscribe_queue_client(consumer_id, shard_id, app_id, table_key, queue_id, callback, 1, db, fastrandstate);
}

int remote_unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_unsubscribe_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, NULL, nonce); // txnid
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(in_buf, n, &ack);

    if(success == CLIENT_ERR_NO_SUBSCRIPTION_EXISTS)
    		return CLIENT_ERR_NO_SUBSCRIPTION_EXISTS;

    // Remove local subscription from client:

    return unsubscribe_queue_client(consumer_id, shard_id, app_id, table_key, queue_id, 1, db);
}

int remote_subscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
						uuid_t * txnid, long nonce, int sockfd, remote_db_t * db, unsigned int * fastrandstate)
{
	assert (0); // Not supported
	return 0;
}

int remote_unsubscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
								uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}

// Txn mgmt:

uuid_t * remote_new_txn(long nonce, int sockfd, remote_db_t * db, unsigned int * seedptr)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;
	int status = -2;

	while(status == -2) // txnid already exists on server
	{
		uuid_t * txnid = new_client_txn(db, seedptr);
		txn_message * q = build_new_txn(txnid, nonce);
		int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len);

	#if CLIENT_VERBOSITY > 0
		char print_buff[1024];
		to_string_txn_message(q, (char *) print_buff);
		printf("Sending new txn: %s\n", print_buff);
	#endif

		// Send packet to server and wait for reply:

		int n = -1;
		success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

		ack_message * ack;
		success = deserialize_ack_message(in_buf, n, &ack);
		status = ack->status;
	}

	assert(status == 0);

	return txnid;
}

int remote_validate_txn(uuid_t * txnid, vector_clock * version, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	txn_message * q = build_validate_txn(txnid, version, nonce);
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending validate txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

	ack_message * ack;
	success = deserialize_ack_message(in_buf, n, &ack);

	assert(success == 0);

	return ack->status;
}

int remote_abort_txn(uuid_t * txnid, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	txn_message * q = build_abort_txn(txnid, nonce);
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending abort txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

	ack_message * ack;
	success = deserialize_ack_message(in_buf, n, &ack);

	assert(success == 0);

	return ack->status;
}

int remote_persist_txn(uuid_t * txnid, vector_clock * version, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	txn_message * q = build_commit_txn(txnid, version, nonce);
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending commit txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, sockfd, (void *) in_buf, BUFSIZE, &n);

	ack_message * ack;
	success = deserialize_ack_message(in_buf, n, &ack);

	assert(success == 0);

	return ack->status;
}


int remote_commit_txn(uuid_t * txnid, vector_clock * version, long nonce, int sockfd, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

#if (CLIENT_VERBOSITY > 0)
	char uuid_str[37];
	uuid_unparse_lower(*txnid, uuid_str);
#endif
#if (CLIENT_VERBOSITY > 1)
	printf("CLIENT: Attempting to validate txn %s\n", uuid_str);
#endif

	int res = remote_validate_txn(txnid, version, nonce, sockfd, db);

#if (CLIENT_VERBOSITY > 1)
	printf("CLIENT: validate txn %s returned %d\n", uuid_str, res);
#endif

	if(res == VAL_STATUS_COMMIT)
	{
		int persist_status = -2;
		while(persist_status != 0)
		{
			persist_status = remote_persist_txn(txnid, version, nonce+1, sockfd, db);

#if (CLIENT_VERBOSITY > 0)
			printf("CLIENT: persist txn %s returned %d\n", uuid_str, persist_status);
#endif
		}

		res = close_client_txn(txnid, db); // Clear local cached txn state on client

#if (CLIENT_VERBOSITY > 1)
		printf("CLIENT: close txn %s returned %d\n", uuid_str, res);
#endif
	}
	else if(res == VAL_STATUS_ABORT)
	{
		res = remote_abort_txn(txnid, nonce+1, sockfd, db);

#if (CLIENT_VERBOSITY > 0)
		printf("CLIENT: abort txn %s returned %d\n", uuid_str, res);
#endif
	}
	else
	{
		assert(0);
	}

	return 0;
}





// Tests:

int populate_db(db_schema_t * schema, int sockfd, unsigned int * fastrandstate)
{
	uuid_t txnid;
	uuid_generate(txnid);
	WORD * column_values = (WORD *) malloc(no_cols * sizeof(WORD));

	for(long aid=0;aid<no_actors;aid++)
	{
		for(long cid=0;cid<no_collections;cid++)
		{
			for(long iid=0;iid<no_items;iid++)
			{
				column_values[0] = (WORD) aid;
				column_values[1] = (WORD) cid;
				column_values[2] = (WORD) iid;
				column_values[3] = (WORD) iid + 1;

				if(remote_insert_in_txn(column_values, no_cols, (WORD) 0, schema, NULL, requests++, sockfd) != 0) // &txnid
					return -1;
			}
		}
	}

	return 0;
}

int delete_test(db_schema_t * schema, int sockfd, unsigned int * fastrandstate)
// Deletes row for last actor
{
	uuid_t txnid;
	uuid_generate(txnid);
	WORD row_key = (WORD) no_actors - 1;
	return remote_delete_row_in_txn(&row_key, 1, (WORD) 0, schema, NULL, requests++, sockfd); //  &txnid
}

int test_search_pk_ck1_ck2(db_schema_t * schema, int sockfd, unsigned int * fastrandstate)
{
	char print_buff[1024];
	WORD * cks = (WORD *) malloc(2 * sizeof(WORD));

	uuid_t txnid;
	uuid_generate(txnid);

	for(long aid=0;aid<no_actors;aid++)
	{
		for(long cid=0;cid<no_collections;cid++)
		{
			for(long iid=0;iid<no_items;iid++)
			{
				cks[0] = (WORD) cid;
				cks[1] = (WORD) iid;

				read_response_message * response = remote_search_clustering_in_txn((WORD *) &aid, cks, 2, (WORD) 0, schema, &txnid, requests++, sockfd);
				to_string_write_query(response, (char *) print_buff);
				printf("Got back response: %s\n", print_buff);

//				if((long) row->key != cid)
//				{
//					printf("Read back mismatched ck1 %ld ( != %ld) in cell (%ld, %ld)!\n", (long) row->key, cid, aid, cid);
//					return -1;
//				}
			}
		}
	}

	return 0;
}


int main(int argc, char **argv) {
    int sockfd, portno, n;
    struct sockaddr_in serveraddr;
    struct hostent *server;
    char *hostname;
    char buf[BUFSIZE];
    unsigned int seed;

    GET_RANDSEED(&seed, 0); // thread_id

    /* check command line arguments */
    if (argc != 3) {
       fprintf(stderr,"usage: %s <hostname> <port>\n", argv[0]);
       exit(0);
    }
    hostname = argv[1];
    portno = atoi(argv[2]);

    db_schema_t * schema = create_schema();

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0)
        error("ERROR opening socket");

    server = gethostbyname(hostname);
    if (server == NULL) {
        fprintf(stderr,"ERROR, no such host as %s\n", hostname);
        exit(0);
    }

    bzero((void *) &serveraddr, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    bcopy((char *)server->h_addr,
	  (char *)&serveraddr.sin_addr.s_addr, server->h_length);
    serveraddr.sin_port = htons(portno);

    if (connect(sockfd, (struct sockaddr *) &serveraddr, sizeof(serveraddr)) < 0)
      error("ERROR connecting");

    bzero(out_buf, BUFSIZE);

    populate_db(schema, sockfd, &seed);

    test_search_pk_ck1_ck2(schema, sockfd, &seed);

    delete_test(schema, sockfd, &seed);

    close(sockfd);
    return 0;
}


