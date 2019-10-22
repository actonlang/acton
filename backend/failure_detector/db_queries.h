/*
 * db_queries.h
 *
 *      Author: aagapi
 */

#include "cells.h"
#include "../db.h"

#ifndef BACKEND_FAILURE_DETECTOR_DB_QUERIES_H_
#define BACKEND_FAILURE_DETECTOR_DB_QUERIES_H_

#define RPC_TYPE_WRITE 0
#define RPC_TYPE_READ 1
#define RPC_TYPE_RANGE_READ 2
#define RPC_TYPE_ACK 3
#define RPC_TYPE_READ_RESPONSE 4
#define RPC_TYPE_RANGE_READ_RESPONSE 5
#define RPC_TYPE_QUEUE 6
#define RPC_TYPE_TXN 7

#define DB_TXN_BEGIN 0
#define DB_TXN_VALIDATION 1
#define DB_TXN_COMMIT 2
#define DB_TXN_ABORT 3

#define DB_ACK 0
#define DB_NACK 1

typedef struct write_query
{
	cell * cell;
	long txnid;
	long nonce;
} write_query;

typedef write_query read_response_message;

write_query * build_write_query(WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, WORD table_key, long txnid, long nonce);
// int db_insert_in_txn(WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
write_query * init_write_query(cell * cell, long txnid, long nonce);
void free_write_query(write_query * ca);
int serialize_write_query(write_query * ca, void ** buf, unsigned * len);
int deserialize_write_query(void * buf, unsigned msg_len, write_query ** ca);
char * to_string_write_query(write_query * ca, char * msg_buff);
int equals_write_query(write_query * ca1, write_query * ca2);

typedef struct read_query
{
	cell_address * cell_address;
	long txnid;
	long nonce;
} read_query;

read_query * build_read_query(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, long txnid, long nonce);
read_query * init_read_query(cell_address * cell_address, long txnid, long nonce);
void free_read_query(read_query * ca);
int serialize_read_query(read_query * ca, void ** buf, unsigned * len);
int deserialize_read_query(void * buf, unsigned msg_len, read_query ** ca);
char * to_string_read_query(read_query * ca, char * msg_buff);
int equals_read_query(read_query * ca1, read_query * ca2);


typedef struct ack_message
{
	cell_address * cell_address;
	int status;
	long txnid;
	long nonce;
} ack_message;

ack_message * init_ack_message(cell_address * cell_address, int status, long txnid, long nonce);
void free_ack_message(ack_message * ca);
int serialize_ack_message(ack_message * ca, void ** buf, unsigned * len);
int deserialize_ack_message(void * buf, unsigned msg_len, ack_message ** ca);
char * to_string_ack_message(ack_message * ca, char * msg_buff);
int equals_ack_message(ack_message * ca1, ack_message * ca2);


typedef struct range_read_query
{
	cell_address * start_cell_address;
	cell_address * end_cell_address;
	long txnid;
	long nonce;
} range_read_query;

range_read_query * init_range_read_query(cell_address * start_cell_address, cell_address * end_cell_address, long txnid, long nonce);
void free_range_read_query(range_read_query * ca);
int serialize_range_read_query(range_read_query * ca, void ** buf, unsigned * len);
int deserialize_range_read_query(void * buf, unsigned msg_len, range_read_query ** ca);
char * to_string_range_read_query(range_read_query * ca, char * msg_buff);
int equals_range_read_query(range_read_query * ca1, range_read_query * ca2);


typedef struct range_read_response_message
{
	cell * cells;
	int no_cells;
	long txnid;
	long nonce;
} range_read_response_message;

range_read_response_message * init_range_read_response_message(cell * cells, int no_cells, long txnid, long nonce);
void free_range_read_response_message(range_read_response_message * ca);
int serialize_range_read_response_message(range_read_response_message * ca, void ** buf, unsigned * len);
int deserialize_range_read_response_message(void * buf, unsigned msg_len, range_read_response_message ** ca);
char * to_string_range_read_response_message(range_read_response_message * ca, char * msg_buff);
int equals_range_read_response_message(range_read_response_message * ca1, range_read_response_message * ca2);


typedef struct queue_query_message
{
	cell_address * cell_address; // queue address
	short msg_type; // {CREATE, DELETE, SUBSCRIBE, UNSUBSCRIBE, ENQUEUE, READ_QUEUE, CONSUME_QUEUE, READ_QUEUE_RESPONSE}

	int app_id;
	int shard_id;
	int consumer_id;

	// For ENQUEUE:

	cell * cells;
	int no_cells;

	// For READ_QUEUE (== max_entries) and CONSUME_QUEUE (== new_consume_head):

	long queue_index;


	// For RESPONSE type messages:

	short status;

	long txnid;
	long nonce;
} queue_query_message;

queue_query_message * init_create_queue_message(cell_address * cell_address, long txnid, long nonce);
queue_query_message * init_delete_queue_message(cell_address * cell_address, long txnid, long nonce);
queue_query_message * init_subscribe_queue_message(cell_address * cell_address, int app_id, int shard_id, int consumer_id, long txnid, long nonce);
queue_query_message * init_unsubscribe_queue_message(cell_address * cell_address, int app_id, int shard_id, int consumer_id, long txnid, long nonce);
queue_query_message * init_enqueue_message(cell_address * cell_address, cell * cells, int no_cells, long txnid, long nonce);
queue_query_message * init_read_queue_message(cell_address * cell_address, int app_id, int shard_id, int consumer_id, long max_entries, long txnid, long nonce);
queue_query_message * init_consume_queue_message(cell_address * cell_address, int app_id, int shard_id, int consumer_id, long new_consume_head, long txnid, long nonce);
queue_query_message * init_read_queue_response(cell_address * cell_address, cell * cells, int no_cells, int app_id, int shard_id, int consumer_id, long new_read_head, short status, long txnid, long nonce);
void free_queue_message(queue_query_message * ca);
int serialize_queue_message(queue_query_message * ca, void ** buf, unsigned * len);
int deserialize_queue_message(void * buf, unsigned msg_len, queue_query_message ** ca);
char * to_string_queue_message(queue_query_message * ca, char * msg_buff);
int equals_queue_message(queue_query_message * ca1, queue_query_message * ca2);

typedef struct txn_message
{
	int type;
	cell * own_read_set;
	int no_own_read_set;
	cell * own_write_set;
	int no_own_write_set;
	cell * complete_read_set;
	int no_complete_read_set;
	cell * complete_write_set;
	int no_complete_write_set;
	long txnid;
	long nonce;
} txn_message;

txn_message * init_txn_message(int type,
								cell * own_read_set, int no_own_read_set,
								cell * own_write_set, int no_own_write_set,
								cell * complete_read_set, int no_complete_read_set,
								cell * complete_write_set, int no_complete_write_set,
								long txnid, long nonce);
void free_txn_message(txn_message * ca);
int serialize_txn_message(txn_message * ca, void ** buf, unsigned * len);
int deserialize_txn_message(void * buf, unsigned msg_len, txn_message ** ca);
char * to_string_txn_message(txn_message * ca, char * msg_buff);
int equals_txn_message(txn_message * ca1, txn_message * ca2);

#endif /* BACKEND_FAILURE_DETECTOR_DB_QUERIES_H_ */
