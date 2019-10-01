/*
 * db_queries.h
 *
 *      Author: aagapi
 */

#include "cells.h"

#ifndef BACKEND_FAILURE_DETECTOR_DB_QUERIES_H_
#define BACKEND_FAILURE_DETECTOR_DB_QUERIES_H_

typedef struct write_query
{
	cell * cell;
	long txnid;
	long nonce;
} write_query;

typedef write_query read_response_query;

write_query * init_write_query(cell * cell, long txnid, long nonce);
void free_write_query(write_query * ca);
int serialize_write_query(write_query * ca, void ** buf, unsigned * len);
int deserialize_write_query(void * buf, unsigned msg_len, cell ** ca);
char * to_string_write_query(write_query * ca, char * msg_buff);

typedef struct read_query
{
	cell_address * cell_address;
	long txnid;
	long nonce;
} read_query;

read_query * init_read_query(cell * cell_address, long txnid, long nonce);
void free_read_query(read_query * ca);
int serialize_read_query(read_query * ca, void ** buf, unsigned * len);
int deserialize_read_query(void * buf, unsigned msg_len, cell ** ca);
char * to_string_read_query(read_query * ca, char * msg_buff);

#define DB_ACK 0
#define DB_NACK 1

typedef struct ack_message
{
	cell_address * cell_address;
	int status;
	long txnid;
	long nonce;
} ack_message;

ack_message * init_ack_message(cell * cell_address, long txnid, long nonce);
void free_ack_message(ack_message * ca);
int serialize_ack_message(ack_message * ca, void ** buf, unsigned * len);
int deserialize_ack_message(void * buf, unsigned msg_len, cell ** ca);
char * to_string_ack_message(ack_message * ca, char * msg_buff);

#define DB_TXN_BEGIN 0
#define DB_TXN_VALIDATION 1
#define DB_TXN_COMMIT 2
#define DB_TXN_ABORT 3

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
int deserialize_txn_message(void * buf, unsigned msg_len, cell ** ca);
char * to_string_txn_message(txn_message * ca, char * msg_buff);

#endif /* BACKEND_FAILURE_DETECTOR_DB_QUERIES_H_ */
