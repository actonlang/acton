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

write_query * init_write_query(cell * cell, long txnid, long nonce);
void free_write_query(write_query * ca);
int serialize_write_query(write_query * ca, void ** buf, unsigned * len);
int deserialize_write_query(void * buf, unsigned msg_len, cell ** ca);
char * to_string_write_query(write_query * ca, char * msg_buff);

#endif /* BACKEND_FAILURE_DETECTOR_DB_QUERIES_H_ */
