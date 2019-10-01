/*
 * db_queries.c
 *
 *      Author: aagapi
 */

#include "db_queries.h"
#include "db_messages.pb-c.h"

write_query * init_write_query(cell * cell, long txnid, long nonce)
{
	write_query * ca = (write_query *) malloc(sizeof(write_query));
	ca->cell = cell;
	ca->txnid = txnid;
	ca->nonce = nonce;
}

write_query * init_write_query_copy(cell * cell, long txnid, long nonce)
{
	write_query * ca = (write_query *) malloc(sizeof(write_query));
	ca->cell = init_cell_copy(cell->table_key, cell->keys, cell->no_keys, cell->columns, cell->no_columns, cell->version);
	ca->txnid = txnid;
	ca->nonce = nonce;
}

void free_write_query(write_query * ca)
{
	free_cell(ca->cell);
	free(ca);
}

int serialize_write_query(write_query * ca, void ** buf, unsigned * len)
{

}

int deserialize_write_query(void * buf, unsigned msg_len, cell ** ca)
{

}

char * to_string_write_query(write_query * ca, char * msg_buff)
{

}





