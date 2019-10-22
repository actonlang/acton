/*
 * db_queries.c
 *
 *      Author: aagapi
 */

#include "db_queries.h"
#include "db_messages.pb-c.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

// Write Query:

write_query * init_write_query(cell * cell, long txnid, long nonce)
{
	write_query * ca = (write_query *) malloc(sizeof(write_query));
	ca->cell = cell;
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

write_query * init_write_query_copy(cell * cell, long txnid, long nonce)
{
	write_query * ca = (write_query *) malloc(sizeof(write_query));
	ca->cell = init_cell_copy(cell->table_key, cell->keys, cell->no_keys, cell->columns, cell->no_columns, cell->version);
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

write_query * build_write_query(WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, WORD table_key, long txnid, long nonce)
{
	int no_keys = no_primary_keys + no_clustering_keys;
	assert(no_cols - no_keys > 0);
	cell * c = init_cell_copy((long) table_key, (long *) column_values, no_keys, ((long *) column_values + no_keys), no_cols - no_keys, NULL);
	return init_write_query(c, txnid, nonce);
}

void free_write_query(write_query * ca)
{
	free_cell(ca->cell);
	free(ca);
}

void init_write_query_msg(WriteQueryMessage * msg, write_query * ca, VersionedCellMessage * vcell_msg)
{
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->cell = vcell_msg;
}

write_query * init_write_query_from_msg(WriteQueryMessage * msg)
{
	cell * cell = init_cell_from_msg(msg->cell);
	write_query * c = init_write_query_copy(cell, msg->txnid, msg->nonce);
	return c;
}

void free_write_query_msg(WriteQueryMessage * msg)
{
	free_cell_msg(msg->cell);
}

int serialize_write_query(write_query * ca, void ** buf, unsigned * len)
{
	WriteQueryMessage msg = WRITE_QUERY_MESSAGE__INIT;
	VersionedCellMessage vcell_msg = VERSIONED_CELL_MESSAGE__INIT;
	VectorClockMessage vc_msg = VECTOR_CLOCK_MESSAGE__INIT;

	init_cell_msg(&vcell_msg, ca->cell, &vc_msg);
	init_write_query_msg(&msg, ca, &vcell_msg);
	msg.mtype = RPC_TYPE_WRITE;

	*len = write_query_message__get_packed_size (&msg);
	*buf = malloc (*len);
	write_query_message__pack (&msg, *buf);

	free_write_query_msg(&msg);

	return 0;
}

int deserialize_write_query(void * buf, unsigned msg_len, write_query ** ca)
{
	WriteQueryMessage * msg = write_query_message__unpack (NULL, msg_len, buf);

	if (msg == NULL || msg->mtype != RPC_TYPE_WRITE)
	{
		fprintf(stderr, "error unpacking write query message\n");
	    return 1;
	}

	*ca = init_write_query_from_msg(msg);

	write_query_message__free_unpacked(msg, NULL);

	return 0;
}

char * to_string_write_query(write_query * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "WriteQuery(txnid=%ld, nonce=%ld, ", ca->txnid, ca->nonce);
	crt_ptr += strlen(crt_ptr);

	to_string_cell(ca->cell, crt_ptr);
	crt_ptr += strlen(crt_ptr);

	sprintf(crt_ptr, ")");

	return msg_buff;
}

int equals_write_query(write_query * ca1, write_query * ca2)
{
	if(ca1->txnid != ca2->txnid || ca1->nonce != ca2->nonce ||
		!equals_cell(ca1->cell, ca2->cell))
		return 0;

	return 1;
}


// Read Query:

read_query * init_read_query(cell_address * cell_address, long txnid, long nonce)
{
	read_query * ca = (read_query *) malloc(sizeof(read_query));
	ca->cell_address = cell_address;
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

read_query * init_read_query_copy(cell_address * cell_address, long txnid, long nonce)
{
	read_query * ca = (read_query *) malloc(sizeof(read_query));
	ca->cell_address = init_cell_address_copy(cell_address->table_key, cell_address->keys, cell_address->no_keys);
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

read_query * build_read_query(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, long txnid, long nonce)
{
	cell_address * c = init_cell_address_copy2((long) table_key, (long *) primary_keys, no_primary_keys, (long *) clustering_keys, no_clustering_keys);

	return init_read_query(c, txnid, nonce);
}

void free_read_query(read_query * ca)
{
	free_cell_address(ca->cell_address);
	free(ca);
}

void init_read_query_msg(ReadQueryMessage * msg, read_query * ca, CellAddressMessage * cell_address_msg)
{
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->cell_address = cell_address_msg;
}

read_query * init_read_query_from_msg(ReadQueryMessage * msg)
{
	cell_address * cell_address = init_cell_address_from_msg(msg->cell_address);
	read_query * c = init_read_query_copy(cell_address, msg->txnid, msg->nonce);
	return c;
}

void free_read_query_msg(ReadQueryMessage * msg)
{
	free_cell_address_msg(msg->cell_address);
}

int serialize_read_query(read_query * ca, void ** buf, unsigned * len)
{
	ReadQueryMessage msg = READ_QUERY_MESSAGE__INIT;
	CellAddressMessage cell_address_msg = CELL_ADDRESS_MESSAGE__INIT;

	init_cell_address_msg(&cell_address_msg, ca->cell_address);
	init_read_query_msg(&msg, ca, &cell_address_msg);
	msg.mtype = RPC_TYPE_READ;

	*len = read_query_message__get_packed_size (&msg);
	*buf = malloc (*len);
	read_query_message__pack (&msg, *buf);

	free_read_query_msg(&msg);

	return 0;
}

int deserialize_read_query(void * buf, unsigned msg_len, read_query ** ca)
{
	ReadQueryMessage * msg = read_query_message__unpack (NULL, msg_len, buf);

	if (msg == NULL || msg->mtype != RPC_TYPE_READ)
	{
		fprintf(stderr, "error unpacking read query message\n");
	    return 1;
	}

	*ca = init_read_query_from_msg(msg);

	read_query_message__free_unpacked(msg, NULL);

	return 0;
}

char * to_string_read_query(read_query * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "ReadQuery(txnid=%ld, nonce=%ld, ", ca->txnid, ca->nonce);
	crt_ptr += strlen(crt_ptr);

	to_string_cell_address(ca->cell_address, crt_ptr);
	crt_ptr += strlen(crt_ptr);

	sprintf(crt_ptr, ")");

	return msg_buff;
}

int equals_read_query(read_query * ca1, read_query * ca2)
{
	if(ca1->txnid != ca2->txnid || ca1->nonce != ca2->nonce ||
		!equals_cell_address(ca1->cell_address, ca2->cell_address))
		return 0;

	return 1;
}

// Range read query:

range_read_query * init_range_read_query(cell_address * start_cell_address, cell_address * end_cell_address, long txnid, long nonce)
{
	range_read_query * ca = (range_read_query *) malloc(sizeof(range_read_query));
	ca->start_cell_address = start_cell_address;
	ca->end_cell_address = end_cell_address;
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

void free_range_read_query(range_read_query * ca)
{
	free_cell_address(ca->start_cell_address);
	free_cell_address(ca->end_cell_address);
	free(ca);
}

range_read_query * init_range_read_query_copy(cell_address * start_cell_address, cell_address * end_cell_address, long txnid, long nonce)
{
	range_read_query * ca = (range_read_query *) malloc(sizeof(range_read_query));
	ca->start_cell_address = init_cell_address_copy(start_cell_address->table_key, start_cell_address->keys, start_cell_address->no_keys);
	ca->end_cell_address = init_cell_address_copy(end_cell_address->table_key, end_cell_address->keys, end_cell_address->no_keys);
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

void init_range_read_query_msg(RangeReadQueryMessage * msg, range_read_query * ca, CellAddressMessage * start_cell_address_msg, CellAddressMessage * end_cell_address_msg)
{
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->start_cell_address = start_cell_address_msg;
	msg->end_cell_address = end_cell_address_msg;
}

range_read_query * init_range_read_query_from_msg(RangeReadQueryMessage * msg)
{
	cell_address * start_cell_address = init_cell_address_from_msg(msg->start_cell_address);
	cell_address * end_cell_address = init_cell_address_from_msg(msg->end_cell_address);
	range_read_query * c = init_range_read_query_copy(start_cell_address, end_cell_address, msg->txnid, msg->nonce);
	return c;
}

void free_range_read_query_msg(RangeReadQueryMessage * msg)
{
	free_cell_address_msg(msg->start_cell_address);
	free_cell_address_msg(msg->end_cell_address);
}

int serialize_range_read_query(range_read_query * ca, void ** buf, unsigned * len)
{
	RangeReadQueryMessage msg = RANGE_READ_QUERY_MESSAGE__INIT;
	CellAddressMessage start_cell_address_msg = CELL_ADDRESS_MESSAGE__INIT;
	CellAddressMessage end_cell_address_msg = CELL_ADDRESS_MESSAGE__INIT;

	init_cell_address_msg(&start_cell_address_msg, ca->start_cell_address);
	init_cell_address_msg(&end_cell_address_msg, ca->end_cell_address);
	init_range_read_query_msg(&msg, ca, &start_cell_address_msg, &end_cell_address_msg);
	msg.mtype = RPC_TYPE_RANGE_READ;

	*len = range_read_query_message__get_packed_size (&msg);
	*buf = malloc (*len);
	range_read_query_message__pack (&msg, *buf);

	free_range_read_query_msg(&msg);

	return 0;
}

int deserialize_range_read_query(void * buf, unsigned msg_len, range_read_query ** ca)
{
	RangeReadQueryMessage * msg = range_read_query_message__unpack (NULL, msg_len, buf);

	if (msg == NULL || msg->mtype != RPC_TYPE_RANGE_READ)
	{
		fprintf(stderr, "error unpacking range read query message\n");
	    return 1;
	}

	*ca = init_range_read_query_from_msg(msg);

	range_read_query_message__free_unpacked(msg, NULL);

	return 0;
}

char * to_string_range_read_query(range_read_query * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "RangeReadQuery(txnid=%ld, nonce=%ld, start_key=", ca->txnid, ca->nonce);
	crt_ptr += strlen(crt_ptr);

	to_string_cell_address(ca->start_cell_address, crt_ptr);
	crt_ptr += strlen(crt_ptr);

	sprintf(crt_ptr, ", end_key=");
	crt_ptr += strlen(crt_ptr);

	to_string_cell_address(ca->end_cell_address, crt_ptr);
	crt_ptr += strlen(crt_ptr);

	sprintf(crt_ptr, ")");

	return msg_buff;
}

int equals_range_read_query(range_read_query * ca1, range_read_query * ca2)
{
	if(ca1->txnid != ca2->txnid || ca1->nonce != ca2->nonce ||
		!equals_cell_address(ca1->start_cell_address, ca2->start_cell_address) ||
		!equals_cell_address(ca1->end_cell_address, ca2->end_cell_address))
		return 0;

	return 1;
}


// Ack Message:

ack_message * init_ack_message(cell_address * cell_address, int status, long txnid, long nonce)
{
	ack_message * ca = (ack_message *) malloc(sizeof(ack_message));
	ca->cell_address = cell_address;
	ca->status = status;
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

ack_message * init_ack_message_copy(cell_address * cell_address, int status, long txnid, long nonce)
{
	ack_message * ca = (ack_message *) malloc(sizeof(ack_message));
	ca->cell_address = init_cell_address_copy(cell_address->table_key, cell_address->keys, cell_address->no_keys);
	ca->status = status;
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

void free_ack_message(ack_message * ca)
{
	free_cell_address(ca->cell_address);
	free(ca);
}

void init_ack_message_msg(AckMessage * msg, ack_message * ca, CellAddressMessage * cell_address_msg)
{
	msg->status = ca->status;
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->cell_address = cell_address_msg;
}

ack_message * init_ack_message_from_msg(AckMessage * msg)
{
	cell_address * cell_address = init_cell_address_from_msg(msg->cell_address);
	ack_message * c = init_ack_message_copy(cell_address, msg->status, msg->txnid, msg->nonce);
	return c;
}

void free_ack_message_msg(AckMessage * msg)
{
	free_cell_address_msg(msg->cell_address);
}

int serialize_ack_message(ack_message * ca, void ** buf, unsigned * len)
{
	AckMessage msg = ACK_MESSAGE__INIT;
	CellAddressMessage cell_address_msg = CELL_ADDRESS_MESSAGE__INIT;

	init_cell_address_msg(&cell_address_msg, ca->cell_address);
	init_ack_message_msg(&msg, ca, &cell_address_msg);
	msg.mtype = RPC_TYPE_ACK;

	*len = ack_message__get_packed_size (&msg);
	*buf = malloc (*len);
	ack_message__pack (&msg, *buf);

	free_ack_message_msg(&msg);

	return 0;
}

int deserialize_ack_message(void * buf, unsigned msg_len, ack_message ** ca)
{
	AckMessage * msg = ack_message__unpack (NULL, msg_len, buf);
	char print_buff[100];

	if (msg == NULL || msg->mtype != RPC_TYPE_ACK)
	{
		fprintf(stderr, "error unpacking ack query message\n");
	    return 1;
	}

	*ca = init_ack_message_from_msg(msg);

//	to_string_ack_message(*ca, (char *) print_buff);
//	printf("Received ACK message: %s\n", print_buff);

	ack_message__free_unpacked(msg, NULL);

	return 0;
}

char * to_string_ack_message(ack_message * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "AckMessage(status=%d, txnid=%ld, nonce=%ld, ", ca->status, ca->txnid, ca->nonce);
	crt_ptr += strlen(crt_ptr);

	to_string_cell_address(ca->cell_address, crt_ptr);
	crt_ptr += strlen(crt_ptr);

	sprintf(crt_ptr, ")");

	return msg_buff;
}

int equals_ack_message(ack_message * ca1, ack_message * ca2)
{
	if(ca1->status != ca2->status || ca1->txnid != ca2->txnid || ca1->nonce != ca2->nonce ||
		!equals_cell_address(ca1->cell_address, ca2->cell_address))
		return 0;

	return 1;
}

// Range read response:

range_read_response_message * init_range_read_response_message(cell * cells, int no_cells, long txnid, long nonce)
{
	range_read_response_message * ca = (range_read_response_message *) malloc(sizeof(range_read_response_message));
	ca->cells = cells;
	ca->no_cells = no_cells;
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

range_read_response_message * init_range_read_response_message_copy(cell * cells, int no_cells, long txnid, long nonce)
{
	range_read_response_message * ca = (range_read_response_message *) malloc(sizeof(range_read_response_message));
	ca->no_cells = no_cells;
	ca->cells = (cell *) malloc(no_cells * sizeof(cell));
	for(int i=0;i<no_cells;i++)
		copy_cell(ca->cells + i, cells[i].table_key, cells[i].keys, cells[i].no_keys, cells[i].columns, cells[i].no_columns, cells[i].version);
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

void init_range_read_response_message_msg(RangeReadResponseMessage * msg, range_read_response_message * ca)
{
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->n_cells = ca->no_cells;

	msg->cells = (VersionedCellMessage **) malloc(msg->n_cells * sizeof (VersionedCellMessage*));
	VectorClockMessage ** vcs = (VectorClockMessage **) malloc(msg->n_cells * sizeof (VectorClockMessage*));

	for(int i=0;i<ca->no_cells;i++)
	{
		msg->cells[i] = malloc (sizeof (VersionedCellMessage));
	    versioned_cell_message__init(msg->cells[i]);
	    vcs[i] = malloc (sizeof (VectorClockMessage));
	    vector_clock_message__init(vcs[i]);
	    init_cell_msg(msg->cells[i], ca->cells+i, vcs[i]);
	}

	free(vcs);
}

range_read_response_message * init_range_read_response_message_from_msg(RangeReadResponseMessage * msg)
{
	cell * cells = (cell *) malloc(msg->n_cells * sizeof(cell));
	for(int i=0;i<msg->n_cells;i++)
		copy_cell_from_msg(cells + i, msg->cells[i]);

	range_read_response_message * c = init_range_read_response_message(cells, msg->n_cells, msg->txnid, msg->nonce);
	return c;
}

void free_range_read_response_message_msg(RangeReadResponseMessage * msg)
{
	for(int i=0;i<msg->n_cells;i++)
		free_cell_msg(msg->cells[i]);

	free(msg->cells);
}

void free_range_read_response_message(range_read_response_message * ca)
{
	for(int i=0;i<ca->no_cells;i++)
		free_cell_ptrs(ca->cells + i);
	free(ca->cells);
	free(ca);
}

int serialize_range_read_response_message(range_read_response_message * ca, void ** buf, unsigned * len)
{
	RangeReadResponseMessage msg = RANGE_READ_RESPONSE_MESSAGE__INIT;

	init_range_read_response_message_msg(&msg, ca);
	msg.mtype = RPC_TYPE_RANGE_READ_RESPONSE;

	*len = range_read_response_message__get_packed_size (&msg);
	*buf = malloc (*len);
	range_read_response_message__pack (&msg, *buf);

	free_range_read_response_message_msg(&msg);

	return 0;
}

int deserialize_range_read_response_message(void * buf, unsigned msg_len, range_read_response_message ** ca)
{
	RangeReadResponseMessage * msg = range_read_response_message__unpack (NULL, msg_len, buf);

	if (msg == NULL || msg->mtype != RPC_TYPE_RANGE_READ_RESPONSE)
	{
		fprintf(stderr, "error unpacking range read response message\n");
	    return 1;
	}

	*ca = init_range_read_response_message_from_msg(msg);

	range_read_response_message__free_unpacked(msg, NULL);

	return 0;
}


char * to_string_range_read_response_message(range_read_response_message * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "RangeReadResponseMessage(txnid=%ld, nonce=%ld", ca->txnid, ca->nonce);
	crt_ptr += strlen(crt_ptr);

	sprintf(crt_ptr, ", cells={");
	crt_ptr += strlen(crt_ptr);
	for(int i=0;i<ca->no_cells;i++)
	{
		to_string_cell(ca->cells+i, crt_ptr);
		crt_ptr += strlen(crt_ptr);
		sprintf(crt_ptr, ", ");
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, "} )");

	return msg_buff;
}

int equals_range_read_response_message(range_read_response_message * ca1, range_read_response_message * ca2)
{
	if(ca1->txnid != ca2->txnid || ca1->nonce != ca2->nonce || ca1->no_cells != ca2->no_cells)
		return 0;

	for(int i=0;i<ca1->no_cells;i++)
		if(!equals_cell(ca1->cells+i, ca2->cells+i))
			return 0;

	return 1;
}


// Queue query (and response) messages:

queue_query_message * init_query_message_basic(cell_address * cell_address, long txnid, long nonce)
{
	queue_query_message * ca = (queue_query_message *) malloc(sizeof(queue_query_message));
	ca->cells = NULL;
	ca->no_cells = 0;
	ca->cell_address = cell_address;
	ca->queue_index = -1;
	ca->app_id = -1;
	ca->shard_id = -1;
	ca->consumer_id = -1;
	ca->status = -1;
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

queue_query_message * init_create_queue_message(cell_address * cell_address, long txnid, long nonce)
{
	queue_query_message * ca = init_query_message_basic(cell_address, txnid, nonce);
	ca->msg_type = QUERY_TYPE_CREATE_QUEUE;
	return ca;
}

queue_query_message * init_delete_queue_message(cell_address * cell_address, long txnid, long nonce)
{
	queue_query_message * ca = init_query_message_basic(cell_address, txnid, nonce);
	ca->msg_type = QUERY_TYPE_DELETE_QUEUE;
	return ca;
}

queue_query_message * init_subscribe_queue_message(cell_address * cell_address, int app_id, int shard_id, int consumer_id, long txnid, long nonce)
{
	queue_query_message * ca = init_query_message_basic(cell_address, txnid, nonce);
	ca->msg_type = QUERY_TYPE_SUBSCRIBE_QUEUE;
	ca->app_id = app_id;
	ca->shard_id = shard_id;
	ca->consumer_id = consumer_id;
	return ca;
}

queue_query_message * init_unsubscribe_queue_message(cell_address * cell_address, int app_id, int shard_id, int consumer_id, long txnid, long nonce)
{
	queue_query_message * ca = init_query_message_basic(cell_address, txnid, nonce);
	ca->msg_type = QUERY_TYPE_UNSUBSCRIBE_QUEUE;
	ca->app_id = app_id;
	ca->shard_id = shard_id;
	ca->consumer_id = consumer_id;
	return ca;
}

queue_query_message * init_enqueue_message(cell_address * cell_address, cell * cells, int no_cells, long txnid, long nonce)
{
	queue_query_message * ca = init_query_message_basic(cell_address, txnid, nonce);
	ca->msg_type = QUERY_TYPE_ENQUEUE;
	ca->cells = cells;
	ca->no_cells = no_cells;
	return ca;
}

queue_query_message * init_read_queue_message(cell_address * cell_address, int app_id, int shard_id, int consumer_id, long max_entries, long txnid, long nonce)
{
	queue_query_message * ca = init_query_message_basic(cell_address, txnid, nonce);
	ca->msg_type = QUERY_TYPE_READ_QUEUE;
	ca->queue_index = max_entries;
	ca->app_id = app_id;
	ca->shard_id = shard_id;
	ca->consumer_id = consumer_id;
	return ca;
}

queue_query_message * init_consume_queue_message(cell_address * cell_address, int app_id, int shard_id, int consumer_id, long new_consume_head, long txnid, long nonce)
{
	queue_query_message * ca = init_query_message_basic(cell_address, txnid, nonce);
	ca->msg_type = QUERY_TYPE_CONSUME_QUEUE;
	ca->queue_index = new_consume_head;
	ca->app_id = app_id;
	ca->shard_id = shard_id;
	ca->consumer_id = consumer_id;
	return ca;
}


queue_query_message * init_read_queue_response(cell_address * cell_address, cell * cells, int no_cells, int app_id, int shard_id, int consumer_id, long new_read_head, short status, long txnid, long nonce)
{
	queue_query_message * ca = init_query_message_basic(cell_address, txnid, nonce);
	ca->msg_type = QUERY_TYPE_READ_QUEUE_RESPONSE;
	ca->queue_index = new_read_head;
	ca->app_id = app_id;
	ca->shard_id = shard_id;
	ca->consumer_id = consumer_id;
	ca->cells = cells;
	ca->no_cells = no_cells;
	return ca;

}


void free_queue_message(queue_query_message * ca)
{
	for(int i=0;i<ca->no_cells;i++)
		free_cell_ptrs(ca->cells + i);
	free(ca->cells);

	free(ca);
}

void init_queue_message_msg(QueueQueryMessage * msg, queue_query_message * ca, CellAddressMessage * cell_address_msg)
{
	VectorClockMessage ** vcs = NULL;

	msg->msg_type = ca->msg_type;
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->n_cells = ca->no_cells;

	msg->queue_address = cell_address_msg;

	msg->app_id = ca->app_id;
	msg->shard_id = ca->shard_id;
	msg->consumer_id = ca->consumer_id;
	msg->queue_index = ca->queue_index;

	if(ca->no_cells > 0)
	{
		msg->cells = (VersionedCellMessage **) malloc(msg->n_cells * sizeof (VersionedCellMessage*));
		vcs = (VectorClockMessage **) malloc(msg->n_cells * sizeof (VectorClockMessage*));

		for(int i=0;i<ca->no_cells;i++)
		{
			msg->cells[i] = malloc (sizeof (VersionedCellMessage));
			versioned_cell_message__init(msg->cells[i]);
			vcs[i] = malloc (sizeof (VectorClockMessage));
			vector_clock_message__init(vcs[i]);
			init_cell_msg(msg->cells[i], ca->cells+i, vcs[i]);
		}

		free(vcs);
	}
}

queue_query_message * init_queue_message_from_msg(QueueQueryMessage * msg)
{
	cell * cells = NULL;
	cell_address * cell_address = init_cell_address_from_msg(msg->queue_address);

	switch(msg->msg_type)
	{
		case QUERY_TYPE_CREATE_QUEUE:
		{
			return init_create_queue_message(cell_address, msg->txnid, msg->nonce);
		}
		case QUERY_TYPE_DELETE_QUEUE:
		{
			return init_delete_queue_message(cell_address, msg->txnid, msg->nonce);
		}
		case QUERY_TYPE_SUBSCRIBE_QUEUE:
		{
			return init_subscribe_queue_message(cell_address, msg->app_id, msg->shard_id, msg->consumer_id, msg->txnid, msg->nonce);
		}
		case QUERY_TYPE_UNSUBSCRIBE_QUEUE:
		{
			return init_unsubscribe_queue_message(cell_address, msg->app_id, msg->shard_id, msg->consumer_id, msg->txnid, msg->nonce);
		}
		case QUERY_TYPE_ENQUEUE:
		{
			if(msg->n_cells > 0)
			{
				cells = (cell *) malloc(msg->n_cells * sizeof(cell));
				for(int i=0;i<msg->n_cells;i++)
					copy_cell_from_msg(cells + i, msg->cells[i]);
			}

			return init_enqueue_message(cell_address, cells, msg->n_cells, msg->txnid, msg->nonce);
		}
		case QUERY_TYPE_READ_QUEUE:
		{
			return init_read_queue_message(cell_address, msg->app_id, msg->shard_id, msg->consumer_id, msg->queue_index, msg->txnid, msg->nonce);
		}
		case QUERY_TYPE_CONSUME_QUEUE:
		{
			return init_consume_queue_message(cell_address, msg->app_id, msg->shard_id, msg->consumer_id, msg->queue_index, msg->txnid, msg->nonce);
		}
		case QUERY_TYPE_READ_QUEUE_RESPONSE:
		{
			if(msg->n_cells > 0)
			{
				cells = (cell *) malloc(msg->n_cells * sizeof(cell));
				for(int i=0;i<msg->n_cells;i++)
					copy_cell_from_msg(cells + i, msg->cells[i]);
			}

			return init_read_queue_response(cell_address, cells, msg->n_cells, msg->app_id, msg->shard_id, msg->consumer_id, msg->queue_index, msg->status, msg->txnid, msg->nonce);
		}
	}

	return NULL;
}

void free_queue_message_msg(QueueQueryMessage * msg)
{
	for(int i=0;i<msg->n_cells;i++)
		free_cell_msg(msg->cells[i]);

	free(msg->cells);
}


int serialize_queue_message(queue_query_message * ca, void ** buf, unsigned * len)
{
	QueueQueryMessage msg = QUEUE_QUERY_MESSAGE__INIT;
	CellAddressMessage cell_address_msg = CELL_ADDRESS_MESSAGE__INIT;

	init_cell_address_msg(&cell_address_msg, ca->cell_address);
	init_queue_message_msg(&msg, ca, &cell_address_msg);
	msg.mtype = RPC_TYPE_QUEUE;

	*len = queue_query_message__get_packed_size (&msg);
	*buf = malloc (*len);
	queue_query_message__pack (&msg, *buf);

	free_queue_message_msg(&msg);

	return 0;

}

int deserialize_queue_message(void * buf, unsigned msg_len, queue_query_message ** ca)
{
	QueueQueryMessage * msg = queue_query_message__unpack (NULL, msg_len, buf);

	if (msg == NULL || msg->mtype != RPC_TYPE_QUEUE)
	{
		fprintf(stderr, "error unpacking queue query message\n");
	    return 1;
	}

	*ca = init_queue_message_from_msg(msg);

	queue_query_message__free_unpacked(msg, NULL);

	return 0;
}

char * to_string_queue_message(queue_query_message * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	switch(ca->msg_type)
	{
		case QUERY_TYPE_CREATE_QUEUE:
		{
			sprintf(crt_ptr, "CreateQueue(txnid=%ld, nonce=%ld, ", ca->txnid, ca->nonce);
			break;
		}
		case QUERY_TYPE_DELETE_QUEUE:
		{
			sprintf(crt_ptr, "DeleteQueue(txnid=%ld, nonce=%ld, ", ca->txnid, ca->nonce);
			break;
		}
		case QUERY_TYPE_SUBSCRIBE_QUEUE:
		{
			sprintf(crt_ptr, "SubscribeQueue(txnid=%ld, nonce=%ld, app_id=%d, shard_id=%d, consumer_id=%d, ", ca->txnid, ca->nonce, ca->app_id, ca->shard_id, ca->consumer_id);
			break;
		}
		case QUERY_TYPE_UNSUBSCRIBE_QUEUE:
		{
			sprintf(crt_ptr, "UnsubscribeQueue(txnid=%ld, nonce=%ld, app_id=%d, shard_id=%d, consumer_id=%d, ", ca->txnid, ca->nonce, ca->app_id, ca->shard_id, ca->consumer_id);
			break;
		}
		case QUERY_TYPE_ENQUEUE:
		{
			sprintf(crt_ptr, "Enqueue(txnid=%ld, nonce=%ld, no_entries=%d, ", ca->txnid, ca->nonce, ca->no_cells);
			break;
		}
		case QUERY_TYPE_READ_QUEUE:
		{
			sprintf(crt_ptr, "ReadQueue(txnid=%ld, nonce=%ld, app_id=%d, shard_id=%d, consumer_id=%d, max_items=%ld, ", ca->txnid, ca->nonce, ca->app_id, ca->shard_id, ca->consumer_id, ca->queue_index);
			break;
		}
		case QUERY_TYPE_CONSUME_QUEUE:
		{
			sprintf(crt_ptr, "ConsumeQueue(txnid=%ld, nonce=%ld, app_id=%d, shard_id=%d, consumer_id=%d, new_consume_head=%ld, ", ca->txnid, ca->nonce, ca->app_id, ca->shard_id, ca->consumer_id, ca->queue_index);
			break;
		}
		case QUERY_TYPE_READ_QUEUE_RESPONSE:
		{
			sprintf(crt_ptr, "ReadQueueResponse(txnid=%ld, nonce=%ld, app_id=%d, shard_id=%d, consumer_id=%d, no_entries=%d, new_read_head=%ld, ", ca->txnid, ca->nonce, ca->app_id, ca->shard_id, ca->consumer_id, ca->no_cells, ca->queue_index);
			break;
		}
	}
	crt_ptr += strlen(crt_ptr);

	to_string_cell_address(ca->cell_address, crt_ptr);
	crt_ptr += strlen(crt_ptr);

	if(ca->no_cells > 0)
	{
		sprintf(crt_ptr, ", cells={");
		crt_ptr += strlen(crt_ptr);
		for(int i=0;i<ca->no_cells;i++)
		{
			to_string_cell(ca->cells+i, crt_ptr);
			crt_ptr += strlen(crt_ptr);
			sprintf(crt_ptr, ", ");
			crt_ptr += strlen(crt_ptr);
		}

		sprintf(crt_ptr, "} )");
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, ")");

	return msg_buff;
}

int equals_queue_message(queue_query_message * ca1, queue_query_message * ca2)
{
	if(ca1->txnid != ca2->txnid || ca1->nonce != ca2->nonce ||
		ca1->msg_type != ca2->msg_type || ca1->queue_index != ca2->queue_index ||
		ca1->no_cells != ca2->no_cells || !equals_cell_address(ca1->cell_address, ca2->cell_address))
		return 0;

	return 1;
}

// Txn Message:

txn_message * init_txn_message(int type,
		cell * own_read_set, int no_own_read_set,
		cell * own_write_set, int no_own_write_set,
		cell * complete_read_set, int no_complete_read_set,
		cell * complete_write_set, int no_complete_write_set,
		long txnid, long nonce)
{
	txn_message * ca = (txn_message *) malloc(sizeof(txn_message));
	ca->type = type;
	ca->own_read_set = own_read_set;
	ca->no_own_read_set = no_own_read_set;
	ca->own_write_set = own_write_set;
	ca->no_own_write_set = no_own_write_set;
	ca->complete_read_set = complete_read_set;
	ca->no_complete_read_set = no_complete_read_set;
	ca->complete_write_set = complete_write_set;
	ca->no_complete_write_set = no_complete_write_set;
	ca->txnid = txnid;
	ca->nonce = nonce;
	return ca;
}

txn_message * init_txn_message_copy(int type,
		cell * own_read_set, int no_own_read_set,
		cell * own_write_set, int no_own_write_set,
		cell * complete_read_set, int no_complete_read_set,
		cell * complete_write_set, int no_complete_write_set,
		long txnid, long nonce)
{
	txn_message * ca = (txn_message *) malloc(sizeof(txn_message));

	ca->type = type;
	ca->txnid = txnid;
	ca->nonce = nonce;
	ca->no_own_read_set = no_own_read_set;
	ca->no_own_write_set = no_own_write_set;
	ca->no_complete_read_set = no_complete_read_set;
	ca->no_complete_write_set = no_complete_write_set;

	ca->own_read_set = (cell *) malloc (no_own_read_set * sizeof(cell));
	for(int i=0;i<no_own_read_set;i++)
		ca->own_read_set[i] = own_read_set[i];

	ca->own_write_set = (cell *) malloc (no_own_write_set * sizeof(cell));
	for(int i=0;i<no_own_write_set;i++)
		ca->own_write_set[i] = own_write_set[i];

	ca->complete_read_set = (cell *) malloc (no_complete_read_set * sizeof(cell));
	for(int i=0;i<no_complete_read_set;i++)
		ca->complete_read_set[i] = complete_read_set[i];

	ca->complete_write_set = (cell *) malloc (no_complete_write_set * sizeof(cell));
	for(int i=0;i<no_complete_write_set;i++)
		ca->complete_write_set[i] = complete_write_set[i];

	return ca;
}

void free_txn_message(txn_message * ca)
{
	for(int i=0;i<ca->no_own_read_set;i++)
		free_cell_ptrs(ca->own_read_set+i);
	free(ca->own_read_set);

	for(int i=0;i<ca->no_own_write_set;i++)
		free_cell_ptrs(ca->own_write_set+i);
	free(ca->own_write_set);

	for(int i=0;i<ca->no_complete_read_set;i++)
		free_cell_ptrs(ca->complete_read_set+i);
	free(ca->complete_read_set);

	for(int i=0;i<ca->no_complete_write_set;i++)
		free_cell_ptrs(ca->complete_write_set+i);
	free(ca->complete_write_set);

	free(ca);
}

void init_txn_message_msg(TxnMessage * msg, txn_message * ca)
{
	msg->n_own_read_set = ca->no_own_read_set;
	msg->n_own_write_set = ca->no_own_write_set;
	msg->n_complete_read_set = ca->no_complete_read_set;
	msg->n_complete_write_set = ca->no_complete_write_set;

	VersionedCellMessage **own_read_set = (VersionedCellMessage **) malloc(msg->n_own_read_set * sizeof (VersionedCellMessage*));
	VectorClockMessage ** vc_msgs_own_read_set = (VectorClockMessage **) malloc(msg->n_own_read_set * sizeof (VectorClockMessage*));

	for(int i = 0; i < msg->n_own_read_set; i++)
	{
		own_read_set[i] = malloc (sizeof (VersionedCellMessage));
	    versioned_cell_message__init(own_read_set[i]);
	    vc_msgs_own_read_set[i] = malloc (sizeof (VectorClockMessage));
	    vector_clock_message__init(vc_msgs_own_read_set[i]);
	    init_cell_msg(own_read_set[i], ca->own_read_set+i, vc_msgs_own_read_set[i]);
	}
	free(vc_msgs_own_read_set);

	VersionedCellMessage **own_write_set = (VersionedCellMessage **) malloc(msg->n_own_write_set * sizeof (VersionedCellMessage*));
	VectorClockMessage ** vc_msgs_own_write_set = (VectorClockMessage **) malloc(msg->n_own_write_set * sizeof (VectorClockMessage*));

	for(int i = 0; i < msg->n_own_write_set; i++)
	{
		own_write_set[i] = malloc (sizeof (VersionedCellMessage));
	    versioned_cell_message__init(own_write_set[i]);
	    vc_msgs_own_write_set[i] = malloc (sizeof (VectorClockMessage));
	    vector_clock_message__init(vc_msgs_own_write_set[i]);
	    init_cell_msg(own_write_set[i], ca->own_write_set+i, vc_msgs_own_write_set[i]);
	}
	free(vc_msgs_own_write_set);

	VersionedCellMessage **complete_read_set = (VersionedCellMessage **) malloc(msg->n_complete_read_set * sizeof (VersionedCellMessage*));
	VectorClockMessage ** vc_msgs_complete_read_set = (VectorClockMessage **) malloc(msg->n_complete_read_set * sizeof (VectorClockMessage*));

	for(int i = 0; i < msg->n_complete_read_set; i++)
	{
		complete_read_set[i] = malloc (sizeof (VersionedCellMessage));
	    versioned_cell_message__init(complete_read_set[i]);
	    vc_msgs_complete_read_set[i] = malloc (sizeof (VectorClockMessage));
	    vector_clock_message__init(vc_msgs_complete_read_set[i]);
	    init_cell_msg(complete_read_set[i], ca->complete_read_set+i, vc_msgs_complete_read_set[i]);
	}
	free(vc_msgs_complete_read_set);

	VersionedCellMessage **complete_write_set = (VersionedCellMessage **) malloc(msg->n_complete_write_set * sizeof (VersionedCellMessage*));
	VectorClockMessage ** vc_msgs_complete_write_set = (VectorClockMessage **) malloc(msg->n_complete_write_set * sizeof (VectorClockMessage*));

	for(int i = 0; i < msg->n_complete_write_set; i++)
	{
		complete_write_set[i] = malloc (sizeof (VersionedCellMessage));
	    versioned_cell_message__init(complete_write_set[i]);
	    vc_msgs_complete_write_set[i] = malloc (sizeof (VectorClockMessage));
	    vector_clock_message__init(vc_msgs_complete_write_set[i]);
	    init_cell_msg(complete_write_set[i], ca->complete_write_set+i, vc_msgs_complete_write_set[i]);
	}
	free(vc_msgs_complete_write_set);

	msg->own_read_set = own_read_set;
	msg->own_write_set = own_write_set;
	msg->complete_read_set = complete_read_set;
	msg->complete_write_set = complete_write_set;
	msg->type = ca->type;
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
}

txn_message * init_txn_message_from_msg(TxnMessage * msg)
{
	cell * own_read_set = (cell *) malloc(msg->n_own_read_set * sizeof(cell));
	cell * own_write_set = (cell *) malloc(msg->n_own_write_set * sizeof(cell));
	cell * complete_read_set = (cell *) malloc(msg->n_complete_read_set * sizeof(cell));
	cell * complete_write_set = (cell *) malloc(msg->n_complete_write_set * sizeof(cell));

	for(int i=0;i<msg->n_own_read_set;i++)
		copy_cell_from_msg(own_read_set+i, msg->own_read_set[i]);
	for(int i=0;i<msg->n_own_write_set;i++)
		copy_cell_from_msg(own_write_set+i, msg->own_write_set[i]);
	for(int i=0;i<msg->n_complete_read_set;i++)
		copy_cell_from_msg(complete_read_set+i, msg->complete_read_set[i]);
	for(int i=0;i<msg->n_complete_write_set;i++)
		copy_cell_from_msg(complete_write_set+i, msg->complete_write_set[i]);

	txn_message * c = init_txn_message_copy(msg->type,
			own_read_set, msg->n_own_read_set,
			own_write_set, msg->n_own_write_set,
			complete_read_set, msg->n_complete_read_set,
			complete_write_set, msg->n_complete_write_set,
			msg->txnid, msg->nonce);

	return c;
}

void free_txn_message_msg(TxnMessage * msg)
{
	for(int i=0;i<msg->n_own_read_set;i++)
		free_cell_msg(msg->own_read_set[i]);
	free(msg->own_read_set);

	for(int i=0;i<msg->n_own_write_set;i++)
		free_cell_msg(msg->own_write_set[i]);
	free(msg->own_write_set);

	for(int i=0;i<msg->n_complete_read_set;i++)
		free_cell_msg(msg->complete_read_set[i]);
	free(msg->complete_read_set);

	for(int i=0;i<msg->n_complete_write_set;i++)
		free_cell_msg(msg->complete_write_set[i]);
	free(msg->complete_write_set);
}

int serialize_txn_message(txn_message * ca, void ** buf, unsigned * len)
{
	TxnMessage msg = TXN_MESSAGE__INIT;

	init_txn_message_msg(&msg, ca);
	msg.mtype = RPC_TYPE_TXN;

	*len = txn_message__get_packed_size (&msg);
	*buf = malloc (*len);
	txn_message__pack (&msg, *buf);

	free_txn_message_msg(&msg);

	return 0;
}

int deserialize_txn_message(void * buf, unsigned msg_len, txn_message ** ca)
{
	TxnMessage * msg = txn_message__unpack (NULL, msg_len, buf);

	if (msg == NULL || msg->mtype != RPC_TYPE_TXN)
	{
		fprintf(stderr, "error unpacking read query message\n");
	    return 1;
	}

	*ca = init_txn_message_from_msg(msg);

	txn_message__free_unpacked(msg, NULL);

	return 0;
}

char * to_string_txn_message(txn_message * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "TxnMessage(type=%d, txnid=%ld, nonce=%ld", ca->type, ca->txnid, ca->nonce);
	crt_ptr += strlen(crt_ptr);

	sprintf(crt_ptr, ", own_read_set={");
	crt_ptr += strlen(crt_ptr);
	for(int i=0;i<ca->no_own_read_set;i++)
	{
		to_string_cell(ca->own_read_set+i, crt_ptr);
		crt_ptr += strlen(crt_ptr);
		sprintf(crt_ptr, ", ");
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, "}, own_write_set={");
	crt_ptr += strlen(crt_ptr);
	for(int i=0;i<ca->no_own_write_set;i++)
	{
		to_string_cell(ca->own_write_set+i, crt_ptr);
		crt_ptr += strlen(crt_ptr);
		sprintf(crt_ptr, ", ");
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, "}, complete_read_set={");
	crt_ptr += strlen(crt_ptr);
	for(int i=0;i<ca->no_complete_read_set;i++)
	{
		to_string_cell(ca->complete_read_set+i, crt_ptr);
		crt_ptr += strlen(crt_ptr);
		sprintf(crt_ptr, ", ");
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, "}, complete_write_set={");
	crt_ptr += strlen(crt_ptr);
	for(int i=0;i<ca->no_complete_write_set;i++)
	{
		to_string_cell(ca->complete_write_set+i, crt_ptr);
		crt_ptr += strlen(crt_ptr);
		sprintf(crt_ptr, "}, ");
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, ")");

	return msg_buff;
}

int equals_txn_message(txn_message * ca1, txn_message * ca2)
{
	if(ca1->type != ca2->type || ca1->txnid != ca2->txnid || ca1->nonce != ca2->nonce ||
		ca1->no_own_read_set != ca2->no_own_read_set ||
		ca1->no_own_write_set != ca2->no_own_write_set ||
		ca1->no_complete_read_set != ca2->no_complete_read_set ||
		ca1->no_complete_write_set != ca2->no_complete_write_set)
		return 0;

	for(int i=0;i<ca1->no_own_read_set;i++)
		if(!equals_cell(ca1->own_read_set+i, ca2->own_read_set+i))
			return 0;

	for(int i=0;i<ca1->no_own_write_set;i++)
		if(!equals_cell(ca1->own_write_set+i, ca2->own_write_set+i))
			return 0;

	for(int i=0;i<ca1->no_complete_read_set;i++)
		if(!equals_cell(ca1->complete_read_set+i, ca2->complete_read_set+i))
			return 0;

	for(int i=0;i<ca1->no_complete_write_set;i++)
		if(!equals_cell(ca1->complete_write_set+i, ca2->complete_write_set+i))
			return 0;

	return 1;
}




