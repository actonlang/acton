/*
 * db_queries.c
 *
 *      Author: aagapi
 */

#include "db_queries.h"
#include "db_messages.pb-c.h"

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

void free_write_query(write_query * ca)
{
	free_cell(ca->cell);
	free(ca);
}

void init_write_query_msg(WriteQueryMessage * msg, write_query * ca, VersionedCellMessage * vcell_msg)
{
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->cell = *vcell_msg;
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

	*len = write_query_message__get_packed_size (&msg);
	*buf = malloc (*len);
	write_query_message__pack (&msg, *buf);

	free_write_query_msg(&msg);

	return 0;
}

int deserialize_write_query(void * buf, unsigned msg_len, write_query ** ca)
{
	WriteQueryMessage * msg = write_query_message__unpack (NULL, msg_len, buf);

	if (msg == NULL)
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

void free_read_query(read_query * ca)
{
	free_cell_address(ca->cell_address);
	free(ca);
}

void init_read_query_msg(ReadQueryMessage * msg, read_query * ca, CellAddressMessage * cell_address_msg)
{
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->cell_address = *cell_address_msg;
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

	*len = read_query_message__get_packed_size (&msg);
	*buf = malloc (*len);
	read_query_message__pack (&msg, *buf);

	free_read_query_msg(&msg);

	return 0;
}

int deserialize_read_query(void * buf, unsigned msg_len, read_query ** ca)
{
	ReadQueryMessage * msg = read_query_message__unpack (NULL, msg_len, buf);

	if (msg == NULL)
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
	msg->cell_address = *cell_address_msg;
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

	*len = ack_message_message__get_packed_size (&msg);
	*buf = malloc (*len);
	ack_message_message__pack (&msg, *buf);

	free_ack_message_msg(&msg);

	return 0;
}

int deserialize_ack_message(void * buf, unsigned msg_len, ack_message ** ca)
{
	AckMessage * msg = ack_message_message__unpack (NULL, msg_len, buf);

	if (msg == NULL)
	{
		fprintf(stderr, "error unpacking read query message\n");
	    return 1;
	}

	*ca = init_ack_message_from_msg(msg);

	ack_message_message__free_unpacked(msg, NULL);

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

void init_txn_message_msg(TxnMessage * msg, txn_message * ca, CellAddressMessage * cell_address_msg)
{
	VersionedCellMessage **own_read_set = (VersionedCellMessage **) malloc (msg->no_own_read_set * sizeof (VersionedCellMessage*));
	for(int i = 0; i < msg->no_own_read_set; i++)
	{
		own_read_set[i] = malloc (sizeof (VersionedCellMessage));
	    versioned_cell_message__init(own_read_set[i]);
	    init_cell_msg(own_read_set+i, m->membership+i);
	}

	msg->n_membership = m->no_nodes;
	msg->membership = membership_v;


	msg->status = ca->status;
	msg->txnid = ca->txnid;
	msg->nonce = ca->nonce;
	msg->cell_address = *cell_address_msg;
}

txn_message * init_txn_message_from_msg(TxnMessage * msg)
{
	cell_address * cell_address = init_cell_address_from_msg(msg->cell_address);
	txn_message * c = init_txn_message_copy(cell_address, msg->status, msg->txnid, msg->nonce);
	return c;
}

void free_txn_message_msg(TxnMessage * msg)
{
	free_cell_address_msg(msg->cell_address);
}

int serialize_txn_message(txn_message * ca, void ** buf, unsigned * len)
{
	TxnMessage msg = ACK_MESSAGE__INIT;
	CellAddressMessage cell_address_msg = CELL_ADDRESS_MESSAGE__INIT;

	init_cell_address_msg(&cell_address_msg, ca->cell_address);
	init_txn_message_msg(&msg, ca, &cell_address_msg);

	*len = txn_message_message__get_packed_size (&msg);
	*buf = malloc (*len);
	txn_message_message__pack (&msg, *buf);

	free_txn_message_msg(&msg);

	return 0;
}

int deserialize_txn_message(void * buf, unsigned msg_len, txn_message ** ca)
{
	TxnMessage * msg = txn_message_message__unpack (NULL, msg_len, buf);

	if (msg == NULL)
	{
		fprintf(stderr, "error unpacking read query message\n");
	    return 1;
	}

	*ca = init_txn_message_from_msg(msg);

	txn_message_message__free_unpacked(msg, NULL);

	return 0;
}

char * to_string_txn_message(txn_message * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "TxnMessage(status=%d, txnid=%ld, nonce=%ld, ", ca->status, ca->txnid, ca->nonce);
	crt_ptr += strlen(crt_ptr);

	to_string_cell_address(ca->cell_address, crt_ptr);
	crt_ptr += strlen(crt_ptr);

	sprintf(crt_ptr, ")");

	return msg_buff;
}



