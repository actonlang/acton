/*
 * cells.c
 *
 *      Author: aagapi
 */

#include "cells.h"
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>


cell_address * init_cell_address(long table_key, long * keys, int no_keys)
{
	cell_address * ca = (cell_address *) malloc(sizeof(cell_address));
	ca->table_key = table_key;
	ca->keys = keys;
	ca->no_keys = no_keys;

	return ca;
}

cell_address * init_cell_address_copy(long table_key, long * keys, int no_keys)
{
	cell_address * ca = (cell_address *) malloc(sizeof(cell_address));
	ca->table_key = table_key;
	ca->no_keys = no_keys;
	ca->keys = (long *) malloc(no_keys * sizeof(long));
	for(int i=0;i<no_keys;i++)
		ca->keys[i] = keys[i];

	return ca;
}

void free_cell_address(cell_address * ca)
{
	free(ca->keys);
	free(ca);
}

void init_cell_address_msg(CellAddressMessage * msg, cell_address * ca)
{
	msg->table_key = ca->table_key;
	msg->n_keys = ca->no_keys;
	msg->keys = (long *) malloc(ca->no_keys * sizeof(long));
	for(int i=0;i<ca->no_keys;i++)
		msg->keys[i] = ca->keys[i];
}

cell_address * init_cell_address_from_msg(CellAddressMessage * msg)
{
	return init_cell_address_copy(msg->table_key, msg->keys, msg->n_keys);
}

void free_cell_address_msg(CellAddressMessage * msg)
{
	free(msg->keys);
}

int serialize_cell_address(cell_address * ca, void ** buf, unsigned * len)
{
	CellAddressMessage msg = CELL_ADDRESS_MESSAGE__INIT;
	init_cell_address_msg(&msg, ca);

	*len = cell_address_message__get_packed_size (&msg);
	*buf = malloc (*len);
	cell_address_message__pack (&msg, *buf);

	free_cell_address_msg(&msg);

	return 0;
}

int deserialize_cell_address(void * buf, unsigned msg_len, cell_address ** ca)
{
	CellAddressMessage * msg = cell_address_message__unpack (NULL, msg_len, buf);

	if (msg == NULL)
	{
		fprintf(stderr, "error unpacking vector_clock message\n");
	    return 1;
	}

	*ca = init_cell_address_from_msg(msg);

	cell_address_message__free_unpacked(msg, NULL);

	return 0;
}

int equals_cell_address(cell_address * ca1, cell_address * ca2)
{
	if(ca1->table_key != ca2->table_key || ca1->no_keys != ca2->no_keys)
		return 0;

	for(int i=0;i<ca1->no_keys;i++)
		if(ca1->keys[i] != ca2->keys[i])
			return 0;

	return 1;
}

char * to_string_cell_address(cell_address * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "CellAddress(table_key=%ld, keys={", ca->table_key);
	crt_ptr += strlen(crt_ptr);

	for(int i=0;i<ca->no_keys;i++)
	{
		sprintf(crt_ptr, "%ld, ", ca->keys);
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, "})");

	return msg_buff;
}


