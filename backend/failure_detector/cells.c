/*
 * cells.c
 *
 *      Author: aagapi
 */

#include "cells.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

// Vector Clock serialization:

int serialize_vc(vector_clock * vc, void ** buf, unsigned * len)
{
	VectorClockMessage msg = VECTOR_CLOCK_MESSAGE__INIT;
	init_vc_msg(&msg, vc);

	*len = vector_clock_message__get_packed_size (&msg);
	*buf = malloc (*len);
	vector_clock_message__pack (&msg, *buf);

	free_vc_msg(&msg);

	return 0;
}

int deserialize_vc(void * buf, unsigned msg_len, vector_clock ** vc)
{
	  VectorClockMessage * msg = vector_clock_message__unpack (NULL, msg_len, buf);

	  if (msg == NULL)
	  { // Something failed
	    fprintf(stderr, "error unpacking vector_clock message\n");
	    return 1;
	  }

	  assert(msg->n_ids == msg->n_counters);

	  *vc = init_vc_from_msg(msg);

	  vector_clock_message__free_unpacked(msg, NULL);

	  return 0;
}


// Cell Address:

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

cell_address * init_cell_address_copy2(long table_key, long * primary_keys, int no_primary_keys, long * clustering_keys, int no_clustering_keys)
{
	int i = 0;
	cell_address * c = (cell_address *) malloc(sizeof(cell_address));
	c->table_key = (long) table_key;
	c->no_keys = no_primary_keys + no_clustering_keys;

	assert(c->no_keys > 0);

	c->keys = (long *) malloc(c->no_keys * sizeof(long));
	for(;i<no_primary_keys;i++)
		c->keys[i] = (long) primary_keys[i];

	for(;i<c->no_keys;i++)
		c->keys[i] = (long) clustering_keys[i-no_primary_keys];

	return c;
}

cell_address * init_cell_address_single_key_copy(long table_key, long key)
{
	cell_address * ca = (cell_address *) malloc(sizeof(cell_address));
	ca->table_key = table_key;
	ca->no_keys = 1;
	ca->keys = (long *) malloc(sizeof(long));
	ca->keys[0] = key;

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
		fprintf(stderr, "error unpacking cell_address message\n");
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
		sprintf(crt_ptr, "%ld, ", ca->keys[i]);
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, "})");

	return msg_buff;
}

// Cell:

cell * init_cell(long table_key, long * keys, int no_keys, long * columns, int no_columns, vector_clock * version)
{
	cell * ca = (cell *) malloc(sizeof(cell));
	ca->table_key = table_key;
	ca->keys = keys;
	ca->columns = columns;
	ca->no_keys = no_keys;
	ca->no_columns = no_columns;
	ca->version = version;

	return ca;
}

void copy_cell(cell * ca, long table_key, long * keys, int no_keys, long * columns, int no_columns, vector_clock * version)
{
	ca->table_key = table_key;

	ca->no_keys = no_keys;
	ca->keys = (long *) malloc(no_keys * sizeof(long));
	for(int i=0;i<no_keys;i++)
		ca->keys[i] = keys[i];

	ca->no_columns = no_columns;
	ca->columns = (long *) malloc(no_columns * sizeof(long));
	for(int i=0;i<no_columns;i++)
		ca->columns[i] = columns[i];

	if(version != NULL)
		ca->version = copy_vc(version);
	else
		ca->version = NULL;
}

cell * init_cell_copy(long table_key, long * keys, int no_keys, long * columns, int no_columns, vector_clock * version)
{
	cell * ca = (cell *) malloc(sizeof(cell));
	copy_cell(ca, table_key, keys, no_keys, columns, no_columns, version);
	return ca;
}

cell_address * get_cell_address(cell * c)
{
	return init_cell_address_copy(c->table_key, c->keys, c->no_keys);
}

void free_cell_ptrs(cell * ca)
{
	free(ca->keys);
	free(ca->columns);
	free_vc(ca->version);
}

void free_cell(cell * ca)
{
	free_cell_ptrs(ca);
	free(ca);
}

void init_cell_msg(VersionedCellMessage * msg, cell * ca, VectorClockMessage * vc_msg)
{
	msg->table_key = ca->table_key;
	msg->n_keys = ca->no_keys;
	msg->keys = (long *) malloc(ca->no_keys * sizeof(long));
	for(int i=0;i<ca->no_keys;i++)
		msg->keys[i] = ca->keys[i];
	msg->n_columns = ca->no_columns;
	msg->columns = (long *) malloc(ca->no_columns * sizeof(long));
	for(int i=0;i<ca->no_columns;i++)
		msg->columns[i] = ca->columns[i];

	if(ca->version != NULL)
	{
		init_vc_msg(vc_msg, ca->version);

//		msg->has_version = 1;
		msg->version = vc_msg;
	}
	else
	{
//		msg->has_version = 0;
	}
}

cell * copy_cell_from_msg(cell * c, VersionedCellMessage * msg)
{

	copy_cell(c, msg->table_key, msg->keys, msg->n_keys, msg->columns, msg->n_columns, init_vc_from_msg(msg->version));
//	if(msg->has_version)
//	c->version = init_vc_from_msg(msg->version);
//	else
//		version = NULL;
	return c;
}

cell * init_cell_from_msg(VersionedCellMessage * msg)
{
	vector_clock * vc = NULL;
	if(msg->version != NULL)
		vc = init_vc_from_msg(msg->version);
	cell * c = init_cell_copy(msg->table_key, msg->keys, msg->n_keys, msg->columns, msg->n_columns, vc);

	return c;
}

void free_cell_msg(VersionedCellMessage * msg)
{
	if(msg->keys != NULL)
		free(msg->keys);
	if(msg->columns != NULL)
		free(msg->columns);
	if(msg->version != NULL)
		free_vc_msg(msg->version);
}

int serialize_cell(cell * ca, void ** buf, unsigned * len)
{
	VersionedCellMessage msg = VERSIONED_CELL_MESSAGE__INIT;
	VectorClockMessage vc_msg = VECTOR_CLOCK_MESSAGE__INIT;

	init_cell_msg(&msg, ca, &vc_msg);

	*len = versioned_cell_message__get_packed_size (&msg);
	*buf = malloc (*len);
	versioned_cell_message__pack (&msg, *buf);

	free_cell_msg(&msg);

	return 0;
}

int deserialize_cell(void * buf, unsigned msg_len, cell ** ca)
{
	VersionedCellMessage * msg = versioned_cell_message__unpack (NULL, msg_len, buf);

	if (msg == NULL)
	{
		fprintf(stderr, "error unpacking cell message\n");
	    return 1;
	}

	*ca = init_cell_from_msg(msg);

	versioned_cell_message__free_unpacked(msg, NULL);

	return 0;
}

int equals_cell(cell * ca1, cell * ca2)
{
	if(ca1->table_key != ca2->table_key || ca1->no_keys != ca2->no_keys)
		return 0;

	for(int i=0;i<ca1->no_keys;i++)
		if(ca1->keys[i] != ca2->keys[i])
			return 0;

	for(int i=0;i<ca1->no_columns;i++)
		if(ca1->columns[i] != ca2->columns[i])
			return 0;

	return 1;
}

char * to_string_cell(cell * ca, char * msg_buff)
{
	char * crt_ptr = msg_buff;

	sprintf(crt_ptr, "Cell(table_key=%ld, keys={", ca->table_key);
	crt_ptr += strlen(crt_ptr);

	for(int i=0;i<ca->no_keys;i++)
	{
		sprintf(crt_ptr, "%ld, ", ca->keys[i]);
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, "}, columns={");
	crt_ptr += strlen(crt_ptr);

	for(int i=0;i<ca->no_columns;i++)
	{
		sprintf(crt_ptr, "%ld, ", ca->columns[i]);
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, "}, version=");
	crt_ptr += strlen(crt_ptr);

	if(ca->version != NULL)
	{
		to_string_vc(ca->version, crt_ptr);
		crt_ptr += strlen(crt_ptr);
	}

	sprintf(crt_ptr, ")");

	return msg_buff;
}


