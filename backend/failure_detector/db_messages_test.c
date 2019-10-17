/*
 * db_messages_test.c
 *
 *      Author: aagapi
 */

#include "vector_clock.h"
#include "fd.h"
#include "cells.h"
#include "db_queries.h"
#include <stdio.h>
#include <assert.h>

#define MAX_MSG_SIZE_VC 1024

int write_msg_to_file (unsigned char *buff, unsigned len, FILE * fp)
{
	unsigned written = fwrite(buff, len, 1, fp);

	printf("Wrote %d / %d bytes to file\n", written * len, len);

	return (written == 1)? 0 : -1;
}

int read_msg_from_file (unsigned max_length, unsigned char *buff, FILE * fp)
{
  unsigned cur_len = 0;
  unsigned nread;

  while ((nread=fread(buff + cur_len, 1, max_length - cur_len, fp)) != 0)
  {
	printf("Read %d bytes\n", nread);
    cur_len += nread;
    if (cur_len == max_length)
    {
      fprintf(stderr, "max message length exceeded\n");
      return -1;
    }
  }
  return cur_len;
}

int main (int argc, const char * argv[])
{
	FILE *fptr_w = NULL, * fptr_r = NULL;
	void * buf_w;
	unsigned len_w;
	unsigned char buf_r[MAX_MSG_SIZE_VC];
	unsigned len_r;
	char err_msg [100];

	// Generate a dummy VC:

	int node_ids[] = {0,1};
	long counters[] = {0,0};

	vector_clock * vc = init_vc(2, node_ids, counters, 1);
	add_component_vc(vc, 2, 0);
	increment_vc(vc, 0);
	increment_vc(vc, 0);
	increment_vc(vc, 1);
	increment_vc(vc, 2);
	increment_vc(vc, 2);

	// Serialize it to file:

	fptr_w = fopen("/tmp/vc.test","wb");

	serialize_vc(vc, &buf_w, &len_w);
	int success = write_msg_to_file(buf_w, len_w, fptr_w);
	assert(success == 0);
	fclose(fptr_w);

	// Read it back:

	fptr_r = fopen("/tmp/vc.test","rb");
	vector_clock * vc_r = NULL;

	len_r = read_msg_from_file(MAX_MSG_SIZE_VC, buf_r, fptr_r);

	if(len_r != len_w)
	{
		printf("len_r=%d != len_w=%d\n", len_r, len_w);
		assert(0);
	}

	deserialize_vc(buf_r, len_r, &vc_r);

	if(compare_vc(vc, vc_r) != 0)
	{
		printf("VC mismatch: %s != %s\n", to_string_vc(vc, err_msg), to_string_vc(vc_r, err_msg));
		assert(0);
	}

	// Generate dummy GS:

	gossip_state * gs = init_gossip_state(0, 0, 0, 0, vc);

	// Serialize it to file:

	fptr_w = fopen("/tmp/vc.test","wb");
	serialize_gs(gs, &buf_w, &len_w);
	success = write_msg_to_file(buf_w, len_w, fptr_w);
	assert(success == 0);
	fclose(fptr_w);

	// Read it back:

	fptr_r = fopen("/tmp/vc.test","rb");
	gossip_state * gs_r = NULL;

	len_r = read_msg_from_file(MAX_MSG_SIZE_VC, buf_r, fptr_r);

	if(len_r != len_w)
	{
		printf("len_r=%d != len_w=%d\n", len_r, len_w);
		assert(0);
	}

	deserialize_gs(buf_r, len_r, &gs_r);

	if(!equals_gs(gs, gs_r))
	{
		printf("GS mismatch: %s != %s\n", to_string_gs(gs, err_msg), to_string_gs(gs_r, err_msg));
		assert(0);
	}

	// Generate dummy Write Query:

	long key = 1;
	long column = 1;

	cell * cell = init_cell(0, &key, 1, &column, 1, vc);
	write_query * wquery = init_write_query(cell, 2, 3);

	// Serialize it to file:

	fptr_w = fopen("/tmp/vc.test","wb");
	serialize_write_query(wquery, &buf_w, &len_w);
	success = write_msg_to_file(buf_w, len_w, fptr_w);
	assert(success == 0);
	fclose(fptr_w);

	// Read it back:

	fptr_r = fopen("/tmp/vc.test","rb");
	write_query * wquery_r = NULL;

	len_r = read_msg_from_file(MAX_MSG_SIZE_VC, buf_r, fptr_r);

	if(len_r != len_w)
	{
		printf("len_r=%d != len_w=%d\n", len_r, len_w);
		assert(0);
	}

	deserialize_write_query(buf_r, len_r, &wquery_r);

	printf("Write query: %s, %s\n", to_string_write_query(wquery, err_msg), to_string_write_query(wquery_r, err_msg));

	if(!equals_write_query(wquery, wquery_r))
	{
		printf("Write query mismatch: %s != %s\n", to_string_write_query(wquery, err_msg), to_string_write_query(wquery_r, err_msg));
		assert(0);
	}
}





