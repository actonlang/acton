/*
 * db_messages_test.c
 *
 *      Author: aagapi
 */

#include "vector_clock.h"
#include "fd.h"
#include <stdio.h>
#include <assert.h>

#define MAX_MSG_SIZE_VC 1024

int write_msg_to_file (unsigned char *buff, unsigned len, FILE * fp)
{
	unsigned written = fwrite(buff,len,1,fp);

	return (written == len)? 0 : -1;
}

unsigned read_msg_from_file (unsigned max_length, unsigned char *buff, FILE * fp)
{
  unsigned cur_len = 0;
  unsigned nread;
  while ((nread=fread(buff + cur_len, 1, max_length - cur_len, fp)) != 0)
  {
    cur_len += nread;
    if (cur_len == max_length)
    {
      fprintf(stderr, "max message length exceeded\n");
      return 0;
    }
  }
  return cur_len;
}

int main (int argc, const char * argv[])
{
	// Generate a dummy VC:

	char err_msg [100];

	int node_ids[] = {0,1};
	long counters[] = {0,0};

	vector_clock * vc = init_vc(2, node_ids, counters, 1);
	add_component(vc, 2, 0);
	increment(vc, 0);
	increment(vc, 0);
	increment(vc, 1);
	increment(vc, 2);
	increment(vc, 2);

	// Serialize it to file:

	FILE *fptr_w = fopen("/tmp/vc.test","wb"); ;
	void * buf_w;
	unsigned len_w;

	serialize_vc(vc, &buf_w, &len_w);
	int success = write_msg_to_file(buf_w, len_w, fptr_w);
	assert(success);

	// Read it back:

	FILE *fptr_r = fopen("/tmp/vc.test","rb"); ;
	unsigned char buf_r[MAX_MSG_SIZE_VC];
	unsigned len_r;
	vector_clock * vc_r = NULL;

	len_r = read_msg_from_file(MAX_MSG_SIZE_VC, buf_r, fptr_r);

	if(len_r != len_w)
		printf("len_r=%d != len_w=%d", len_r, len_w);
//	assert(len_r == len_w);

	deserialize_vc(buf_r, len_r, &vc_r);

	assert(compare(vc, vc_r) == 0);
}





