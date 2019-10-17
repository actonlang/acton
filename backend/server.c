/*
 * server.c
 *
 *      Author: aagapi
 */

// Server:

#include "db.h"
#include "failure_detector/db_queries.h"
#include "fastrand.h"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define BUFSIZE (1024 * 1024)

char in_buf[BUFSIZE];
char out_buf[BUFSIZE];

int no_cols = 4;
int no_primary_keys = 1;
int no_clustering_keys = 2;
int no_index_keys = 1;

int no_actors = 2;
int no_collections = 2;
int no_items = 2;

typedef struct db_server
{
	db_t * db;
} db_server;

db_schema_t * get_schema(db_t * db, WORD table_key)
{
	snode_t * node = skiplist_search(db->tables, table_key);

	if(node == NULL)
		return NULL;

	db_table_t * table = (db_table_t *) (node->value);

	return table->schema;
}

int get_ack_packet(int status, write_query * q,
					void ** snd_buf, unsigned * snd_msg_len)
{
	ack_message * ack = init_ack_message(get_cell_address(q->cell), status, q->txnid, q->nonce);
	return serialize_ack_message(ack, snd_buf, snd_msg_len);
}

int handle_write_query(write_query * wq, db_t * db, unsigned int * fastrandstate)
{
	int i=0;
	int total_columns = wq->cell->no_keys + wq->cell->no_columns;
	WORD * column_values = (WORD *) malloc(total_columns * sizeof(WORD));
	for(;i<wq->cell->no_keys;i++)
		column_values[i] = (WORD) wq->cell->keys[i];
	for(;i<total_columns;i++)
		column_values[i] = (WORD) wq->cell->keys[i-wq->cell->no_keys];

	return db_insert(column_values, total_columns, (WORD) wq->cell->table_key, db, fastrandstate);
}

int handle_write_packet(void * rcv_buf, unsigned rcv_msg_len,
						void ** snd_buf, unsigned * snd_msg_len,
						db_t * db, unsigned int * fastrandstate)
{
	write_query * q;
	char print_buff[100];

	int success = deserialize_write_query(rcv_buf, rcv_msg_len, &q);
	if(success != 0)
		return success;

	to_string_write_query(q, (char *) print_buff);
	printf("Received write query: %s\n", print_buff);

	if(q->cell->no_columns <= 0)
		return 2;

	success = handle_write_query(q, db, fastrandstate);
	return get_ack_packet(success, q, snd_buf, snd_msg_len);
}

vector_clock * get_empty_vc()
{
	int node_ids[] = {0};
	long counters[] = {0};
	return init_vc(1, node_ids, counters, 0);
}

int get_read_response_packet(db_row_t* result, read_query * q, db_schema_t * schema, void ** snd_buf, unsigned * snd_msg_len)
{
	int no_keys = schema->no_primary_keys + schema->no_clustering_keys;
	int no_columns = schema->no_cols - no_keys;
	long * keys = (long *) malloc(no_keys);
	long * columns = (long *) malloc(no_columns);
	for(int i = 0;i < no_keys;i++)
		keys[i] = q->cell_address->keys[i];
	for(int i = 0;i < no_columns;i++)
		columns[i] = (long) result->column_array[i];
	vector_clock * vc = get_empty_vc();

	cell * c = init_cell(q->cell_address->table_key, keys, no_keys, columns, no_columns, vc);
	read_response_message * m = init_write_query(c, q->txnid, q->nonce);
	return serialize_write_query(m, snd_buf, snd_msg_len);
}

db_row_t* handle_read_query(read_query * q, db_t * db, db_schema_t ** schema, unsigned int * fastrandstate)
{
	int i=0;

	*schema = get_schema(db, (WORD) q->cell_address->table_key);
	WORD * primary_keys = (WORD *) malloc((*schema)->no_primary_keys * sizeof(WORD));
	int no_clustering_keys = q->cell_address->no_keys - (*schema)->no_primary_keys;
	WORD * clustering_keys = (WORD *) malloc(no_clustering_keys * sizeof(WORD));

	for(;i<(*schema)->no_primary_keys;i++)
		primary_keys[i] = (WORD) q->cell_address->keys[i];
	for(;i<q->cell_address->no_keys;i++)
		clustering_keys[i-(*schema)->no_primary_keys] = (WORD) q->cell_address->keys[i];

	db_row_t* result = db_search_clustering(primary_keys, clustering_keys, no_clustering_keys, (WORD) q->cell_address->table_key, db);

	return result;
}

int handle_read_packet(void * buf, unsigned msg_len, db_t * db,
						void ** snd_buf, unsigned * snd_msg_len,
						unsigned int * fastrandstate)
{
	read_query * q;
	db_schema_t * schema;
	char print_buff[100];

	int success = deserialize_read_query(buf, msg_len, &q);
	if(success != 0)
		return success;

	to_string_read_query(q, (char *) print_buff);
	printf("Received read query: %s\n", print_buff);

	db_row_t* result = handle_read_query(q, db, &schema, fastrandstate);
	return get_read_response_packet(result, q, schema, snd_buf, snd_msg_len);
}

void error(char *msg) {
  perror(msg);
  exit(1);
}

int create_schema(db_t * db, unsigned int * fastrandstate) {
	int primary_key_idx = 0;
	int clustering_key_idxs[2];
	clustering_key_idxs[0]=1;
	clustering_key_idxs[1]=2;
	int index_key_idx=3;

	int * col_types = (int *) malloc(no_cols * sizeof(int));

	for(int i=0;i<no_cols;i++)
		col_types[i] = DB_TYPE_INT32;

	db_schema_t* db_schema = db_create_schema(col_types, no_cols, &primary_key_idx, no_primary_keys, clustering_key_idxs, no_clustering_keys, &index_key_idx, no_index_keys);

	assert(db_schema != NULL && "Schema creation failed");

	// Create table:

	return db_create_table((WORD) 0, db_schema, db, fastrandstate);;
}


int main(int argc, char **argv) {
  int parentfd;
  int childfd;
  int portno;
  int clientlen;
  struct sockaddr_in serveraddr;
  struct sockaddr_in clientaddr;
  struct hostent *hostp;
  char *hostaddrp;
  int optval; /* flag value for setsockopt */
  int msg_len; /* message byte size */
  unsigned snd_msg_len;
  unsigned int seed;
  int ret = 0;

  if (argc != 2) {
    fprintf(stderr, "usage: %s <port>\n", argv[0]);
    exit(1);
  }
  portno = atoi(argv[1]);

  GET_RANDSEED(&seed, 0); // thread_id

  // Get db pointer:
  db_t * db = get_db();

  // Create schema:
  ret = create_schema(db, &seed);
  printf("Test %s - %s\n", "create_schema", ret==0?"OK":"FAILED");

  parentfd = socket(AF_INET, SOCK_STREAM, 0);
  if (parentfd < 0)
    error("ERROR opening socket");

  optval = 1;
  setsockopt(parentfd, SOL_SOCKET, SO_REUSEADDR, (const void *)&optval , sizeof(int));

  bzero((char *) &serveraddr, sizeof(serveraddr));
  serveraddr.sin_family = AF_INET;
  serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
  serveraddr.sin_port = htons((unsigned short)portno);

  if (bind(parentfd, (struct sockaddr *) &serveraddr, sizeof(serveraddr)) < 0)
    error("ERROR on binding");

  if (listen(parentfd, 100) < 0) /* allow 100 requests to queue up */
    error("ERROR on listen");

  clientlen = sizeof(clientaddr);

  childfd = accept(parentfd, (struct sockaddr *) &clientaddr, &clientlen);
  if (childfd < 0)
    error("ERROR on accept");

  hostp = gethostbyaddr((const char *)&clientaddr.sin_addr.s_addr,
			  sizeof(clientaddr.sin_addr.s_addr), AF_INET);
  if (hostp == NULL)
    error("ERROR on gethostbyaddr");
  hostaddrp = inet_ntoa(clientaddr.sin_addr);
  if (hostaddrp == NULL)
    error("ERROR on inet_ntoa\n");
  printf("server established connection with %s (%s)\n", hostp->h_name, hostaddrp);

  while (1)
  {
    bzero(in_buf, BUFSIZE);
    msg_len = -1;
    while(msg_len < 0)
    {
		msg_len = read(childfd, in_buf, BUFSIZE);
		if (msg_len < 0)
		  error("ERROR reading from socket");
    }
    printf("server received %d bytes\n", msg_len);

    void * tmp_out_buf = NULL;
    int status = handle_write_packet((void *) in_buf, msg_len, &tmp_out_buf, &snd_msg_len, db, &seed);

    if(status != 0)
    {
        status = handle_read_packet((void *) in_buf, msg_len, db, &tmp_out_buf, &snd_msg_len, &seed);
    }

    if(status != 0)
    {
    		error("ERROR decoding client request");
    }

    int n = write(childfd, tmp_out_buf, snd_msg_len);
    if (n < 0)
      error("ERROR writing to socket");

    free(tmp_out_buf);
  }
  close(childfd);
}



