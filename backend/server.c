/*
 * server.c
 *
 *      Author: aagapi
 */

// Server:

#include "db.h"
#include "failure_detector/db_queries.h"
#include "comm.h"
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

#if (VERBOSE_RPC > 0)
	char print_buff[1024];
	to_string_ack_message(ack, (char *) print_buff);
	printf("Sending ack message: %s\n", print_buff);
#endif

	return serialize_ack_message(ack, snd_buf, snd_msg_len);
}

int handle_write_query(write_query * wq, db_t * db, unsigned int * fastrandstate)
{
	int i=0;
	int total_columns = wq->cell->no_keys + wq->cell->no_columns;

	db_schema_t * schema = get_schema(db, (WORD) wq->cell->table_key);

	switch(wq->msg_type)
	{
		case RPC_TYPE_WRITE:
		{
			assert(wq->cell->no_columns > 0);

			WORD * column_values = (WORD *) malloc(total_columns * sizeof(WORD));
			for(;i<wq->cell->no_keys;i++)
				column_values[i] = (WORD) wq->cell->keys[i];
			for(;i<total_columns;i++)
				column_values[i] = (WORD) wq->cell->columns[i-wq->cell->no_keys];

			if(wq->txnid == NULL) // Write out of txn
				return db_insert_transactional(column_values, total_columns, wq->cell->version, (WORD) wq->cell->table_key, db, fastrandstate);
			else // Write in txn
				return db_insert_in_txn(column_values, total_columns, schema->no_primary_keys, schema->no_clustering_keys, (WORD) wq->cell->table_key, wq->txnid, db, fastrandstate);
		}
		case RPC_TYPE_DELETE:
		{
			if(wq->txnid == NULL) // Delete out of txn
			{
				if(wq->cell->no_keys == schema->no_primary_keys)
					return db_delete_row_transactional((WORD *) wq->cell->keys, wq->cell->version, (WORD) wq->cell->table_key, db, fastrandstate);
				else
					assert(0); // db_delete_cell not implemented yet
			}
			else
			{
				if(wq->cell->no_keys == schema->no_primary_keys)
					return db_delete_row_in_txn((WORD *) wq->cell->keys, wq->cell->no_keys, (WORD) wq->cell->table_key, wq->txnid, db, fastrandstate);
				else
					return db_delete_cell_in_txn((WORD *) wq->cell->keys, schema->no_primary_keys, schema->no_clustering_keys, (WORD) wq->cell->table_key, wq->txnid, db, fastrandstate);
				// TO DO: To support db_delete_by_index_in_txn (in RPCs and backend)
			}
		}
	}

	return 1;
}

vector_clock * get_empty_vc()
{
	int node_ids[] = {0};
	long counters[] = {0};
	return init_vc(1, node_ids, counters, 0);
}

int count_cells(db_row_t* result)
{
	if(result == NULL)
		return 0;

	if(result->cells == NULL || result->cells->no_items == 0)
		return 1;

	int no_cells = 0;
	for(snode_t * crt_cell = HEAD(result->cells); crt_cell != NULL; crt_cell = NEXT(crt_cell))
	{
		db_row_t * child = (db_row_t*) crt_cell->value;
		no_cells += count_cells(child);
	}

	return no_cells;
}

cell * serialize_cells(db_row_t* result, cell * cells, long table_key, long * key_path, int depth, int no_schema_keys)
{
	if(result == NULL)
		return cells;

	key_path[depth-1] = (long) result->key;

	if(result->cells == NULL || result->cells->no_items == 0)
	{
		assert(result->no_columns > 0);
		assert(depth==no_schema_keys);
		copy_cell(cells, table_key,
//					q->cell_address->keys + first_key_index, no_cell_keys,
					key_path, depth,
					(long *) result->column_array, result->no_columns,
					result->version);
		return cells + 1;
	}

	cell * cells_ptr = cells;
	for(snode_t * crt_cell = HEAD(result->cells); crt_cell != NULL; crt_cell = NEXT(crt_cell))
	{
		db_row_t * child = (db_row_t*) crt_cell->value;
		cells_ptr = serialize_cells(child, cells_ptr, table_key, key_path, depth+1, no_schema_keys);
	}

	return cells_ptr;
}

int get_read_response_packet(db_row_t* result, read_query * q, db_schema_t * schema, void ** snd_buf, unsigned * snd_msg_len)
{
	int schema_keys = schema->no_primary_keys + schema->no_clustering_keys;
	int no_keys = schema_keys - q->cell_address->no_keys + 1;
	int no_columns = schema->no_cols - schema_keys;

	if(result == NULL)
	{
		read_response_message * m = init_write_query(NULL, RPC_TYPE_WRITE, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
		char print_buff[1024];
		to_string_write_query(m, (char *) print_buff);
		printf("Sending (NULL) read response message (write query): %s\n", print_buff);
#endif

		return serialize_write_query(m, snd_buf, snd_msg_len);
	}
	else if(result->cells == NULL || result->cells->no_items == 0)
	// Return a single cell read result
	{
		assert(result->no_columns > 0);
		assert(q->cell_address->keys[q->cell_address->no_keys - 1] == (long) result->key);
		cell * c = init_cell(q->cell_address->table_key,
							(long *) &result->key, 1, // Result cell always points to last (inner-most) key of the query
//							q->cell_address->keys + q->cell_address->no_keys - 1, 1,
							(long *) result->column_array, no_columns,
							result->version);

		read_response_message * m = init_write_query(c, RPC_TYPE_WRITE, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
		char print_buff[1024];
		to_string_write_query(m, (char *) print_buff);
		printf("Sending read response message (write query): %s\n", print_buff);
#endif

		return serialize_write_query(m, snd_buf, snd_msg_len);
	}
	else
	// Return a multi-cell read result; traverse db_row downwards and get all child cells recursively:
	{
		int no_results = count_cells(result);

		cell * cells = malloc(no_results * sizeof(cell));

		long * key_path = (long *) malloc(no_keys * sizeof(long));

		cell * last_cell_ptr = serialize_cells(result, cells, q->cell_address->table_key, key_path, 1, schema_keys);

		assert(last_cell_ptr - cells == no_results);

		range_read_response_message * m = init_range_read_response_message(cells, no_results, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
		char print_buff[1024];
		to_string_range_read_response_message(m, (char *) print_buff);
		printf("Sending range read response message: %s\n", print_buff);
#endif

		return serialize_range_read_response_message(m, snd_buf, snd_msg_len);

/*
		long keys = (long *) malloc(no_keys);
		long columns = (long *) malloc(no_columns);
		for(int i = 0; i < no_keys; i++)
			keys[i] = q->cell_address->keys[i + q->cell_address->no_keys - 1];
		for(int i = 0; i < no_columns; i++)
			columns[i] = (long) result->column_array[i];

		cell * c = init_cell(q->cell_address->table_key, keys, no_keys, columns, no_columns, result->version);
*/
	}
}

db_row_t* handle_read_query(read_query * q, db_schema_t ** schema, db_t * db, unsigned int * fastrandstate)
{
	int i=0;

	*schema = get_schema(db, (WORD) q->cell_address->table_key);
	int no_clustering_keys = q->cell_address->no_keys - (*schema)->no_primary_keys;

	if(no_clustering_keys == 0)
	{
		return db_search((WORD *) q->cell_address->keys, (WORD) q->cell_address->table_key, db);
	}
	else
	{
		return db_search_clustering((WORD *) q->cell_address->keys,
									(WORD *) (q->cell_address->keys + (*schema)->no_primary_keys),
									no_clustering_keys, (WORD) q->cell_address->table_key, db);
	}
}

int get_range_read_response_packet(snode_t* start_row, snode_t* end_row, int no_results, range_read_query * q, db_schema_t * schema, void ** snd_buf, unsigned * snd_msg_len)
{
//	int no_keys = schema->no_primary_keys + schema->no_clustering_keys;
//	int no_columns = schema->no_cols - no_keys;

	int schema_keys = schema->no_primary_keys + schema->no_clustering_keys;
	int no_keys = schema_keys - q->start_cell_address->no_keys + 1;
	int no_columns = schema->no_cols - schema_keys;
	range_read_response_message * m = NULL;

	if(no_results == 0)
	{
		m = init_range_read_response_message(NULL, 0, q->txnid, q->nonce);
	}
	else
	{
		assert(start_row != NULL);

		int no_cells = 0, i=0;
		for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
		{
			db_row_t* result = (db_row_t* ) crt_row->value;
			no_cells += count_cells(result);
		}

		cell * cells = malloc(no_results * sizeof(cell));

		long * key_path = (long *) malloc(no_keys * sizeof(long));

		i=0;
		cell * last_cell_ptr = cells;
		for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
		{
			db_row_t* result = (db_row_t* ) crt_row->value;
			last_cell_ptr = serialize_cells(result, last_cell_ptr, q->start_cell_address->table_key, key_path, 1, schema_keys);
		}

		assert(last_cell_ptr - cells == no_cells);

		m = init_range_read_response_message(cells, no_cells, q->txnid, q->nonce);

/*
		cell * cells = malloc(no_results * sizeof(cell));

		int i=0;
		for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
		{
			db_row_t* result = (db_row_t* ) crt_row->value;
			assert(q->start_cell_address->keys[q->start_cell_address->no_keys - 1] <= (long) result->key);
			assert(q->end_cell_address->keys[q->end_cell_address->no_keys - 1] >= (long) result->key);
			copy_cell(cells+i, q->start_cell_address->table_key,
						(long *) &result->key, 1, // Result cell always points to last (inner-most) key of the query
						(long *) result->column_array, no_columns,
						result->version);
		}

		m = init_range_read_response_message(cells, no_results, q->txnid, q->nonce);
*/
	}

#if (VERBOSE_RPC > 0)
		char print_buff[1024];
		to_string_range_read_response_message(m, (char *) print_buff);
		printf("Sending range read response message: %s\n", print_buff);
#endif

		return serialize_range_read_response_message(m, snd_buf, snd_msg_len);
}

int handle_range_read_query(range_read_query * q,
							snode_t** start_row, snode_t** end_row, db_schema_t ** schema,
							db_t * db, unsigned int * fastrandstate)
{
	int i=0;

	assert(q->start_cell_address->table_key == q->end_cell_address->table_key);
	assert(q->start_cell_address->no_keys == q->end_cell_address->no_keys);

	*schema = get_schema(db, (WORD) q->start_cell_address->table_key);
	int no_clustering_keys = q->start_cell_address->no_keys - (*schema)->no_primary_keys;

	if(no_clustering_keys == 0)
	{
		return db_range_search((WORD *) q->start_cell_address->keys, (WORD *) q->end_cell_address->keys, start_row, end_row, (WORD) q->start_cell_address->table_key, db);
	}
	else
	{
		return db_range_search_clustering((WORD *) q->start_cell_address->keys,
										(WORD *) (q->start_cell_address->keys + (*schema)->no_primary_keys),
										(WORD *) (q->end_cell_address->keys + (*schema)->no_primary_keys),
										no_clustering_keys, start_row, end_row, (WORD) q->start_cell_address->table_key, db);
	}
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

    void * tmp_out_buf = NULL, * q = NULL;
    short msg_type;
	db_schema_t * schema;

    int status = parse_message(in_buf, msg_len, &q, &msg_type, 1);

    if(status != 0)
    {
    		error("ERROR decoding client request");
    }

    switch(msg_type)
    {
    		case RPC_TYPE_WRITE:
    		{
    			status = handle_write_query((write_query *) q, db, &seed);
    			status = get_ack_packet(status, (write_query *) q, &tmp_out_buf, &snd_msg_len);
    			break;
    		}
    		case RPC_TYPE_READ:
    		{
    			db_row_t* result = handle_read_query((read_query *) q, &schema, db, &seed);
    			status = get_read_response_packet(result, (read_query *) q, schema, &tmp_out_buf, &snd_msg_len);
    			break;
    		}
    		case RPC_TYPE_RANGE_READ:
    		{
    			snode_t * start_row = NULL, * end_row = NULL;
    			int no_results = handle_range_read_query((range_read_query *) q, &start_row, &end_row, &schema, db, &seed);
    			status = get_range_read_response_packet(start_row, end_row, no_results, (range_read_query *) q, schema, &tmp_out_buf, &snd_msg_len);
    			break;
    		}
    		case RPC_TYPE_QUEUE:
    		{
    			break;
    		}
    		case RPC_TYPE_TXN:
    		{
    			break;
    		}
    		case RPC_TYPE_ACK:
    		{
    			assert(0); // S'dn't happen currently
    			break;
    		}
    }

    assert(status == 0);

    int n = write(childfd, tmp_out_buf, snd_msg_len);
    if (n < 0)
      error("ERROR writing to socket");

    free(tmp_out_buf);
  }
  close(childfd);
}



