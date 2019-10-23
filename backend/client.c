/*
 * client.c
 *
 *      Author: aagapi
 */


#include "db.h"
#include "failure_detector/db_queries.h"
#include "fastrand.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <uuid/uuid.h>

#define BUFSIZE 4096

long requests=0;
char out_buf[BUFSIZE];
char in_buf[BUFSIZE];

typedef struct remote_db {
    int db_id;
} remote_db_t;

typedef struct actor_collection_item {
	int actor_id;
	int collection_id;
	int item_id;
	int item_value;
} actor_collection_item_t;

int no_cols = 4;
int no_primary_keys = 1;
int no_clustering_keys = 2;
int no_index_keys = 1;

int no_actors = 2;
int no_collections = 2;
int no_items = 2;


/*
 * error - wrapper for perror
 */
void error(char *msg) {
    perror(msg);
    exit(0);
}

db_schema_t * create_schema() {
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

	return db_schema;
}

int db_remote_insert(WORD * column_values, int no_cols, WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce, int sockfd)
{
	unsigned len = 0;
	write_query * wq = build_insert_in_txn(column_values, no_cols, schema->no_primary_keys, schema->no_clustering_keys, table_key, txnid, nonce);
	void * tmp_out_buf = NULL;
	char print_buff[1024];
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len);

	to_string_write_query(wq, (char *) print_buff);
	printf("Sending write query: %s\n", print_buff);

	// Send packet to server and wait for reply:

    int n = write(sockfd, tmp_out_buf, len);
    if (n < 0)
    		error("ERROR writing to socket");
    else
		printf("Wrote %d bytes to socket\n", n);

    bzero(in_buf, BUFSIZE);
    n = -1;
    while(n < 0)
    {
		n = read(sockfd, in_buf, BUFSIZE);
		if (n < 0)
			error("ERROR reading from socket");
		else
			printf("Read %d bytes from socket\n", n);
    }

    ack_message * ack;

    success = deserialize_ack_message(in_buf, n, &ack);

	return success;
}

int db_remote_delete_row(WORD * column_values, int no_cols, WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce, int sockfd)
{
	unsigned len = 0;
	write_query * wq = build_delete_row_in_txn(column_values, schema->no_primary_keys, table_key, txnid, nonce);
	void * tmp_out_buf = NULL;
	char print_buff[1024];
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len);

	to_string_write_query(wq, (char *) print_buff);
	printf("Sending delete row query: %s\n", print_buff);

	// Send packet to server and wait for reply:

    int n = write(sockfd, tmp_out_buf, len);
    if (n < 0)
    		error("ERROR writing to socket");
    else
		printf("Wrote %d bytes to socket\n", n);

    bzero(in_buf, BUFSIZE);
    n = -1;
    while(n < 0)
    {
		n = read(sockfd, in_buf, BUFSIZE);
		if (n < 0)
			error("ERROR reading from socket");
		else
			printf("Read %d bytes from socket\n", n);
    }

    ack_message * ack;

    success = deserialize_ack_message(in_buf, n, &ack);

	return success;
}

read_response_message* db_remote_search_clustering(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, db_schema_t * schema, uuid_t * txnid, long nonce, int sockfd)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;
	char print_buff[1024];

	read_query * q = build_search_clustering_in_txn(primary_keys, schema->no_primary_keys, clustering_keys, schema->no_clustering_keys, table_key, txnid, nonce);
	int success = serialize_read_query(q, (void **) &tmp_out_buf, &len);

	to_string_read_query(q, (char *) print_buff);
	printf("Sending read query: %s\n", print_buff);

	// Send packet to server and wait for reply:

    int n = write(sockfd, tmp_out_buf, len);
    if (n < 0)
      error("ERROR writing to socket");

    bzero(in_buf, BUFSIZE);
    n = read(sockfd, in_buf, BUFSIZE);
    if (n < 0)
      error("ERROR reading from socket");

    read_response_message * response;
    success = deserialize_write_query(in_buf, n, &response);

	return response;
}



int populate_db(db_schema_t * schema, int sockfd, unsigned int * fastrandstate)
{
	uuid_t txnid;
	uuid_generate(txnid);
	WORD * column_values = (WORD *) malloc(no_cols * sizeof(WORD));

	for(long aid=0;aid<no_actors;aid++)
	{
		for(long cid=0;cid<no_collections;cid++)
		{
			for(long iid=0;iid<no_items;iid++)
			{
				column_values[0] = (WORD) aid;
				column_values[1] = (WORD) cid;
				column_values[2] = (WORD) iid;
				column_values[3] = (WORD) iid + 1;

				if(db_remote_insert(column_values, no_cols, (WORD) 0, schema, &txnid, requests++, sockfd) != 0)
					return -1;
			}
		}
	}

	return 0;
}

int delete_test(db_schema_t * schema, int sockfd, unsigned int * fastrandstate)
// Deletes row for last actor
{
	uuid_t txnid;
	uuid_generate(txnid);
	return db_remote_delete_row((WORD *) (no_actors - 1), 1, (WORD) 0, schema, &txnid, requests++, sockfd);
}

int test_search_pk_ck1_ck2(db_schema_t * schema, int sockfd, unsigned int * fastrandstate)
{
	char print_buff[1024];
	WORD * cks = (WORD *) malloc(2 * sizeof(WORD));

	uuid_t txnid;
	uuid_generate(txnid);

	for(long aid=0;aid<no_actors;aid++)
	{
		for(long cid=0;cid<no_collections;cid++)
		{
			for(long iid=0;iid<no_items;iid++)
			{
				cks[0] = (WORD) cid;
				cks[1] = (WORD) iid;

				read_response_message * response = db_remote_search_clustering((WORD *) &aid, cks, 2, (WORD) 0, schema, &txnid, requests++, sockfd);
				to_string_write_query(response, (char *) print_buff);
				printf("Got back response: %s\n", print_buff);

//				if((long) row->key != cid)
//				{
//					printf("Read back mismatched ck1 %ld ( != %ld) in cell (%ld, %ld)!\n", (long) row->key, cid, aid, cid);
//					return -1;
//				}
			}
		}
	}

	return 0;
}


int main(int argc, char **argv) {
    int sockfd, portno, n;
    struct sockaddr_in serveraddr;
    struct hostent *server;
    char *hostname;
    char buf[BUFSIZE];
    unsigned int seed;

    GET_RANDSEED(&seed, 0); // thread_id

    /* check command line arguments */
    if (argc != 3) {
       fprintf(stderr,"usage: %s <hostname> <port>\n", argv[0]);
       exit(0);
    }
    hostname = argv[1];
    portno = atoi(argv[2]);

    db_schema_t * schema = create_schema();

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0)
        error("ERROR opening socket");

    server = gethostbyname(hostname);
    if (server == NULL) {
        fprintf(stderr,"ERROR, no such host as %s\n", hostname);
        exit(0);
    }

    bzero((void *) &serveraddr, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    bcopy((char *)server->h_addr,
	  (char *)&serveraddr.sin_addr.s_addr, server->h_length);
    serveraddr.sin_port = htons(portno);

    if (connect(sockfd, (struct sockaddr *) &serveraddr, sizeof(serveraddr)) < 0)
      error("ERROR connecting");

    bzero(out_buf, BUFSIZE);

    populate_db(schema, sockfd, &seed);

    test_search_pk_ck1_ck2(schema, sockfd, &seed);

    close(sockfd);
    return 0;
}


