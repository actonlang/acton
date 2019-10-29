/*
 * client_api.c
 *
 *      Author: aagapi
 */

#include "client_api.h"

long requests=0;
// char out_buf[BUFSIZE];
// char in_buf[BUFSIZE];

int queue_callback_cmp(WORD e1, WORD e2)
{
	queue_callback_args * a1 = (queue_callback_args *) e1;
	queue_callback_args * a2 = (queue_callback_args *) e2;

	if(a1->consumer_id != a2->consumer_id)
		return (long) a1->consumer_id - (long) a2->consumer_id;

	if(a1->queue_id != a2->queue_id)
		return (long) a1->queue_id - (long) a2->queue_id;

	if(a1->table_key != a2->table_key)
		return (long) a1->table_key - (long) a2->table_key;

	if(a1->shard_id != a2->shard_id)
		return (long) a1->shard_id - (long) a2->shard_id;

	if(a1->app_id != a2->app_id)
		return (long) a1->app_id - (long) a2->app_id;

	return 0;
}

int sockaddr_cmp(WORD a1, WORD a2)
{
	struct sockaddr * x = (struct sockaddr *) a1;
	struct sockaddr * y = (struct sockaddr *) a2;

#define CMP(a, b) if (a != b) return a - b

    CMP(x->sa_family, y->sa_family);

    if (x->sa_family == AF_UNIX) {
        struct sockaddr_un *xun = (void*)x, *yun = (void*)y;
        int r = strcmp(xun->sun_path, yun->sun_path);
        if (r != 0)
            return r;
    } else if (x->sa_family == AF_INET) {
        struct sockaddr_in *xin = (void*)x, *yin = (void*)y;
        CMP(ntohl(xin->sin_addr.s_addr), ntohl(yin->sin_addr.s_addr));
        CMP(ntohs(xin->sin_port), ntohs(yin->sin_port));
    } else if (x->sa_family == AF_INET6) {
        struct sockaddr_in6 *xin6 = (void*)x, *yin6 = (void*)y;
        int r = memcmp(xin6->sin6_addr.s6_addr, yin6->sin6_addr.s6_addr, sizeof(xin6->sin6_addr.s6_addr));
        if (r != 0)
            return r;
        CMP(ntohs(xin6->sin6_port), ntohs(yin6->sin6_port));
        CMP(xin6->sin6_flowinfo, yin6->sin6_flowinfo);
        CMP(xin6->sin6_scope_id, yin6->sin6_scope_id);
    } else {
        assert(!"unknown sa_family");
    }

#undef CMP
    return 0;
}

// Remote DB API:

remote_db_t * get_remote_db(int quorum_size)
{
	remote_db_t * db = (remote_db_t *) malloc(sizeof(remote_db_t) + sizeof(pthread_mutex_t));

	db->servers = create_skiplist(&sockaddr_cmp);
	db->txn_state = create_skiplist_uuid();
	db->queue_subscriptions = create_skiplist(&queue_callback_cmp);
	db->subscribe_lock = (pthread_mutex_t*) ((char*) db + sizeof(remote_db_t));
	pthread_mutex_init(db->subscribe_lock, NULL);
	db->quorum_size = quorum_size;

	return db;
}

int add_server_to_membership(char *hostname, int portno, remote_db_t * db, unsigned int * seedptr)
{
    remote_server * rs = get_remote_server(hostname, portno);

    if(skiplist_search(db->servers, &rs->serveraddr) != NULL)
    {
		fprintf(stderr, "ERROR: Server address %s:%d was already added to membership!\n", hostname, portno);
		free_remote_server(rs);
		return -1;
    }

    int status = skiplist_insert(db->servers, &rs->serveraddr, rs, seedptr);

    if(status != 0)
    {
		fprintf(stderr, "ERROR: Error adding server address %s:%d to membership!\n", hostname, portno);
		free_remote_server(rs);
		return -2;
    }

    return 0;
}

long get_nonce(remote_db_t * db)
{
#ifdef RANDOM_NONCES
	unsigned int randno1, randno2;
	long randlong;
	FASTRAND(&(db->fastrandstate), randno1);
	FASTRAND(&(db->fastrandstate), randno2);
	return ((long) randno1 << 32) | ((long) randno2 & 0xFFFFFFFFL);
#else
	return ++requests;
#endif
}

int close_remote_db(remote_db_t * db)
{
	for(snode_t * crt = HEAD(db->servers); crt!=NULL; crt = NEXT(crt))
	{
		remote_server * rs = (remote_server *) crt->value;
		close(rs->sockfd);
	}

	return free_remote_db(db);
}

void free_remote_server_ptr(WORD ptr);

int free_remote_db(remote_db_t * db)
{
	skiplist_free_val(db->servers, &free_remote_server_ptr);
	skiplist_free(db->txn_state);
	skiplist_free(db->queue_subscriptions);
	free(db);
	return 0;
}

// Remote server struct fctns:

remote_server * get_remote_server(char *hostname, int portno)
{
	remote_server * rs = (remote_server *) malloc(sizeof(remote_server));
    bzero(rs, sizeof(remote_server));

	rs->sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (rs->sockfd < 0)
    {
        fprintf(stderr, "ERROR opening socket!\n");
        free_remote_server(rs);
        return NULL;
    }

    rs->server = gethostbyname(hostname);
    if (rs->server == NULL)
    {
        fprintf(stderr, "ERROR, no such host %s\n", hostname);
        free_remote_server(rs);
        return NULL;
    }

//    bzero((void *) &rs->serveraddr, sizeof(struct sockaddr_in));
    rs->serveraddr.sin_family = AF_INET;
    bcopy((char *)rs->server->h_addr,
	  (char *)&(rs->serveraddr.sin_addr.s_addr), rs->server->h_length);
    rs->serveraddr.sin_port = htons(portno);

    if (connect(rs->sockfd, (struct sockaddr *) &rs->serveraddr, sizeof(struct sockaddr_in)) < 0)
    {
    		fprintf(stderr, "ERROR connecting to %s:%d\n", hostname, portno);
        free_remote_server(rs);
        return NULL;
    }

    snprintf((char *) &rs->id, 256, "%s:%d", hostname, portno);

	return rs;
}

void free_remote_server(remote_server * rs)
{
	free(rs);
}

void free_remote_server_ptr(WORD ptr)
{
	free_remote_server((remote_server *) ptr);
}


// Comm fctns:

/*
 * error - wrapper for perror
 */
void error(char *msg) {
    perror(msg);
    exit(0);
}

int send_packet(void * buf, unsigned len, int sockfd)
{
    int n = write(sockfd, buf, len);
    if (n < 0)
    {
    		error("ERROR writing to socket");
    }
    else
    {
#if CLIENT_VERBOSITY > 0
		printf("Wrote %d bytes to socket\n", n);
#endif
    }

    return 0;
}

int send_packet_wait_reply(void * out_buf, unsigned out_len, int sockfd, void * in_buf, unsigned in_buf_size, int * in_len)
{
	int ret = send_packet(out_buf, out_len, sockfd);

	if(ret != 0)
		return ret;

    bzero(in_buf, in_buf_size);
    *in_len = -1;
    while(*in_len < 0)
    {
    		*in_len = read(sockfd, in_buf, BUFSIZE);
		if (*in_len < 0)
			error("ERROR reading from socket");
		else
			printf("Read %d bytes from socket\n", *in_len);
    }

    return 0;
}

// Write ops:

int remote_insert_in_txn(WORD * column_values, int no_cols, WORD table_key, db_schema_t * schema, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	write_query * wq = build_insert_in_txn(column_values, no_cols, schema->no_primary_keys, schema->no_clustering_keys, table_key, txnid, get_nonce(db));
	void * tmp_out_buf = NULL;
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending write query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

	return ack->status;
}

int remote_update_in_txn(int * col_idxs, int no_cols, WORD * column_values, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}

int remote_delete_row_in_txn(WORD * column_values, int no_cols, WORD table_key, db_schema_t * schema, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	write_query * wq = build_delete_row_in_txn(column_values, schema->no_primary_keys, table_key, txnid, get_nonce(db));
	void * tmp_out_buf = NULL;
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending delete row query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

	return ack->status;
}

int remote_delete_cell_in_txn(WORD * column_values, int no_cols, int no_clustering_keys, db_schema_t * schema, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	write_query * wq = build_delete_cell_in_txn(column_values, schema->no_primary_keys, no_clustering_keys, table_key, txnid, get_nonce(db));
	void * tmp_out_buf = NULL;
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending delete cell query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

	return ack->status;

}

int remote_delete_by_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}


// Read ops:

db_row_t* remote_search_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key,
		uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	read_query * q = build_search_in_txn(primary_keys, no_primary_keys, table_key, txnid, get_nonce(db));
	int success = serialize_read_query(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return NULL;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_read_query(q, (char *) print_buff);
	printf("Sending read row query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    read_response_message * response;
    success = deserialize_write_query(rs->in_buf, n, &response);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_write_query(response, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

    if(success != 0)
    		return NULL;

    return create_db_row_schemaless2((WORD *) response->cell->keys, response->cell->no_keys,
        									(WORD *) response->cell->columns, response->cell->no_columns, &(db->fastrandstate));
}

db_row_t* remote_search_clustering_in_txn(WORD* primary_keys, WORD* clustering_keys, int no_clustering_keys,
														WORD table_key, db_schema_t * schema, uuid_t * txnid,
														remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	read_query * q = build_search_clustering_in_txn(primary_keys, schema->no_primary_keys, clustering_keys, schema->no_clustering_keys, table_key, txnid, get_nonce(db));
	int success = serialize_read_query(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return NULL;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_read_query(q, (char *) print_buff);
	printf("Sending read cell query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    read_response_message * response;
    success = deserialize_write_query(rs->in_buf, n, &response);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_write_query(response, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

    return create_db_row_schemaless2((WORD *) response->cell->keys, response->cell->no_keys,
        									(WORD *) response->cell->columns, response->cell->no_columns, &(db->fastrandstate));
}

db_row_t* remote_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
									WORD* col_keys, int no_columns, WORD table_key,
									uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}

db_row_t* remote_search_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported; TO DO
	return 0;
}

int remote_range_search_in_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
							snode_t** start_row, snode_t** end_row,
							WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	range_read_query * q = build_range_search_in_txn(start_primary_keys, end_primary_keys, no_primary_keys, table_key, txnid, get_nonce(db));
	int success = serialize_range_read_query(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_range_read_query(q, (char *) print_buff);
	printf("Sending range read row query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    range_read_response_message * response;
    success = deserialize_range_read_response_message(rs->in_buf, n, &response);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_range_read_response_message(response, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

    if(success < 0)
    		return success;

    // Parse range_read_response_message to row list:

    skiplist_t * rows = create_skiplist_long();
    for(int i=0;i<response->no_cells;i++)
    {
    		db_row_t * row = create_db_row_schemaless2((WORD *) response->cells[i].keys, response->cells[i].no_keys,
    													(WORD *) response->cells[i].columns, response->cells[i].no_columns,
													&(db->fastrandstate)); // Note that cell versions are only kept on the server, we don't return them to the client
    		skiplist_insert(rows, (WORD) response->cells[i].keys[0], (WORD) row, &(db->fastrandstate));
    }

    *start_row = HEAD(rows);
    for(*end_row = *start_row;*end_row != NULL;*end_row = NEXT(*end_row));

	return success;
}

int remote_range_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys,
									 WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
									 snode_t** start_row, snode_t** end_row,
									 WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	range_read_query * q = build_range_search_clustering_in_txn(primary_keys, no_primary_keys,
															start_clustering_keys, end_clustering_keys, no_clustering_keys,
															table_key, txnid, get_nonce(db));
	int success = serialize_range_read_query(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_range_read_query(q, (char *) print_buff);
	printf("Sending range read cell query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    range_read_response_message * response;
    success = deserialize_range_read_response_message(rs->in_buf, n, &response);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_range_read_response_message(response, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

    if(success < 0)
    		return success;

    // Parse range_read_response_message to row list:

    skiplist_t * rows = create_skiplist_long();
    for(int i=0;i<response->no_cells;i++)
    {
    		db_row_t * row = create_db_row_schemaless2((WORD *) response->cells[i].keys, response->cells[i].no_keys,
    													(WORD *) response->cells[i].columns, response->cells[i].no_columns,
													&(db->fastrandstate)); // Note that cell versions are only kept on the server, we don't return them to the client
    		skiplist_insert(rows, (WORD) response->cells[i].keys[0], (WORD) row, &(db->fastrandstate));
    }

    *start_row = HEAD(rows);
    for(*end_row = *start_row;*end_row != NULL;*end_row = NEXT(*end_row));

	return success;
}

int remote_range_search_index_in_txn(int idx_idx, WORD start_idx_key, WORD end_idx_key,
								snode_t** start_row, snode_t** end_row,
								WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported; TO DO
	return 0;
}

// Queue ops:

int remote_create_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_create_queue_in_txn(table_key, queue_id, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

	return ack->status;
}

int remote_delete_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_delete_queue_in_txn(table_key, queue_id, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

	return ack->status;
}

int remote_enqueue_in_txn(WORD * column_values, int no_cols, WORD table_key, WORD queue_id, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_enqueue_in_txn(column_values, no_cols, table_key, queue_id, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

	return ack->status;
}

int remote_read_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, long * new_read_head,
		snode_t** start_row, snode_t** end_row, uuid_t * txnid,
		remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_read_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, max_entries, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

	queue_query_message * response;
    success = deserialize_queue_message(rs->in_buf, n, &response);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_queue_message(response, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

    assert(response->msg_type == QUERY_TYPE_READ_QUEUE_RESPONSE);

    if(success < 0)
    		return success;

    // TO DO: Parse queue read response message to row list:

    skiplist_t * rows = create_skiplist_long();
    for(int i=0;i<response->no_cells;i++)
    {
    		db_row_t * row = create_db_row_schemaless2((WORD *) response->cells[i].keys, response->cells[i].no_keys,
    													(WORD *) response->cells[i].columns, response->cells[i].no_columns,
													&(db->fastrandstate)); // Note that cell versions are only kept on the server, we don't return them to the client
    		skiplist_insert(rows, (WORD) response->cells[i].keys[0], (WORD) row, &(db->fastrandstate));
    }

    *start_row = HEAD(rows);
    for(*end_row = *start_row;*end_row != NULL;*end_row = NEXT(*end_row));

	return response->status;
}

int remote_consume_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					long new_consume_head, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_consume_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, new_consume_head, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

	return ack->status;
}

int remote_subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
						remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_subscribe_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, NULL, get_nonce(db)); // txnid
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

    if(ack->status == CLIENT_ERR_SUBSCRIPTION_EXISTS)
    		return CLIENT_ERR_SUBSCRIPTION_EXISTS;

    // Add local subscription on client:

    return subscribe_queue_client(consumer_id, shard_id, app_id, table_key, queue_id, callback, 1, db);
}

int remote_unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_unsubscribe_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, NULL, get_nonce(db)); // txnid
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

    ack_message * ack;
    success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

    if(ack->status == CLIENT_ERR_NO_SUBSCRIPTION_EXISTS)
    		return CLIENT_ERR_NO_SUBSCRIPTION_EXISTS;

    // Remove local subscription from client:

    return unsubscribe_queue_client(consumer_id, shard_id, app_id, table_key, queue_id, 1, db);
}

int remote_subscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
						uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}

int remote_unsubscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
								uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}

// Subscription handling client-side:

int subscribe_queue_client(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					queue_callback * callback, short use_lock, remote_db_t * db)
{
	queue_callback_args * qca = get_queue_callback_args(table_key, queue_id, app_id, shard_id, consumer_id, QUEUE_NOTIF_ENQUEUED);

	if(use_lock)
		pthread_mutex_lock(db->subscribe_lock);

	snode_t * subscription_node = skiplist_search(db->queue_subscriptions, (WORD) qca);

	if(subscription_node != NULL)
	{
		if(use_lock)
			pthread_mutex_unlock(db->subscribe_lock);

		return CLIENT_ERR_SUBSCRIPTION_EXISTS; // Subscription already exists
	}

	int status = skiplist_insert(db->queue_subscriptions, (WORD) qca, (WORD) callback, &(db->fastrandstate));

	if(use_lock)
		pthread_mutex_unlock(db->subscribe_lock);

	assert(status == 0);

#if (VERBOSITY > 0)
	printf("CLIENT: Subscriber %ld/%ld/%ld subscribed queue %ld/%ld with callback %p\n",
					(long) app_id, (long) shard_id, (long) consumer_id,
					(long) table_key, (long) queue_id, cs->callback);
#endif

	return status;
}

int unsubscribe_queue_client(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						short use_lock, remote_db_t * db)
{
	queue_callback_args * qca = get_queue_callback_args(table_key, queue_id, app_id, shard_id, consumer_id, QUEUE_NOTIF_ENQUEUED);

	if(use_lock)
		pthread_mutex_lock(db->subscribe_lock);

	queue_callback * callback = skiplist_delete(db->queue_subscriptions, (WORD) qca);

	if(use_lock)
		pthread_mutex_unlock(db->subscribe_lock);

	assert(callback != NULL);

	free_queue_callback(callback);

#if (VERBOSITY > 0)
	printf("CLIENT: Subscriber %ld/%ld/%ld unsubscribed queue %ld/%ld with callback %p\n",
					(long) app_id, (long) shard_id, (long) consumer_id,
					(long) table_key, (long) queue_id, cs->callback);
#endif

	return (callback != NULL)?0:CLIENT_ERR_NO_SUBSCRIPTION_EXISTS;
}


// Txn mgmt:

uuid_t * remote_new_txn(remote_db_t * db, unsigned int * seedptr)
{
	uuid_t * txnid = NULL;
	unsigned len = 0;
	void * tmp_out_buf = NULL;
	int status = -2;

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return NULL;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

	while(status == -2) // txnid already exists on server
	{
		txnid = new_client_txn(db, seedptr);
		txn_message * q = build_new_txn(txnid, get_nonce(db));
		int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len);

	#if CLIENT_VERBOSITY > 0
		char print_buff[1024];
		to_string_txn_message(q, (char *) print_buff);
		printf("Sending new txn to server %s: %s\n", rs->id, print_buff);
	#endif

		// Send packet to server and wait for reply:

		int n = -1;
		success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

		ack_message * ack;
		success = deserialize_ack_message(rs->in_buf, n, &ack);
	    assert(success == 0);
		status = ack->status;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	assert(status == 0);

	return txnid;
}

int _remote_validate_txn(uuid_t * txnid, vector_clock * version, remote_server * rs_in, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	txn_message * q = build_validate_txn(txnid, version, get_nonce(db));
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (rs_in != NULL)?(rs_in):((remote_server *) (HEAD(db->servers))->value);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending validate txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

	ack_message * ack;
	success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response: %s\n", print_buff);
#endif

	return ack->status;
}

int remote_validate_txn(uuid_t * txnid, vector_clock * version, remote_db_t * db)
{
	return _remote_validate_txn(txnid, version, NULL, db);

}

int _remote_abort_txn(uuid_t * txnid, remote_server * rs_in, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	txn_message * q = build_abort_txn(txnid, get_nonce(db));
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (rs_in != NULL)?(rs_in):((remote_server *) (HEAD(db->servers))->value);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending abort txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

	ack_message * ack;
	success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response: %s\n", print_buff);
#endif

	return ack->status;
}

int remote_abort_txn(uuid_t * txnid, remote_db_t * db)
{
	return _remote_abort_txn(txnid, NULL, db);
}

int _remote_persist_txn(uuid_t * txnid, vector_clock * version, remote_server * rs_in, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	txn_message * q = build_commit_txn(txnid, version, get_nonce(db));
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (rs_in != NULL)?(rs_in):((remote_server *) (HEAD(db->servers))->value);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending commit txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	int n = -1;
	success = send_packet_wait_reply(tmp_out_buf, len, rs->sockfd, (void *) (&rs->in_buf), BUFSIZE, &n);

	ack_message * ack;
	success = deserialize_ack_message(rs->in_buf, n, &ack);
    assert(success == 0);

#if CLIENT_VERBOSITY > 0
	to_string_ack_message(ack, (char *) print_buff);
	printf("Got back response: %s\n", print_buff);
#endif

	return ack->status;
}

int remote_commit_txn(uuid_t * txnid, vector_clock * version, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

#if (CLIENT_VERBOSITY > 0)
	char uuid_str[37];
	uuid_unparse_lower(*txnid, uuid_str);
#endif
#if (CLIENT_VERBOSITY > 1)
	printf("CLIENT: Attempting to validate txn %s\n", uuid_str);
#endif

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->quorum_size);
		return -1;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

	int res = _remote_validate_txn(txnid, version, rs, db);

#if (CLIENT_VERBOSITY > 1)
	printf("CLIENT: validate txn %s from server %s returned %d\n", uuid_str, rs->id, res);
#endif

	if(res == VAL_STATUS_COMMIT)
	{
		int persist_status = -2;
		while(persist_status != 0)
		{
			persist_status = _remote_persist_txn(txnid, version, rs, db);

#if (CLIENT_VERBOSITY > 0)
			printf("CLIENT: persist txn %s from server %s returned %d\n", uuid_str, rs->id, persist_status);
#endif
		}

		res = close_client_txn(txnid, db); // Clear local cached txn state on client

#if (CLIENT_VERBOSITY > 1)
		printf("CLIENT: close txn %s returned %d\n", uuid_str, res);
#endif
	}
	else if(res == VAL_STATUS_ABORT)
	{
		res = _remote_abort_txn(txnid, rs, db);

#if (CLIENT_VERBOSITY > 0)
		printf("CLIENT: abort txn %s from server %s returned %d\n", uuid_str, rs->id, res);
#endif
	}
	else
	{
		assert(0);
	}

	return 0;
}

// Txn state handling client-side:

txn_state * get_client_txn_state(uuid_t * txnid, remote_db_t * db)
{
	snode_t * txn_node = (snode_t *) skiplist_search(db->txn_state, (WORD) txnid);

	return (txn_node != NULL)? (txn_state *) txn_node->value : NULL;
}

uuid_t * new_client_txn(remote_db_t * db, unsigned int * seedptr)
{
	txn_state * ts = NULL, * previous = NULL;

	while(ts == NULL)
	{
		ts = init_txn_state();
		previous = get_client_txn_state(&(ts->txnid), db);
		if(previous != NULL)
		{
			free_txn_state(ts);
			ts = NULL;
		}
	}

	skiplist_insert(db->txn_state, (WORD) &(ts->txnid), (WORD) ts, seedptr);

	return &(ts->txnid);
}

int close_client_txn(uuid_t * txnid, remote_db_t * db)
{
	txn_state * ts = get_client_txn_state(txnid, db);
	if(ts == NULL)
		return -2; // No such txn

	skiplist_delete(db->txn_state, txnid);
	free_txn_state(ts);

	return 0;
}




