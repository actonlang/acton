/*
 * Copyright (C) 2019-2021 Deutsche Telekom AG
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * client_api.c
 *
 *      Author: aagapi
 */

#include "client_api.h"

int64_t requests=0;

int queue_callback_cmp(WORD e1, WORD e2)
{
	queue_callback_args * a1 = (queue_callback_args *) e1;
	queue_callback_args * a2 = (queue_callback_args *) e2;

	if(a1->consumer_id != a2->consumer_id)
		return (int64_t) a1->consumer_id - (int64_t) a2->consumer_id;

	if(a1->queue_id != a2->queue_id)
		return (int64_t) a1->queue_id - (int64_t) a2->queue_id;

	if(a1->table_key != a2->table_key)
		return (int64_t) a1->table_key - (int64_t) a2->table_key;

	if(a1->shard_id != a2->shard_id)
		return (int64_t) a1->shard_id - (int64_t) a2->shard_id;

	if(a1->app_id != a2->app_id)
		return (int64_t) a1->app_id - (int64_t) a2->app_id;

	return 0;
}

// Remote DB API:

void * comm_thread_loop(void * args);

remote_db_t * get_remote_db(int replication_factor)
{
	remote_db_t * db = (remote_db_t *) malloc(sizeof(remote_db_t) + 3 * sizeof(pthread_mutex_t));
	memset(db, 0, sizeof(remote_db_t) + 3 * sizeof(pthread_mutex_t));

	db->servers = create_skiplist(&sockaddr_cmp);
	db->txn_state = create_skiplist_uuid();
	db->queue_subscriptions = create_skiplist(&queue_callback_cmp);
	db->msg_callbacks = create_skiplist_long();
	db->subscribe_lock = (pthread_mutex_t*) ((char*) db + sizeof(remote_db_t));
	pthread_mutex_init(db->subscribe_lock, NULL);
	db->msg_callbacks_lock = (pthread_mutex_t*) ((char*) db + sizeof(remote_db_t) + sizeof(pthread_mutex_t));
	pthread_mutex_init(db->msg_callbacks_lock, NULL);
	db->lc_lock = (pthread_mutex_t*) ((char*) db + sizeof(remote_db_t) + 2 * sizeof(pthread_mutex_t));
	pthread_mutex_init(db->lc_lock, NULL);

	db->replication_factor = replication_factor;
	db->quorum_size = (int) (replication_factor / 2) + 1;
	db->rpc_timeout = 10;

	db->stop_comm = 0;
	assert(pthread_create(&(db->comm_thread), NULL, comm_thread_loop, db) == 0);

	db->my_lc = init_empty_vc();

	return db;
}

int handle_socket_close(int * childfd)
{
	struct sockaddr_in address;
	int addrlen;
	getpeername(*childfd , (struct sockaddr*)&address,
				(socklen_t*)&addrlen);
	printf("Host disconnected , ip %s , port %d \n" ,
		  inet_ntoa(address.sin_addr) , ntohs(address.sin_port));

	//Close the socket and mark as 0 in list for reuse
	close(*childfd);
	*childfd = 0;

	return 0;
}


void * comm_thread_loop(void * args)
{
	remote_db_t * db = (remote_db_t *) args;
	struct timeval timeout;
	timeout.tv_sec = 3;
	timeout.tv_usec = 0;
	char in_buf[BUFSIZE];
	int msg_len = -1;
	int announced_msg_len = -1;
	int read_buf_offset = 0;

	while(!db->stop_comm)
	{
		FD_ZERO(&(db->readfds));
		int max_fd = -1;

		for(snode_t * crt = HEAD(db->servers); crt!=NULL; crt = NEXT(crt))
		{
			remote_server * rs = (remote_server *) crt->value;
			if(rs->sockfd > 0)
			{
//				printf("Listening to server socket %s..\n", rs->id);
				FD_SET(rs->sockfd, &(db->readfds));
				max_fd = (rs->sockfd > max_fd)? rs->sockfd : max_fd;
			}
			else
			{
//				printf("Not listening to disconnected server socket %s..\n", rs->id);
			}
		}

		int status = select(max_fd + 1 , &(db->readfds) , NULL , NULL , &timeout);

        if ((status < 0) && (errno!=EINTR))
        {
            printf("select error!\n");
            assert(0);
        }

		for(snode_t * crt = HEAD(db->servers); crt!=NULL; crt = NEXT(crt))
		{
			remote_server * rs = (remote_server *) crt->value;
			if(rs->sockfd > 0 && FD_ISSET(rs->sockfd , &(db->readfds)))
			// Received a msg from this server:
			{
				int skip_parsing = 0;

				while(1) // Loop until reading complete packet:
				{
					assert(read_buf_offset < BUFSIZE - sizeof(int));

					if(read_buf_offset == 0)
					{
						// Read msg len header from packet:

						bzero(in_buf, BUFSIZE);
						msg_len = -1;

						int size_len = read(rs->sockfd, in_buf, sizeof(int));

						if (size_len < 0)
						{
//							fprintf(stderr, "ERROR reading from socket\n");
							continue;
						}
						else if (size_len == 0)
						{
							handle_socket_close(&(rs->sockfd));
							skip_parsing = 1;
							break;
						}

						announced_msg_len = *((int *)in_buf);

						*((int *)in_buf) = 0; // 0 back buffer

						read_buf_offset = 0;
					}

					if(announced_msg_len <= 0)
					{
						read_buf_offset = 0;
						continue;
					}

				    msg_len = read(rs->sockfd, in_buf + sizeof(int) + read_buf_offset, announced_msg_len - read_buf_offset);

#if CLIENT_VERBOSITY > 1
					printf("announced_msg_len=%d, msg_len=%d, read_buf_offset=%d\n", announced_msg_len, msg_len, read_buf_offset);
#endif

				    if (msg_len < 0)
				    {
				    		fprintf(stderr, "ERROR reading from socket\n");
						continue;
				    }
					else if(msg_len == 0) // client closed socket
				    {
						handle_socket_close(&(rs->sockfd));
						skip_parsing = 1;
				        break;
				    }
					else if(msg_len < announced_msg_len - read_buf_offset)
					{
						read_buf_offset += msg_len;
						continue; // Continue reading socket until full packet length
					}

				    break;
				}

				if(skip_parsing)
					continue;

			    assert(announced_msg_len == msg_len);

			    read_buf_offset = 0; // Reset

#if CLIENT_VERBOSITY > 1
			    printf("client received %d / %d bytes\n", announced_msg_len, msg_len);
#endif

			    void * tmp_out_buf = NULL, * q = NULL;
			    short msg_type;
				db_schema_t * schema;
				int64_t nonce = -1;

				vector_clock * lc_read = NULL;
			    int status = parse_message(in_buf + sizeof(int), msg_len, &q, &msg_type, &nonce, 0, &lc_read);

			    if(status != 0)
			    {
			    		fprintf(stderr, "ERROR decoding server response!\n");
			    		continue;
			    		assert(0);
			    }

			    if(lc_read != NULL)
			    {
			    		update_lc_protected(db, lc_read);
			    		free_vc(lc_read);
			    }

			    if(nonce > 0) // A server reply
			    {
			    		status = add_reply_to_nonce(q, msg_type, nonce, db);
			    }
			    else // A queue notification
			    {
			    		// Notify local subscriber if found:

			    		assert(msg_type == RPC_TYPE_QUEUE);

			    		queue_query_message * qqm = (queue_query_message *) q;

			    		assert(qqm->msg_type == QUERY_TYPE_QUEUE_NOTIFICATION);

			    		WORD notif_table_key = (WORD) qqm->cell_address->table_key;
			    		WORD notif_queue_id = (WORD) qqm->cell_address->keys[0];

			    		queue_callback * qc = get_queue_client_callback((WORD) qqm->consumer_id, (WORD) qqm->shard_id, (WORD) qqm->app_id,
			    														notif_table_key, notif_queue_id,
																	1, db);

			    		if(qc == NULL)
			    		{
						fprintf(stderr, "CLIENT: No local subscriber subscriber %" PRId64 "/%" PRId64 "/%" PRId64 " exists for queue %" PRId64 "/%" PRId64 "!\n",
																		(int64_t) qqm->consumer_id, (int64_t) qqm->shard_id, (int64_t) qqm->app_id,
																		(int64_t) notif_table_key, (int64_t) notif_queue_id);
						continue;
			    		}

					queue_callback_args * qca = get_queue_callback_args(notif_table_key, notif_queue_id, (WORD) qqm->app_id, (WORD) qqm->shard_id, (WORD) qqm->consumer_id, QUEUE_NOTIF_ENQUEUED);

#if (CLIENT_VERBOSITY > 0)
					printf("CLIENT: Attempting to notify local subscriber %" PRId64 " (%p/%p/%p/%p)\n", (int64_t) qqm->consumer_id, qc, qc->lock, qc->signal, qc->callback);
#endif

					status = pthread_mutex_lock(qc->lock);

#if (CLIENT_LOCK_VERBOSITY > 0)
					printf("CLIENT: Locked consumer lock of %" PRId64 " (%p/%p), status=%d\n", (int64_t) qqm->consumer_id, qc, qc->lock, status);
#endif

					pthread_cond_signal(qc->signal);
					qc->callback(qca);
					status = pthread_mutex_unlock(qc->lock);
					assert(status == 0);

#if (CLIENT_LOCK_VERBOSITY > 0)
					printf("CLIENT: Unlocked consumer lock of %" PRId64 " (%p/%p), status=%d\n", (int64_t) qqm->consumer_id, qc, qc->lock, status);
#endif

#if (CLIENT_VERBOSITY > 0)
					printf("CLIENT: Notified local subscriber %" PRId64 " (%p/%p/%p/%p)\n", (int64_t) qqm->consumer_id, qc, qc->lock, qc->signal, qc->callback);
#endif
			    }

//			    assert(status > 0);
			}
		}
	}

	return NULL;
}

int add_server_to_membership(char *hostname, int portno, remote_db_t * db, unsigned int * seedptr)
{
	struct sockaddr_in dummy_serveraddr;

    remote_server * rs = get_remote_server(hostname, portno, dummy_serveraddr, -2, 1);

    if(rs == NULL)
    {
		printf("ERROR: Failed joining server %s:%d (DNS/network problem?)!\n", hostname, portno);
    		return 1;
    }

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

    if(rs->status == NODE_DEAD)
    {
		printf("ERROR: Failed joining server %s:%d (it looks down)!\n", hostname, portno);
    		return 1;
    }

    return 0;
}

msg_callback * add_msg_callback(int64_t nonce, void (*callback)(void *), remote_db_t * db)
{
	pthread_mutex_lock(db->msg_callbacks_lock);

	snode_t * node = skiplist_search(db->msg_callbacks, (WORD) nonce);

	if(node != NULL)
	{
		pthread_mutex_unlock(db->msg_callbacks_lock);
		return NULL;
	}

	msg_callback * mc = get_msg_callback(nonce, NULL, callback, db->replication_factor);

    int status = skiplist_insert(db->msg_callbacks, (WORD) nonce, mc, &(db->fastrandstate));

	pthread_mutex_unlock(db->msg_callbacks_lock);

    if(status != 0)
    {
		fprintf(stderr, "ERROR: Found duplicate nonce %" PRId64 " when trying to add msg callback!\n", nonce);
		assert(0);
		delete_msg_callback(mc->nonce, db);
		return NULL;
    }

    return mc;
}

int add_reply_to_nonce(void * reply, short reply_type, int64_t nonce, remote_db_t * db)
{
	int ret = 0;

	pthread_mutex_lock(db->msg_callbacks_lock);

	snode_t * node = skiplist_search(db->msg_callbacks, (WORD) nonce);

	if(node == NULL)
	{
		pthread_mutex_unlock(db->msg_callbacks_lock);

//		printf("Nonce %" PRId64 " not found!\n", nonce);

		return -1;
	}

	msg_callback * mc = (msg_callback *) node->value;

	int no_replies = add_reply_to_msg_callback(reply, reply_type, mc);

	pthread_mutex_unlock(db->msg_callbacks_lock);

	if(no_replies < 0)
		return no_replies;

	// Signal consumer if a quorum of replies have arrived:
	if(no_replies >= db->quorum_size)
	{
		ret = pthread_mutex_lock(mc->lock);
		pthread_cond_signal(mc->signal);
		if(mc->callback != NULL)
			mc->callback(NULL);
		ret = pthread_mutex_unlock(mc->lock);
	}

	return no_replies;
}

int delete_msg_callback(int64_t nonce, remote_db_t * db)
{
	pthread_mutex_lock(db->msg_callbacks_lock);

	snode_t * node = skiplist_search(db->msg_callbacks, (WORD) nonce);

	if(node == NULL)
	{
		pthread_mutex_unlock(db->msg_callbacks_lock);

		return 1;
	}

	msg_callback * mc = (msg_callback *) node->value;

    skiplist_delete(db->msg_callbacks, (WORD) nonce);

    free_msg_callback(mc);

	pthread_mutex_unlock(db->msg_callbacks_lock);

    return 0;
}

int64_t _get_nonce(remote_db_t * db)
{
#ifdef RANDOM_NONCES
	unsigned int randno1, randno2;
	int64_t randlong;
	FASTRAND(&(db->fastrandstate), randno1);
	FASTRAND(&(db->fastrandstate), randno2);
	return ((int64_t) randno1 << 32) | ((int64_t) randno2 & 0xFFFFFFFFL);
#else
	return ++requests;
#endif
}

int64_t get_nonce(remote_db_t * db)
{
	int64_t nonce = -1;
	snode_t * node = (snode_t *) 1;

	while(node != NULL)
	{
		nonce = _get_nonce(db);
		pthread_mutex_lock(db->msg_callbacks_lock);
		node = skiplist_search(db->msg_callbacks, (WORD) nonce);
		pthread_mutex_unlock(db->msg_callbacks_lock);
	}

	return nonce;
}

vector_clock * get_and_increment_lc(remote_db_t * db, int node_id)
{
	pthread_mutex_lock(db->lc_lock);

	vector_clock * vc = copy_vc(db->my_lc);

	increment_vc(db->my_lc, node_id);

	pthread_mutex_unlock(db->lc_lock);

	return vc;
}

vector_clock * get_lc(remote_db_t * db)
{
	return copy_vc(db->my_lc);
}

int update_lc_protected(remote_db_t * db, vector_clock * vc_in)
{
	pthread_mutex_lock(db->lc_lock);

	int ret = update_vc(db->my_lc, vc_in);

	pthread_mutex_unlock(db->lc_lock);

	return ret;
}

int close_remote_db(remote_db_t * db)
{
	db->stop_comm = 1;
	pthread_join(db->comm_thread, NULL);

	for(snode_t * crt = HEAD(db->servers); crt!=NULL; crt = NEXT(crt))
	{
		remote_server * rs = (remote_server *) crt->value;
		close(rs->sockfd);
	}

	return free_remote_db(db);
}

int free_remote_db(remote_db_t * db)
{
#if (CLIENT_VERBOSITY > 0)
	printf("CLIENT: free_remote_db(): Deleting all client side state \n");
#endif

	skiplist_free_val(db->servers, &free_remote_server_ptr);
	skiplist_free(db->txn_state);
	skiplist_free(db->queue_subscriptions);
	free(db);
	free_vc(db->my_lc);
	return 0;
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
#if CLIENT_VERBOSITY > 2
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
		{
#if CLIENT_VERBOSITY > 2
			printf("Read %d bytes from socket\n", *in_len);
#endif
		}
    }

    return 0;
}

int wait_on_msg_callback(msg_callback * mc, remote_db_t * db)
{
	if(mc == NULL)
	{
		assert(0);

		return NO_SUCH_MSG_CALLBACK;
	}

	// Wait for signal from comm thread. It will come when 'db->quorum_size' replies have arrived on that nonce:

	int ret = pthread_mutex_lock(mc->lock);

	if(mc->no_replies >= db->quorum_size)
	// Enough replies arrived already
	{
		pthread_mutex_unlock(mc->lock);
		return 0;
	}

	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	ts.tv_sec += db->rpc_timeout;
	ret = pthread_cond_timedwait(mc->signal, mc->lock, &ts);

	pthread_mutex_unlock(mc->lock);

	if(ret != 0 && ret != ETIMEDOUT)
	{
		printf("pthread_cond_timedwait returned %d/%d\n", ret, errno);
		assert(0);
	}

	return 0;
}

int send_packet_wait_replies_async(void * out_buf, unsigned out_len, int64_t nonce, msg_callback ** mc, remote_db_t * db)
{
	int ret = 0;
	*mc = add_msg_callback(nonce, NULL, db);

	if(*mc == NULL)
	{
		assert(0);

		return -1;
	}

	for(snode_t * server_node = HEAD(db->servers); server_node!=NULL; server_node=NEXT(server_node))
	{
		remote_server * rs = (remote_server *) server_node->value;
#if SYNC_SOCKET > 0
		pthread_mutex_lock(rs->sockfd_lock);
#endif
		ret = send_packet(out_buf, out_len, rs->sockfd);
#if SYNC_SOCKET > 0
		pthread_mutex_unlock(rs->sockfd_lock);
#endif
		if(ret != 0)
		{
			assert(0);
#if CLIENT_VERBOSITY > 0
			printf("Server %s seems down.\n", rs->id);
#endif
		}
	}

	return 0;
}

int send_packet_wait_replies_sync(void * out_buf, unsigned out_len, int64_t nonce, msg_callback ** mc, remote_db_t * db)
{
	int ret = send_packet_wait_replies_async(out_buf, out_len, nonce, mc, db);

	if(ret != 0)
		return ret;

	// Wait for signal from comm thread. It will come when 'db->quorum_size' replies have arrived on that nonce:

	return wait_on_msg_callback(*mc, db);
}


// Write ops:

int remote_insert_in_txn(WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, WORD blob, size_t blob_size, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

#if (DEBUG_BLOBS > 0)
	size_t print_size = 256 + no_cols * sizeof(long) + blob_size;
	char * printbuf = (char *) malloc(print_size);
	char * crt_ptr = printbuf;
	sprintf(crt_ptr, "cols={");
	crt_ptr += strlen(crt_ptr);
	for(int i=0;i<no_cols;i++)
	{
		sprintf(crt_ptr, "%ld, ", (long) column_values[i]);
		crt_ptr += strlen(crt_ptr);
	}
	sprintf(crt_ptr, "}, blob(%d)={", blob_size);
	crt_ptr += strlen(crt_ptr);
	if(blob_size > 0)
	{
		for(int i=0;i<blob_size / sizeof(long);i++)
		{
			sprintf(crt_ptr, "%lu ", *((long *)blob + i));
			crt_ptr += strlen(crt_ptr);
		}
	}
	sprintf(crt_ptr, "}");
	crt_ptr += strlen(crt_ptr);
	printf("remote_insert_in_txn: %s\n", printbuf);
	free(printbuf);
#endif

	write_query * wq = build_insert_in_txn(column_values, no_cols, no_primary_keys, no_clustering_keys, blob, blob_size, table_key, txnid, get_nonce(db));
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "Not enough servers configured for quorum (%d/%d servers configured)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending write query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, wq->nonce, &mc, db);
	assert(success == 0);
	free_write_query(wq);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
//		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_update_in_txn(int * col_idxs, int no_cols, WORD * column_values, WORD blob, size_t blob_size, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}

int remote_delete_row_in_txn(WORD * column_values, int no_primary_keys, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	write_query * wq = build_delete_row_in_txn(column_values, no_primary_keys, table_key, txnid, get_nonce(db));
	void * tmp_out_buf = NULL;
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending delete row query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, wq->nonce, &mc, db);
	assert(success == 0);
	free_write_query(wq);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_delete_cell_in_txn(WORD * column_values, int no_primary_keys, int no_clustering_keys, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	write_query * wq = build_delete_cell_in_txn(column_values, no_primary_keys, no_clustering_keys, table_key, txnid, get_nonce(db));
	void * tmp_out_buf = NULL;
	int success = serialize_write_query(wq, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_write_query(wq, (char *) print_buff);
	printf("Sending delete cell query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, wq->nonce, &mc, db);
	assert(success == 0);
	free_write_query(wq);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_delete_by_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported
	return 0;
}


// Read ops:

int get_db_rows_forest_from_read_response(range_read_response_message * response, snode_t** start_row, snode_t** end_row, remote_db_t * db)
// If a DB query returned multiple cells, accumulate them all in a forest of trees rooted at
// elements of the returned list start_row->end_row. Return the number of roots found. For forests with a single root
// (which e.g. results from non-range queries will be), a list with a single element (located at start_row==end_row) will be returned:
{
	if(response->no_cells == 0) // No results
	{
		*start_row = NULL;
		*end_row = NULL;
		return 0;
	}

	skiplist_t * roots = create_skiplist_long();

	db_row_t* result = create_db_row_schemaless2((WORD *) response->cells[0].keys, response->cells[0].no_keys,
			(WORD *) response->cells[0].columns, response->cells[0].no_columns,
			response->cells[0].last_blob, response->cells[0].last_blob_size, &(db->fastrandstate));

	for(int i=0;i<response->no_cells;i++) // We have a deeper result than 1
	{
		db_row_t* root_cell = NULL;
		snode_t * root_cell_node = skiplist_search(roots, (WORD) response->cells[i].keys[0]);

		if(root_cell_node == NULL)
		{
//			printf("Creating new root cell for cell %d (%" PRId64 ")\n", i, response->cells[i].keys[0]);

			root_cell = create_db_row_schemaless2((WORD *) response->cells[i].keys, response->cells[i].no_keys,
					(WORD *) response->cells[i].columns, response->cells[i].no_columns,
					response->cells[i].last_blob, response->cells[i].last_blob_size, &(db->fastrandstate));
			skiplist_insert(roots, (WORD) response->cells[i].keys[0], (WORD) root_cell, &(db->fastrandstate));
			continue;
		}
		else
		{
			root_cell = (db_row_t *) (root_cell_node->value);
		}

		db_row_t * cell = root_cell, * new_cell = NULL;
		for(int j=1;j<response->cells[i].no_keys;j++, cell = new_cell)
		{
			snode_t * new_cell_node = skiplist_search(cell->cells, (WORD) response->cells[i].keys[j]);

			if(new_cell_node == NULL)
			{
				new_cell = create_db_row_schemaless2((WORD *) response->cells[i].keys + j, response->cells[i].no_keys - j,
						(WORD *) response->cells[i].columns, response->cells[i].no_columns,
						response->cells[i].last_blob, response->cells[i].last_blob_size, &(db->fastrandstate));

//				printf("Inserting cell %d (%" PRId64 ") into tree at level %d\n", i, response->cells[i].keys[j], j);

				skiplist_insert(cell->cells, (WORD) response->cells[i].keys[j], (WORD) new_cell, &(db->fastrandstate));

				break;
			}
			else
			{
				new_cell = (db_row_t *) (new_cell_node->value);

				assert(j < response->cells[i].no_keys - 1); // there s'dn't be 2 cells returned with the exact same keypath
			}
		}
	}

	int no_roots = 1;
	*start_row = HEAD(roots);
	for(*end_row=*start_row;NEXT(*end_row) != NULL;*end_row=NEXT(*end_row), no_roots++);

	assert(roots->no_items == no_roots);
//	assert(roots->no_items == *end_row - *start_row + 1);

	return roots->no_items;
}

db_row_t* get_db_rows_tree_from_read_response(range_read_response_message * response, remote_db_t * db)
// If a DB query returned multiple cells, accumulate them all in a single tree rooted at "result"
// (assumes this is a non-range query, i.e. all cells will have a common parent on the key path):
{
	if(response->no_cells == 0) // No results
		return NULL;

	db_row_t* result = create_db_row_schemaless2((WORD *) response->cells[0].keys, response->cells[0].no_keys,
			(WORD *) response->cells[0].columns, response->cells[0].no_columns,
			response->cells[0].last_blob, response->cells[0].last_blob_size, &(db->fastrandstate));

	for(int i=1;i<response->no_cells;i++) // We have a deeper result than 1
	{
		db_row_t * cell = result, * new_cell = NULL;

		assert(response->cells[i].keys[0] == (int64_t) result->key);

		for(int j=1;j<response->cells[i].no_keys;j++, cell = new_cell)
		{
			snode_t * new_cell_node = skiplist_search(cell->cells, (WORD) response->cells[i].keys[j]);

			if(new_cell_node == NULL)
			{
				new_cell = create_db_row_schemaless2((WORD *) response->cells[i].keys + j, response->cells[i].no_keys - j,
						(WORD *) response->cells[i].columns, response->cells[i].no_columns,
						response->cells[i].last_blob, response->cells[i].last_blob_size, &(db->fastrandstate));

//				printf("Inserting cell %d (%" PRId64 ") into tree at level %d\n", i, response->cells[i].keys[j], j);

				skiplist_insert(cell->cells, (WORD) response->cells[i].keys[j], (WORD) new_cell, &(db->fastrandstate));

				break;
			}
			else
			{
				new_cell = (db_row_t *) (new_cell_node->value);

				assert(j < response->cells[i].no_keys - 1); // there s'dn't be 2 cells returned with the exact same keypath
			}
		}
	}

//	print_long_row(result);

	return result;
}

db_row_t* remote_search_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key,
		uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	read_query * q = build_search_in_txn(primary_keys, no_primary_keys, table_key, txnid, get_nonce(db));
	int success = serialize_read_query(q, (void **) &tmp_out_buf, &len, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NULL;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_read_query(q, (char *) print_buff);
	printf("Sending read row query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_read_query(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NULL;
	}

	int ok_status = 0;
	db_row_t * result = NULL;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_RANGE_READ_RESPONSE);
		range_read_response_message * response = (range_read_response_message *) mc->replies[i];

#if CLIENT_VERBOSITY > 0
		to_string_range_read_response_message(response, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

		// If result returned multiple cells, accumulate them all in a single tree rooted at "result":
		result = get_db_rows_tree_from_read_response(response, db);
	}

	delete_msg_callback(mc->nonce, db);

	return result;
}


db_row_t* remote_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
														WORD table_key, uuid_t * txnid,
														remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	read_query * q = build_search_clustering_in_txn(primary_keys, no_primary_keys, clustering_keys, no_clustering_keys, table_key, txnid, get_nonce(db));
	int success = serialize_read_query(q, (void **) &tmp_out_buf, &len, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NULL;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_read_query(q, (char *) print_buff);
	printf("Sending read cell query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_read_query(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NULL;
	}

	int ok_status = 0;
	db_row_t * result = NULL;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_RANGE_READ_RESPONSE);
		range_read_response_message * response = (range_read_response_message *) mc->replies[i];

#if CLIENT_VERBOSITY > 0
		to_string_range_read_response_message(response, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

		// If result returned multiple cells, accumulate them all in a single tree rooted at "result":
		result = get_db_rows_tree_from_read_response(response, db);
	}

	delete_msg_callback(mc->nonce, db);

	return result;
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
	int success = serialize_range_read_query(q, (void **) &tmp_out_buf, &len, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_range_read_query(q, (char *) print_buff);
	printf("Sending range read row query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_range_read_query(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;
	int result = -1;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_RANGE_READ_RESPONSE);
		range_read_response_message * response = (range_read_response_message *) mc->replies[i];

#if CLIENT_VERBOSITY > 0
		to_string_range_read_response_message(response, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

		// If result returned multiple cells, accumulate them all in a forest of db_rows rooted at elements of list start_row->end_row:
		result = get_db_rows_forest_from_read_response(response, start_row, end_row, db);
	}

	delete_msg_callback(mc->nonce, db);

	return result;
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
	int success = serialize_range_read_query(q, (void **) &tmp_out_buf, &len, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_range_read_query(q, (char *) print_buff);
	printf("Sending range read cell query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_range_read_query(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;
	int result = -1;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_RANGE_READ_RESPONSE);
		range_read_response_message * response = (range_read_response_message *) mc->replies[i];

#if CLIENT_VERBOSITY > 0
		to_string_range_read_response_message(response, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

		// If result returned multiple cells, accumulate them all in a forest of db_rows rooted at elements of list start_row->end_row:
		result = get_db_rows_forest_from_read_response(response, start_row, end_row, db);
	}

	delete_msg_callback(mc->nonce, db);

	return result;
}

int remote_range_search_index_in_txn(int idx_idx, WORD start_idx_key, WORD end_idx_key,
								snode_t** start_row, snode_t** end_row,
								WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	assert (0); // Not supported; TO DO
	return 0;
}

int remote_read_full_table_in_txn(snode_t** start_row, snode_t** end_row,
									WORD table_key, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	range_read_query * q = build_wildcard_range_search_in_txn(table_key, txnid, get_nonce(db));
	int success = serialize_range_read_query(q, (void **) &tmp_out_buf, &len, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_range_read_query(q, (char *) print_buff);
	printf("Sending full table read query to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_range_read_query(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;
	int result = -1;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_RANGE_READ_RESPONSE);
		range_read_response_message * response = (range_read_response_message *) mc->replies[i];

#if CLIENT_VERBOSITY > 0
		to_string_range_read_response_message(response, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif

		// If result returned multiple cells, accumulate them all in a forest of db_rows rooted at elements of list start_row->end_row:
		result = get_db_rows_forest_from_read_response(response, start_row, end_row, db);
	}

	delete_msg_callback(mc->nonce, db);

#if DEBUG_BLOBS > 0
	printf("remote_read_full_table_in_txn: Returning %" PRId64 " [%d rows]:\n", (int64_t) table_key, result);

	for(snode_t * node = *start_row; node!=NULL; node=NEXT(node))
		print_long_row((db_row_t*) node->value);
#endif

	return result;
}

void remote_print_long_table(WORD table_key, remote_db_t * db)
{
	snode_t* start_row = NULL, * end_row = NULL;
	int no_items = remote_read_full_table_in_txn(&start_row, &end_row, table_key, NULL, db);

	printf("DB_TABLE: %" PRId64 " [%d rows]\n", (int64_t) table_key, no_items);

	for(snode_t * node = start_row; node!=NULL; node=NEXT(node))
		print_long_row((db_row_t*) node->value);
}


// Queue ops:

int remote_create_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_create_queue_in_txn(table_key, queue_id, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_queue_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_delete_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_delete_queue_in_txn(table_key, queue_id, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_queue_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_enqueue_in_txn(WORD * column_values, int no_cols, WORD blob, size_t blob_size, WORD table_key, WORD queue_id, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_enqueue_in_txn(column_values, no_cols, blob, blob_size, table_key, queue_id, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_queue_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_read_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
		int max_entries, int * entries_read, int64_t * new_read_head,
		snode_t** start_row, snode_t** end_row, uuid_t * txnid,
		remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_read_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, max_entries, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_queue_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	queue_query_message * response = NULL;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_QUEUE);
		response = (queue_query_message *) mc->replies[i];
		assert(response->msg_type == QUERY_TYPE_READ_QUEUE_RESPONSE);

#if CLIENT_VERBOSITY > 0
		to_string_queue_message(response, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
		break;
	}

    // Parse queue read response message to row list:

    skiplist_t * rows = create_skiplist_long();
    for(int i=0;i<response->no_cells;i++)
    {
    		db_row_t * row = create_db_row_schemaless2((WORD *) response->cells[i].keys, response->cells[i].no_keys,
    													(WORD *) response->cells[i].columns, response->cells[i].no_columns,
													response->cells[i].last_blob, response->cells[i].last_blob_size,
													&(db->fastrandstate)); // Note that cell versions are only kept on the server, we don't return them to the client
    		skiplist_insert(rows, (WORD) response->cells[i].keys[0], (WORD) row, &(db->fastrandstate));
    }

    *start_row = HEAD(rows);

    if((*start_row) != NULL)
    {
    		for(*end_row = *start_row;NEXT(*end_row) != NULL;*end_row = NEXT(*end_row));
    }

    *entries_read = response->no_cells;
	*new_read_head = response->queue_index;

	delete_msg_callback(mc->nonce, db);

	return response->status;
}

int remote_consume_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					int64_t new_consume_head, uuid_t * txnid, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_consume_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, new_consume_head, txnid, get_nonce(db));
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_queue_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
						remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_subscribe_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, NULL, get_nonce(db)); // txnid
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_queue_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
	    if(ack->status == CLIENT_ERR_SUBSCRIPTION_EXISTS)
	    {
	    		delete_msg_callback(mc->nonce, db);
	    		return CLIENT_ERR_SUBSCRIPTION_EXISTS;
	    }

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

    // Add local subscription on client:

    return subscribe_queue_client(consumer_id, shard_id, app_id, table_key, queue_id, callback, 1, db);
}

int remote_unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	queue_query_message * q = build_unsubscribe_queue_in_txn(consumer_id, shard_id, app_id, table_key, queue_id, NULL, get_nonce(db)); // txnid
	int success = serialize_queue_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_queue_message(q, (char *) print_buff);
	printf("Sending queue message to server %s: %s\n", rs->id, print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_queue_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
	    if(ack->status == CLIENT_ERR_NO_SUBSCRIPTION_EXISTS)
	    {
	    		delete_msg_callback(mc->nonce, db);
	    		return CLIENT_ERR_NO_SUBSCRIPTION_EXISTS;
	    }

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

    // Remove local subscription from client:

    return unsubscribe_queue_client(consumer_id, shard_id, app_id, table_key, queue_id, 1, db);
}

int remote_subscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
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
	printf("CLIENT: Subscriber %" PRId64 "/%" PRId64 "/%" PRId64 " subscribed queue %" PRId64 "/%" PRId64 " with callback %p\n",
					(int64_t) app_id, (int64_t) shard_id, (int64_t) consumer_id,
					(int64_t) table_key, (int64_t) queue_id, cs->callback);
#endif

	return status;
}

queue_callback * get_queue_client_callback(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, short use_lock, remote_db_t * db)
{
	queue_callback_args * qca = get_queue_callback_args(table_key, queue_id, app_id, shard_id, consumer_id, QUEUE_NOTIF_ENQUEUED);

	if(use_lock)
		pthread_mutex_lock(db->subscribe_lock);

	snode_t * subscription_node = skiplist_search(db->queue_subscriptions, (WORD) qca);

	if(use_lock)
		pthread_mutex_unlock(db->subscribe_lock);

	return (subscription_node != NULL)? ((queue_callback *) (subscription_node->value)):NULL;
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
	printf("CLIENT: Subscriber %" PRId64 "/%" PRId64 "/%" PRId64 " unsubscribed queue %" PRId64 "/%" PRId64 " with callback %p\n",
					(int64_t) app_id, (int64_t) shard_id, (int64_t) consumer_id,
					(int64_t) table_key, (int64_t) queue_id, cs->callback);
#endif

	return (callback != NULL)?0:CLIENT_ERR_NO_SUBSCRIPTION_EXISTS;
}


// Txn mgmt:

uuid_t * remote_new_txn(remote_db_t * db)
{
	uuid_t * txnid = NULL;
	unsigned len = 0;
	void * tmp_out_buf = NULL;
	int status = -2;

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NULL;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

	while(status == -2) // txnid already exists on server
	{
		txnid = new_client_txn(db, &(db->fastrandstate));
		txn_message * q = build_new_txn(txnid, get_nonce(db));
		int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

#if CLIENT_VERBOSITY > 0
		char print_buff[1024];
		to_string_txn_message(q, (char *) print_buff);
		printf("Sending new txn to server %s: %s\n", rs->id, print_buff);
#endif

		// Send packet to server and wait for reply:

		msg_callback * mc = NULL;
		success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
		assert(success == 0);
		free_txn_message(q);

		if(mc->no_replies < db->quorum_size)
		{
			fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
			delete_msg_callback(mc->nonce, db);
			return NULL;
		}

		int ok_status = 0;

		for(int i=0;i<mc->no_replies;i++)
		{
			assert(mc->reply_types[i] == RPC_TYPE_ACK);
			ack_message * ack = (ack_message *) mc->replies[i];
			if(ack->status == 0)
				ok_status++;

#if CLIENT_VERBOSITY > 0
			to_string_ack_message(ack, (char *) print_buff);
			printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
		}

		// TO DO: Update my_lc from each ACK reply received from servers:



		delete_msg_callback(mc->nonce, db);

		status = !(ok_status >= db->quorum_size);
	}

	assert(status == 0);

	return txnid;
}

int _remote_validate_txn(uuid_t * txnid, vector_clock * version, remote_server * rs_in, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	txn_message * q = build_validate_txn(txnid, version, get_nonce(db));
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len, 1, version);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (rs_in != NULL)?(rs_in):((remote_server *) (HEAD(db->servers))->value);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending validate txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_txn_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_validate_txn(uuid_t * txnid, remote_db_t * db)
{
	vector_clock * version = get_lc(db);

	int ret = _remote_validate_txn(txnid, get_lc(db), NULL, db);

	free_vc(version);

	return ret;
}

int _remote_abort_txn(uuid_t * txnid, remote_server * rs_in, remote_db_t * db)
{
	unsigned len = 0;
	void * tmp_out_buf = NULL;

	txn_message * q = build_abort_txn(txnid, get_nonce(db));
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len, 1, NULL);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (rs_in != NULL)?(rs_in):((remote_server *) (HEAD(db->servers))->value);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending abort txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_txn_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
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
	int success = serialize_txn_message(q, (void **) &tmp_out_buf, &len, 1, version);

	if(db->servers->no_items < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (rs_in != NULL)?(rs_in):((remote_server *) (HEAD(db->servers))->value);

#if CLIENT_VERBOSITY > 0
	char print_buff[1024];
	to_string_txn_message(q, (char *) print_buff);
	printf("Sending commit txn: %s\n", print_buff);
#endif

	// Send packet to server and wait for reply:

	msg_callback * mc = NULL;
	success = send_packet_wait_replies_sync(tmp_out_buf, len, q->nonce, &mc, db);
	assert(success == 0);
	free_txn_message(q);

	if(mc->no_replies < db->quorum_size)
	{
		fprintf(stderr, "No quorum (%d/%d replies received)\n", mc->no_replies, db->replication_factor);
		delete_msg_callback(mc->nonce, db);
		return NO_QUORUM_ERR;
	}

	int ok_status = 0;

	for(int i=0;i<mc->no_replies;i++)
	{
		assert(mc->reply_types[i] == RPC_TYPE_ACK);
		ack_message * ack = (ack_message *) mc->replies[i];
		if(ack->status == 0)
			ok_status++;

#if CLIENT_VERBOSITY > 0
		to_string_ack_message(ack, (char *) print_buff);
		printf("Got back response from server %s: %s\n", rs->id, print_buff);
#endif
	}

	delete_msg_callback(mc->nonce, db);

	return !(ok_status >= db->quorum_size);
}

int remote_commit_txn(uuid_t * txnid, remote_db_t * db)
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
		fprintf(stderr, "No quorum (%d/%d servers alive)\n", db->servers->no_items, db->replication_factor);
		return NO_QUORUM_ERR;
	}
	remote_server * rs = (remote_server *) (HEAD(db->servers))->value;

	vector_clock * commit_stamp = get_lc(db);

	int val_res = _remote_validate_txn(txnid, commit_stamp, rs, db);

#if (CLIENT_VERBOSITY > 1)
	printf("CLIENT: validate txn %s from server %s returned %d\n", uuid_str, rs->id, val_res);
#endif

	if(val_res == VAL_STATUS_COMMIT)
	{
		int persist_status = -2;
		while(persist_status != 0)
		{
			persist_status = _remote_persist_txn(txnid, commit_stamp, rs, db);

#if (CLIENT_VERBOSITY > 0)
			printf("CLIENT: persist txn %s from server %s returned %d\n", uuid_str, rs->id, persist_status);
#endif
		}

		int res = close_client_txn(*txnid, db); // Clear local cached txn state on client

#if (CLIENT_VERBOSITY > 1)
		printf("CLIENT: close txn %s returned %d\n", uuid_str, res);
#endif
	}
	else if(val_res == VAL_STATUS_ABORT)
	{
		int res = _remote_abort_txn(txnid, rs, db);

#if (CLIENT_VERBOSITY > 0)
		printf("CLIENT: abort txn %s from server %s returned %d\n", uuid_str, rs->id, res);
#endif
	}
	else
	{
		assert(0);
	}

	free_vc(commit_stamp);

	return val_res;
}

// Txn state handling client-side:

txn_state * get_client_txn_state(uuid_t txnid, remote_db_t * db)
{
	snode_t * txn_node = (snode_t *) skiplist_search(db->txn_state, (WORD) txnid);
#if (CLIENT_VERBOSITY > 0)
	char uuid_str[37];
	uuid_unparse_lower(txnid, uuid_str);
	printf("CLIENT: get_client_txn_state(%s): skiplist_search() returned: %p / %p\n", uuid_str, txn_node, (txn_node != NULL)? (txn_state *) txn_node->value : NULL);
#endif

	return (txn_node != NULL)? (txn_state *) txn_node->value : NULL;
}

uuid_t * new_client_txn(remote_db_t * db, unsigned int * seedptr)
{
	txn_state * ts = NULL, * previous = NULL;

#if (CLIENT_VERBOSITY > 0)
	printf("CLIENT: new_client_txn(): \n");
#endif

	while(ts == NULL)
	{
		ts = init_txn_state();
		previous = get_client_txn_state(ts->txnid, db);
		if(previous != NULL)
		{
#if (CLIENT_VERBOSITY > 0)
			char uuid_str[37];
			uuid_unparse_lower(ts->txnid, uuid_str);
			printf("CLIENT: new_client_txn(): Previous txn existed with the same txnid %s. Freeing previous state and retry-ing new uuid. \n", uuid_str);
#endif
			free_txn_state(ts);
			ts = NULL;
		}
	}

#if (CLIENT_VERBOSITY > 0)
	char uuid_str[37];
	uuid_unparse_lower(ts->txnid, uuid_str);
	printf("CLIENT: new_client_txn(): Inserting new txn state for txn uuid %s. \n", uuid_str);
#endif

	skiplist_insert(db->txn_state, (WORD) (ts->txnid), (WORD) ts, seedptr); // &

	return &(ts->txnid);
}

int close_client_txn(uuid_t txnid, remote_db_t * db)
{
	txn_state * ts = get_client_txn_state(txnid, db);
#if (CLIENT_VERBOSITY > 0)
	char uuid_str[37];
	uuid_unparse_lower(txnid, uuid_str);
	printf("CLIENT: close_client_txn(%s): get_client_txn_state() returned: %p\n", uuid_str, ts);
#endif
	if(ts == NULL)
		return -2; // No such txn

	skiplist_delete(db->txn_state, (WORD) txnid);
	free_txn_state(ts);

	return 0;
}

// Msg callback handling:

msg_callback * get_msg_callback(int64_t nonce, WORD client_id, void (*callback)(void *), int replication_factor)
{
	msg_callback * mc = (msg_callback *) malloc(sizeof(msg_callback) +
							2 * sizeof(pthread_mutex_t) + sizeof(pthread_cond_t) +
							replication_factor * (sizeof(void *) + sizeof(short) ));
	mc->client_id = client_id;
	mc->nonce = nonce;
	mc->lock = (pthread_mutex_t *) ((char *)mc + sizeof(msg_callback));
	mc->signal = (pthread_cond_t *) ((char *)mc + sizeof(msg_callback) + sizeof(pthread_mutex_t));
	pthread_mutex_init(mc->lock, NULL);
	pthread_cond_init(mc->signal, NULL);
	mc->callback = callback;

	mc->no_replies = 0;
	mc->reply_lock = (pthread_mutex_t *) ((char *)mc + sizeof(msg_callback) + sizeof(pthread_mutex_t) + sizeof(pthread_cond_t));
	pthread_mutex_init(mc->reply_lock, NULL);
//	mc->replies = (void **) ((char *)mc + sizeof(msg_callback) + 2 * sizeof(pthread_mutex_t) + sizeof(pthread_cond_t));
//	mc->reply_types = (short *) ((char *)mc + sizeof(msg_callback) + 2 * sizeof(pthread_mutex_t) + sizeof(pthread_cond_t) + replication_factor * sizeof(void *));

	mc->replies = (void **) malloc(replication_factor*sizeof(void *));
	mc->reply_types = (short *) malloc(replication_factor*sizeof(short));

	return mc;
}

int add_reply_to_msg_callback(void * reply, short reply_type, msg_callback * mc)
{
	int no_replies = 0;
	int ret = pthread_mutex_lock(mc->reply_lock);

	mc->replies[mc->no_replies] = reply;
	mc->reply_types[mc->no_replies] = reply_type;
	mc->no_replies++;
	no_replies = mc->no_replies;

	ret = pthread_mutex_unlock(mc->reply_lock);

	return no_replies;
}

void free_msg_callback(msg_callback * mc)
{
	free(mc->replies);
	free(mc->reply_types);
	free(mc);
}





