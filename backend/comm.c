/*
 * comm.c
 *
 *      Author: aagapi
 */

#include "comm.h"
#include <stdio.h>

int parse_message_v1(void * rcv_buf, size_t rcv_msg_len, void ** out_msg, short * out_msg_type, long * nonce, short is_server)
{
	write_query * wq;
	read_query * rq;
	range_read_query * rrq;
	queue_query_message * qq;
	ack_message * am;
	txn_message * tm;

	range_read_response_message * rrr;

#if (VERBOSE_RPC > 0)
	char print_buff[4096];
#endif
	int status = 0;

	if(is_server) // RPCs received by server
	{
		status = deserialize_write_query(rcv_buf, rcv_msg_len, &wq);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_write_query(wq, (char *) print_buff);
			printf("Received write query: %s\n", print_buff);
#endif
			*out_msg = (void *) wq;
			*out_msg_type = RPC_TYPE_WRITE;
			*nonce = wq->nonce;
			return 0;
		}

		status = deserialize_read_query(rcv_buf, rcv_msg_len, &rq);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_read_query(rq, (char *) print_buff);
			printf("Received read query: %s\n", print_buff);
#endif
			*out_msg = (void *) rq;
			*out_msg_type = RPC_TYPE_READ;
			*nonce = rq->nonce;
			return 0;
		}

		status = deserialize_range_read_query(rcv_buf, rcv_msg_len, &rrq);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_range_read_query(rrq, (char *) print_buff);
			printf("Received range read query: %s\n", print_buff);
#endif
			*out_msg = (void *) rrq;
			*out_msg_type = RPC_TYPE_RANGE_READ;
			*nonce = rrq->nonce;
			return 0;
		}

		status = deserialize_txn_message(rcv_buf, rcv_msg_len, &tm);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_txn_message(tm, (char *) print_buff);
			printf("Received txn message: %s\n", print_buff);
#endif
			*out_msg = (void *) tm;
			*out_msg_type = RPC_TYPE_TXN;
			*nonce = tm->nonce;
			return 0;
		}

		status = deserialize_queue_message(rcv_buf, rcv_msg_len, &qq);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_queue_message(qq, (char *) print_buff);
			printf("Received queue query: %s\n", print_buff);
#endif
			*out_msg = (void *) qq;
			*out_msg_type = RPC_TYPE_QUEUE;
			*nonce = qq->nonce;
			return 0;
		}

		status = deserialize_ack_message(rcv_buf, rcv_msg_len, &am);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_ack_message(am, (char *) print_buff);
			printf("Received ack message: %s\n", print_buff);
#endif
			*out_msg = (void *) am;
			*out_msg_type = RPC_TYPE_ACK;
			*nonce = am->nonce;

			return 0;
		}
	}
	else // RPCs received by client
	{
		status = deserialize_ack_message(rcv_buf, rcv_msg_len, &am);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_ack_message(am, (char *) print_buff);
			printf("Received ack message: %s\n", print_buff);
#endif
			*out_msg = (void *) am;
			*out_msg_type = RPC_TYPE_ACK;
			*nonce = am->nonce;

			return 0;
		}

		status = deserialize_write_query(rcv_buf, rcv_msg_len, &wq);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_write_query(wq, (char *) print_buff);
			printf("Received write query: %s\n", print_buff);
#endif
			*out_msg = (void *) wq;
			*out_msg_type = RPC_TYPE_READ_RESPONSE;
			*nonce = wq->nonce;

			return 0;
		}

		status = deserialize_range_read_response_message(rcv_buf, rcv_msg_len, &rrr);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_range_read_response_message(rrr, (char *) print_buff);
			printf("Received range read response: %s\n", print_buff);
#endif
			*out_msg = (void *) rrr;
			*out_msg_type = RPC_TYPE_RANGE_READ_RESPONSE;
			*nonce = rrr->nonce;

			return 0;
		}

		status = deserialize_queue_message(rcv_buf, rcv_msg_len, &qq);
		if(status == 0)
		{
#if (VERBOSE_RPC > 0)
			to_string_queue_message(qq, (char *) print_buff);
			printf("Received queue read response: %s\n", print_buff);
#endif
			*out_msg = (void *) qq;
			*out_msg_type = RPC_TYPE_QUEUE;
			*nonce = qq->nonce;

			return 0;
		}
	}

	*out_msg = NULL;
	*out_msg_type = -1;
	*nonce = -1;

	return 1;
}

int parse_message(void * rcv_buf, size_t rcv_msg_len, void ** out_msg, short * out_msg_type, long * nonce, short is_server)
{
	read_query * rq;
	range_read_query * rrq;
	queue_query_message * qq;
	ack_message * am;
	txn_message * tm;

	range_read_response_message * rrr;

#if (VERBOSE_RPC > 0)
	char print_buff[4096];
#endif
	int status = 0;

	if(is_server) // RPCs received by server
	{
		status = deserialize_server_message(rcv_buf, rcv_msg_len, out_msg, out_msg_type);

		if(status == 0)
		{
			switch(*out_msg_type)
			{
				case RPC_TYPE_WRITE:
				{
					write_query * wq = (write_query *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_write_query(wq, (char *) print_buff);
					printf("Received write query: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				case RPC_TYPE_READ:
				{
					read_query * wq = (read_query *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_read_query(wq, (char *) print_buff);
					printf("Received read query: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				case RPC_TYPE_RANGE_READ:
				{
					range_read_query * wq = (range_read_query *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_range_read_query(wq, (char *) print_buff);
					printf("Received range read query: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				case RPC_TYPE_QUEUE:
				{
					queue_query_message * wq = (queue_query_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_queue_message(wq, (char *) print_buff);
					printf("Received queue message: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				case RPC_TYPE_TXN:
				{
					txn_message * wq = (txn_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_txn_message(wq, (char *) print_buff);
					printf("Received txn message: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				default:
				{
					assert(0);
				}
			}
		}
	}
	else // RPCs received by client
	{
		status = deserialize_client_message(rcv_buf, rcv_msg_len, out_msg, out_msg_type);

		if(status == 0)
		{
			switch(*out_msg_type)
			{
				case RPC_TYPE_ACK:
				{
					ack_message * wq = (ack_message *) *(out_msg);
	#if (VERBOSE_RPC > 0)
					to_string_ack_message(wq, (char *) print_buff);
					printf("Received ack message: %s\n", print_buff);
	#endif
					*nonce = wq->nonce;
					return 0;
				}
				case RPC_TYPE_WRITE:
				{
					write_query * wq = (write_query *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_write_query(wq, (char *) print_buff);
					printf("Received write query: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				case RPC_TYPE_RANGE_READ_RESPONSE:
				{
					range_read_response_message * wq = (range_read_response_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_range_read_response_message(wq, (char *) print_buff);
					printf("Received range read response message: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				case RPC_TYPE_QUEUE:
				{
					queue_query_message * wq = (queue_query_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_queue_message(wq, (char *) print_buff);
					printf("Received queue message: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				case RPC_TYPE_TXN:
				{
					txn_message * wq = (txn_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
					to_string_txn_message(wq, (char *) print_buff);
					printf("Received txn message: %s\n", print_buff);
#endif
					*nonce = wq->nonce;
					return 0;
				}
				default:
				{
					assert(0);
				}
			}
		}
	}

	*out_msg = NULL;
	*out_msg_type = -1;
	*nonce = -1;

	return 1;
}


