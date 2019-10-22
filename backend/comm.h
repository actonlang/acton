/*
 * comm.h
 *
 *      Author: aagapi
 */

#ifndef BACKEND_COMM_H_
#define BACKEND_COMM_H_

#include "failure_detector/db_queries.h"

#define VERBOSE_RPC 1

int parse_message(void * rcv_buf, size_t rcv_msg_len, void ** out_msg, short * out_msg_type, short is_server);

#endif /* BACKEND_COMM_H_ */
