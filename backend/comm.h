/*
 * comm.h
 *
 *      Author: aagapi
 */

#ifndef BACKEND_COMM_H_
#define BACKEND_COMM_H_

#include "failure_detector/db_queries.h"
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <arpa/inet.h>
#include <string.h>
#include <sys/time.h>
#include <sys/select.h>
#include <netdb.h>

#define VERBOSE_RPC 0

#define BUFSIZE 4096

// Comm loop fctns:

int parse_message(void * rcv_buf, size_t rcv_msg_len, void ** out_msg, short * out_msg_type, long * nonce, short is_server, vector_clock ** vc);
int read_full_packet(int * sockfd, char * inbuf, size_t inbuf_size, int * msg_len, int (*handle_socket_close)(int * sockfd));
int sockaddr_cmp(WORD a1, WORD a2);

// Remote server mgmt fctns:

typedef struct remote_server
{
	char * hostname;
	int portno;
	int sockfd;
    pthread_mutex_t* sockfd_lock;
	struct sockaddr_in serveraddr;
	struct hostent *server;
	char id[256];
	char in_buf[BUFSIZE];
//	char out_buf[BUFSIZE];
} remote_server;

remote_server * get_remote_server(char *hostname, int portno);
void free_remote_server(remote_server * rs);
void free_remote_server_ptr(WORD ptr);


#endif /* BACKEND_COMM_H_ */
