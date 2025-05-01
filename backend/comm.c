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
 * comm.c
 *
 *      Author: aagapi
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "backend/comm.h"
#include "backend/log.h"

int parse_message_v1(void * rcv_buf, size_t rcv_msg_len, void ** out_msg, short * out_msg_type, int64_t * nonce, short is_server)
{
    write_query * wq;
    read_query * rq;
    range_read_query * rrq;
    queue_query_message * qq;
    ack_message * am;
    txn_message * tm;

    range_read_response_message * rrr;

#if (VERBOSE_RPC > 0)
    char print_buff[BUFSIZE];
#endif
    int status = 0;

    if(is_server) // RPCs received by server
    {
        status = deserialize_write_query(rcv_buf, rcv_msg_len, &wq);
        if(status == 0)
        {
#if (VERBOSE_RPC > 0)
            to_string_write_query(wq, (char *) print_buff);
            log_debug("Received write query: %s", print_buff);
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
            log_debug("Received read query: %s", print_buff);
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
            log_debug("Received range read query: %s", print_buff);
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
            log_debug("Received txn message: %s", print_buff);
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
            log_debug("Received queue query: %s", print_buff);
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
            log_debug("Received ack message: %s", print_buff);
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
            log_debug("Received ack message: %s", print_buff);
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
            log_debug("Received write query: %s", print_buff);
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
            log_debug("Received range read response: %s", print_buff);
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
            log_debug("Received queue read response: %s", print_buff);
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

int parse_message(void * rcv_buf, size_t rcv_msg_len, void ** out_msg, short * out_msg_type, short * is_gossip_message, int64_t * nonce, short is_server, vector_clock ** vc)
{
    read_query * rq;
    range_read_query * rrq;
    queue_query_message * qq;
    ack_message * am;
    txn_message * tm;

    range_read_response_message * rrr;

#if (VERBOSE_RPC > 0)
    char print_buff[BUFSIZE];
#endif
    int status = 0;

    if(is_server) // RPCs received by server
    {
        status = deserialize_server_message(rcv_buf, rcv_msg_len, out_msg, out_msg_type, vc);

        if(status == 0)
        {
            switch(*out_msg_type)
            {
                case RPC_TYPE_WRITE:
                {
                    write_query * wq = (write_query *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_write_query(wq, (char *) print_buff);
                    log_debug("Received write query: %s", print_buff);
#endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_READ:
                {
                    read_query * wq = (read_query *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_read_query(wq, (char *) print_buff);
                    log_debug("Received read query: %s", print_buff);
#endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_RANGE_READ:
                {
                    range_read_query * wq = (range_read_query *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_range_read_query(wq, (char *) print_buff);
                    log_debug("Received range read query: %s", print_buff);
#endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_QUEUE:
                {
                    queue_query_message * wq = (queue_query_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_queue_message(wq, (char *) print_buff);
                    log_debug("Received queue message: %s", print_buff);
#endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_TXN:
                {
                    txn_message * wq = (txn_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_txn_message(wq, (char *) print_buff);
                    log_debug("Received txn message: %s", print_buff);
#endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_GOSSIP_LISTEN:
                {
                    gossip_listen_message * wq = (gossip_listen_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_gossip_listen_msg(wq, (char *) print_buff);
                    log_debug("Received gossip listen message: %s", print_buff);
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
        status = deserialize_client_message(rcv_buf, rcv_msg_len, out_msg, out_msg_type, is_gossip_message, vc);

        if(status == 0)
        {
            if(*is_gossip_message)
            {
                return 0;
            }

            switch(*out_msg_type)
            {
                case RPC_TYPE_ACK:
                {
                    ack_message * wq = (ack_message *) *(out_msg);
    #if (VERBOSE_RPC > 0)
                    to_string_ack_message(wq, (char *) print_buff);
                    log_debug("Received ack message: %s", print_buff);
    #endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_WRITE:
                {
                    write_query * wq = (write_query *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_write_query(wq, (char *) print_buff);
                    log_debug("Received write query: %s", print_buff);
#endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_RANGE_READ_RESPONSE:
                {
                    range_read_response_message * wq = (range_read_response_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_range_read_response_message(wq, (char *) print_buff);
                    log_debug("Received range read response message: %s", print_buff);
#endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_QUEUE:
                {
                    queue_query_message * wq = (queue_query_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_queue_message(wq, (char *) print_buff);
                    log_debug("Received queue message: %s", print_buff);
#endif
                    *nonce = wq->nonce;
                    return 0;
                }
                case RPC_TYPE_TXN:
                {
                    txn_message * wq = (txn_message *) *(out_msg);
#if (VERBOSE_RPC > 0)
                    to_string_txn_message(wq, (char *) print_buff);
                    log_debug("Received txn message: %s", print_buff);
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

int read_full_packet(int * sockfd, char * inbuf, size_t inbuf_size, int * msg_len, int * statusp, int (*handle_socket_close)(int * sockfd, int * status))
{
    int announced_msg_len = -1;
    int read_buf_offset = 0;
    int status = 0;

    while(1) // Loop until reading complete packet:
    {
        assert(read_buf_offset < inbuf_size - sizeof(int));

        if(read_buf_offset == 0)
        {
            // Read msg len header from packet:

            bzero(inbuf, inbuf_size);
            *msg_len = -1;

            int size_len = read(*sockfd, inbuf, sizeof(int));

            if (size_len < 0)
            {
                log_error("ERROR reading from socket");
                continue;
            }
            else if (size_len == 0)
            {
                handle_socket_close(sockfd, statusp);
                status = 1;
                break;
            }

            announced_msg_len = *((int *)inbuf);

            *((int *)inbuf) = 0; // 0 back buffer

            read_buf_offset = 0;
        }

        if(announced_msg_len <= 0)
        {
            read_buf_offset = 0;
            continue;
        }

        *msg_len = read(*sockfd, inbuf + sizeof(int) + read_buf_offset, announced_msg_len - read_buf_offset);

#if COMM_VERBOSITY > 2
        log_debug("announced_msg_len=%d, msg_len=%d, read_buf_offset=%d", announced_msg_len, *msg_len, read_buf_offset);
#endif

        if (*msg_len < 0)
        {
                log_error("ERROR reading from socket");
            continue;
        }
        else if(*msg_len == 0) // client closed socket
        {
            handle_socket_close(sockfd, statusp);
            status = 1;
            break;
        }
        else if(*msg_len < announced_msg_len - read_buf_offset)
        {
            read_buf_offset += *msg_len;
            continue; // Continue reading socket until full packet length
        }

        break;
    }

    assert(status != 0 || announced_msg_len == *msg_len);

//    read_buf_offset = 0; // Reset

#if COMM_VERBOSITY > 2
    log_debug("server received %d / %d bytes", announced_msg_len, *msg_len);
#endif

    return status;
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

// Remote server struct fctns:

remote_server * get_remote_server(char *hostname, unsigned short portno,
        struct sockaddr_in serveraddr, struct sockaddr_in client_socket_addr,
        int serverfd, int do_connect, int is_rts)
{
    remote_server * rs = (remote_server *) malloc(sizeof(remote_server));
    bzero(rs, sizeof(remote_server));
    rs->status = NODE_DEAD;
    assert(serverfd != NULL_FD);
    memcpy(&(rs->client_socket_addr), &client_socket_addr, sizeof(struct sockaddr_in));

    if(serverfd > 0 || serverfd == OWN_FD) // For own node, use provided serveraddr and mark as live
    {
            memcpy(&(rs->serveraddr), &serveraddr, sizeof(struct sockaddr_in));
            rs->sockfd = serverfd;
            rs->server = gethostbyname(hostname);
            assert(rs->server != NULL);
            rs->status = NODE_LIVE;
    }
    else
    {
            rs->server = gethostbyname(hostname);
        if (rs->server == NULL)
        {
            log_error("ERROR, no such host %s", hostname);
            free_remote_server(rs);
            return NULL;
        }

        bzero((void *) &rs->serveraddr, sizeof(struct sockaddr_in));
        rs->serveraddr.sin_family = AF_INET;
        bcopy((char *)rs->server->h_addr_list[0], (char *)&(rs->serveraddr.sin_addr.s_addr), rs->server->h_length);
        rs->serveraddr.sin_port = htons(portno);

        if(do_connect)
        {
            rs->status = NODE_LIVE;

                rs->sockfd = socket(AF_INET, SOCK_STREAM, 0);
            if (rs->sockfd < 0)
            {
                log_error("ERROR opening socket!");
                free_remote_server(rs);
                return NULL;
            }

                int connect_success = -1;
                for(int connect_retries = 0; connect_success != 0 && connect_retries < MAX_CONNECT_RETRIES; connect_retries++)
                {
                    connect_success = connect(rs->sockfd, (struct sockaddr *) &rs->serveraddr, sizeof(struct sockaddr_in));
                    if(connect_success != 0)
                        sleep(1);
                }
            if (connect_success != 0)
            {
                log_error("get_remote_server: ERROR connecting to %s:%d", hostname, portno);
                rs->status = NODE_DEAD;
                rs->sockfd = -1;
            }
        }
    }

    rs->sockfd_lock = (pthread_mutex_t*) malloc (sizeof(pthread_mutex_t));
    pthread_mutex_init(rs->sockfd_lock, NULL);

    snprintf((char *) &rs->id, 262, "%s%s%d", hostname, is_rts?"/":":", portno);

    strncpy((char *) &(rs->hostname), hostname, strnlen(hostname, 256) + 1);
    rs->portno = portno;

    return rs;
}

int update_listen_socket(remote_server * rs, char *hostname, unsigned short portno, int do_connect)
{
    struct hostent * old_hostent = rs->server;

    rs->server = gethostbyname(hostname);
    if (rs->server == NULL)
    {
        log_error("ERROR, no such host %s", hostname);
        rs->server = old_hostent;
        return -1;
    }

    bzero((void *) &rs->serveraddr, sizeof(struct sockaddr_in));
    rs->serveraddr.sin_family = AF_INET;
    bcopy((char *)rs->server->h_addr_list[0], (char *)&(rs->serveraddr.sin_addr.s_addr), rs->server->h_length);
    rs->serveraddr.sin_port = htons(portno);

    if(do_connect)
    {
        int old_sockfd = rs->sockfd;
        rs->sockfd = socket(AF_INET, SOCK_STREAM, 0);
        if (rs->sockfd < 0)
        {
            log_error("update_listen_socket: ERROR opening socket!");
            rs->sockfd = old_sockfd;
            return -2;
        }

        int connect_success = -1;
        for(int connect_retries = 0; connect_success != 0 && connect_retries < MAX_CONNECT_RETRIES; connect_retries++)
        {
            connect_success = connect(rs->sockfd, (struct sockaddr *) &rs->serveraddr, sizeof(struct sockaddr_in));
            if(connect_success != 0)
                sleep(2);
        }
        if(connect_success != 0)
        {
            log_error("update_listen_socket: ERROR connecting to %s:%d", hostname, portno);
            rs->status = NODE_DEAD;
            rs->sockfd = -1;
        }
        else
        {
            log_debug("SERVER: Updated listen socket of %s/%s:%d (%s:%d) from %d to %d", rs->id, rs->hostname, rs->portno, hostname, portno, old_sockfd, rs->sockfd);
        }
    }

    log_debug("SERVER: Updating listen socket of %s/%s:%d (%d) to %s:%d", rs->id, rs->hostname, rs->portno, rs->sockfd, hostname, portno);

    snprintf((char *) &rs->id, 262, "%s:%d", hostname, portno);

    strncpy((char *) &(rs->hostname), hostname, strnlen(hostname, 256) + 1);

    rs->portno = portno;

    return 0;
}

int connect_remote_server(remote_server * rs)
{
    if(rs->sockfd > 0)
    {
        log_error("connect_remote_server: Skipping connect, server %s:%d already connected!", rs->hostname, rs->portno);
        return 0;
    }

    rs->sockfd = socket(AF_INET, SOCK_STREAM, 0);

    if (rs->sockfd < 0)
    {
        log_error("connect_remote_server: ERROR opening socket!");
        return -1;
    }

    int connect_success = -1;
    for(int connect_retries = 0; connect_success != 0 && connect_retries < MAX_CONNECT_RETRIES; connect_retries++)
    {
        connect_success = connect(rs->sockfd, (struct sockaddr *) &rs->serveraddr, sizeof(struct sockaddr_in));
        if(connect_success != 0)
            sleep(2);
    }

    if(connect_success != 0)
    {
        log_error("connect_remote_server: ERROR connecting to %s:%d", rs->hostname, rs->portno);
        rs->status = NODE_DEAD;
        rs->sockfd = -1;
    }

    return connect_success;
}

void free_remote_server(remote_server * rs)
{
    free(rs->sockfd_lock);
    free(rs);
}

void free_remote_server_ptr(WORD ptr)
{
    free_remote_server((remote_server *) ptr);
}


