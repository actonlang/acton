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
 * server.c
 *
 *      Author: aagapi
 */

// ActonDB Server:

#include "backend/db.h"
#include "backend/queue_callback.h"
#include "backend/failure_detector/db_queries.h"
#include "backend/failure_detector/fd.h"
#include "backend/comm.h"
#include "backend/fastrand.h"
#include "backend/log.h"

#include "netstring.h"
#include "yyjson.h"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <sys/select.h>
#include <fcntl.h>
#include <errno.h>
#include <argp.h>
#include <string.h>
#include <limits.h>
#include <signal.h>

#define LOGPFX "#ActDB# "

pid_t pid;

#define SERVER_BUFSIZE 128 * 1024 // (1024 * 1024)
#define PRINT_BUFSIZE 128 * 1024

#define UPDATE_LC_ON_GOSSIP
#define DO_HIERARCHICAL_CONNECT 0

char in_buf[SERVER_BUFSIZE];
char out_buf[SERVER_BUFSIZE];

int no_state_cols = 2;                  // 1;   // 4;
int no_state_primary_keys = 1;
int min_state_clustering_keys = 1;      // 0;   // 1;
int no_state_index_keys = 0;            // 1;

int no_queue_cols = 1;                  // 2;

#define ACTORS_TABLE    (WORD)0
#define MSGS_TABLE      (WORD)1
#define MSG_QUEUE       (WORD)2

//WORD state_table_key = (WORD) 0;
//WORD queue_table_key = (WORD) 1;


void error(char *msg) {
  log_error(msg);
}

#define SERVER_VERBOSITY 1

#define DEFAULT_DATA_PORT 32000
#define DEFAULT_GOSSIP_PORT 32001

#define RANDOM_NONCES
int64_t requests = 0;

int64_t _get_nonce(unsigned int * fastrandstate)
{
#ifdef RANDOM_NONCES
    unsigned int randno1, randno2;
    int64_t randlong;
    FASTRAND(fastrandstate, randno1);
    FASTRAND(fastrandstate, randno2);
    return ((int64_t) randno1 << 32) | ((int64_t) randno2 & 0xFFFFFFFFL);
#else
    return ++requests;
#endif
}

#define PROPOSAL_STATUS_NON_EXISTING 0
#define PROPOSAL_STATUS_ACTIVE 1
#define PROPOSAL_STATUS_ACCEPTED 2
#define PROPOSAL_STATUS_AMMENDED 3
#define PROPOSAL_STATUS_REJECTED 4

#define AGREED_PEERS 0
#define LOCAL_PEERS 1
#define CONNECTED_PEERS 2
#define CONNECTED_CLIENTS 3

typedef struct membership
{
    skiplist_t * connected_peers;
    skiplist_t * local_peers;
    skiplist_t * agreed_peers;
    skiplist_t * stable_peers;
    skiplist_t * connected_clients;
    skiplist_t * connected_client_sockets;

    vector_clock * view_id;

    int my_id;

    skiplist_t * outstanding_proposal;
    skiplist_t * outstanding_proposal_clients;
    int64_t outstanding_proposal_nonce;
    vector_clock * outstanding_view_id;
    int outstanding_proposal_acks;
    short proposal_status;
    skiplist_t * merged_responses;
    skiplist_t * merged_client_responses;
} membership;

int add_remote_server_to_list(remote_server * rs, skiplist_t * peer_list, unsigned int * seedptr);
int add_client_to_membership(struct sockaddr_in addr, int sockfd, char *hostname, int portno, skiplist_t * clients, unsigned int * seedptr);
int propose_local_membership(membership * m, vector_clock * my_vc, membership_agreement_msg ** amr, int64_t nonce, unsigned int * fastrandstate);

membership * get_membership(int my_id)
{
    membership * m = (membership *) malloc(sizeof(membership));

    m->connected_peers = create_skiplist(&sockaddr_cmp);
    m->local_peers = create_skiplist(&sockaddr_cmp);
    m->agreed_peers = create_skiplist(&sockaddr_cmp);
    m->stable_peers = create_skiplist(&sockaddr_cmp);
    m->connected_clients = create_skiplist(&sockaddr_cmp);
    m->view_id = NULL;
    m->my_id = my_id;

    m->outstanding_proposal = create_skiplist(&sockaddr_cmp);
    m->outstanding_proposal_clients = create_skiplist(&sockaddr_cmp);
    m->outstanding_view_id = NULL;
    m->outstanding_proposal_acks = 0;
    m->outstanding_proposal_nonce = -1;
    m->proposal_status = PROPOSAL_STATUS_NON_EXISTING;
    m->merged_responses = create_skiplist(&sockaddr_cmp);
    m->merged_client_responses = create_skiplist(&sockaddr_cmp);

    return m;
}

void free_membership(membership * m)
{
    skiplist_free(m->connected_peers);
    skiplist_free(m->local_peers);
    skiplist_free(m->agreed_peers);
    skiplist_free(m->stable_peers);
    skiplist_free(m->connected_clients);

    if(m->view_id != NULL)
        free_vc(m->view_id);

    if(m->outstanding_proposal != NULL)
        skiplist_free(m->outstanding_proposal);

    if(m->outstanding_proposal_clients != NULL)
        skiplist_free(m->outstanding_proposal_clients);

    if(m->outstanding_view_id != NULL)
        free_vc(m->outstanding_view_id);

    if(m->merged_responses != NULL)
        skiplist_free(m->merged_responses);

    if(m->merged_client_responses != NULL)
        skiplist_free(m->merged_client_responses);

    free(m);
}

typedef struct client_descriptor
{
    struct sockaddr_in addr;
    int sockfd;
    char id[256];
} client_descriptor;

client_descriptor * get_client_descriptor(struct sockaddr_in addr, int sockfd, char *hostname, int portno)
{
    client_descriptor * cd = (client_descriptor *) malloc(sizeof(struct client_descriptor));
    memcpy(&(cd->addr), &addr, sizeof(struct sockaddr_in));
    cd->sockfd = sockfd;
    snprintf((char *) &cd->id, 256, "%s:%d", hostname, portno);
    return cd;
}

void free_client_descriptor(client_descriptor * cd)
{
    free(cd);
}

int add_client_to_membership(struct sockaddr_in addr, int sockfd, char *hostname, int portno, skiplist_t * clients, unsigned int * seedptr)
{
    client_descriptor * cd = get_client_descriptor(addr, sockfd, hostname, portno);

    if(skiplist_search(clients, &(cd->addr)) != NULL)
    {
        log_info("Client address %s:%d was already added to membership!", hostname, portno);
        free_client_descriptor(cd);
        return -1;
    }

    int status = skiplist_insert(clients, &(cd->addr), cd, seedptr);

    if(status != 0)
    {
        log_error("Error adding client address %s:%d to membership!", hostname, portno);
        free_client_descriptor(cd);
        return -2;
    }

    return 0;
}

client_descriptor * lookup_client_by_fd(int fd, skiplist_t * clients)
{
    for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt))
    {
        client_descriptor * crt_cd = (client_descriptor *) crt->value;
        if(crt_cd->sockfd == fd)
            return crt_cd;
    }
    return NULL;
}

int create_state_schema(db_t * db, unsigned int * fastrandstate)
{
    int primary_key_idx = 0;
    int clustering_key_idxs[2];
    clustering_key_idxs[0]=1;
    clustering_key_idxs[1]=2;
    int index_key_idx=3;

    int * col_types = NULL;

    db_schema_t* db_schema = db_create_schema(col_types, no_state_cols + 1, &primary_key_idx, no_state_primary_keys, clustering_key_idxs, min_state_clustering_keys, &index_key_idx, no_state_index_keys);

    assert(db_schema != NULL && "Schema creation failed");

    // Create table:

    int ret = db_create_table(ACTORS_TABLE, db_schema, db, fastrandstate);

    log_info("%s - %s (%d)", "Create ACTORS_TABLE", ret==0?"OK":"FAILED", ret);

    ret = db_create_table(MSGS_TABLE, db_schema, db, fastrandstate);

    log_info("%s - %s (%d)", "Create MSGS_TABLE", ret==0?"OK":"FAILED", ret);

    return ret;
}

int create_queue_schema(db_t * db, unsigned int * fastrandstate)
{
    int * col_types = (int *) malloc((no_queue_cols + 1) * sizeof(int));
    col_types[0] = DB_TYPE_INT64;

    col_types[no_queue_cols] = DB_TYPE_BLOB; // Include blob

    int ret = create_queue_table(MSG_QUEUE, no_queue_cols + 1, col_types, db,  fastrandstate);

    log_info("%s - %s (%d)", "Create MSG_QUEUE table", ret==0?"OK":"FAILED", ret);

    return ret;
}



db_schema_t * get_schema(db_t * db, WORD table_key)
{
    snode_t * node = skiplist_search(db->tables, table_key);

    if(node == NULL)
        return NULL;

    db_table_t * table = (db_table_t *) (node->value);

    return table->schema;
}

// Write message handlers:

int get_ack_packet(int status, write_query * q,
                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    ack_message * ack = init_ack_message(get_cell_address(q->cell), status, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_ack_message(ack, (char *) print_buff);
    log_info("Sending ack message: %s", print_buff);
#endif

    int ret = serialize_ack_message(ack, snd_buf, snd_msg_len, vc);

    free_ack_message(ack);

    return ret;
}

int get_gossip_ack_packet(int status, gossip_listen_message * q,
                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    ack_message * ack = init_ack_message(NULL, status, NULL, q->nonce);

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_ack_message(ack, (char *) print_buff);
    log_info("Sending ack message: %s", print_buff);
#endif

    int ret = serialize_ack_message(ack, snd_buf, snd_msg_len, vc);

    free_ack_message(ack);

    return ret;
}

int handle_write_query(write_query * wq, db_t * db, unsigned int * fastrandstate)
{
    int total_cols = wq->cell->no_keys + wq->cell->no_columns;
    int total_cols_plus_blob = total_cols + ((wq->cell->last_blob_size > 0)?(1):(0));

    db_schema_t * schema = get_schema(db, (WORD) wq->cell->table_key);

    int no_clustering_keys = wq->cell->no_keys - schema->no_primary_keys;

    switch(wq->msg_type)
    {
        case RPC_TYPE_WRITE:
        {
            assert(wq->cell->no_columns > 0 || (wq->cell->last_blob != NULL && wq->cell->last_blob_size > 0));

            WORD * column_values = (WORD *) malloc(total_cols_plus_blob * sizeof(WORD));

            int j = 0;
            for(;j<wq->cell->no_keys;j++)
                column_values[j] = (WORD) wq->cell->keys[j];
            for(;j<total_cols;j++)
                column_values[j] = (WORD) wq->cell->columns[j-wq->cell->no_keys];

            if(wq->cell->last_blob_size > 0)
            {
                assert(total_cols_plus_blob == total_cols + 1);
                column_values[total_cols] = malloc(wq->cell->last_blob_size);
                memcpy(column_values[total_cols], wq->cell->last_blob, wq->cell->last_blob_size);
            }


            if(wq->txnid == NULL) // Write out of txn
                return db_insert_transactional(column_values, total_cols_plus_blob, no_clustering_keys, wq->cell->last_blob_size, wq->cell->version, (WORD) wq->cell->table_key, db, fastrandstate);
            else // Write in txn
                return db_insert_in_txn(column_values, total_cols_plus_blob, schema->no_primary_keys, no_clustering_keys, wq->cell->last_blob_size, (WORD) wq->cell->table_key, wq->txnid, db, fastrandstate);
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
                    return db_delete_cell_in_txn((WORD *) wq->cell->keys, schema->no_primary_keys, no_clustering_keys, (WORD) wq->cell->table_key, wq->txnid, db, fastrandstate);
                // TO DO: To support db_delete_by_index_in_txn (in RPCs and backend)
            }
        }
    }

    return 1;
}

vector_clock * get_empty_vc()
{
    return init_vc(0, NULL, NULL, 0);
}

vector_clock * get_local_vc(int my_id)
{
    int node_ids[] = {my_id};
    int64_t counters[] = {0};
    return init_vc(1, node_ids, counters, 0);
}

// Read (regular and range) message handlers:

int count_cells(db_row_t* result, int * max_depth)
{
    if(result == NULL)
        return 0;

    if(result->cells == NULL || result->cells->no_items == 0)
        return 1;

    *max_depth = *max_depth + 1;

    int no_cells = 0;
    for(snode_t * crt_cell = HEAD(result->cells); crt_cell != NULL; crt_cell = NEXT(crt_cell))
    {
        db_row_t * child = (db_row_t*) crt_cell->value;
        no_cells += count_cells(child, max_depth);
    }

    return no_cells;
}

cell * serialize_cells(db_row_t* result, cell * cells, int64_t table_key, int64_t * key_path, int depth, int no_schema_keys)
{
    if(result == NULL)
        return cells;

    key_path[depth-1] = (int64_t) result->key;

    if(result->cells == NULL || result->cells->no_items == 0)
    {
        assert(result->no_columns > 0);
        assert(depth >= no_schema_keys);

        if(result->last_blob_size <= 0)
            copy_cell(cells, table_key,
                    key_path, depth,
                    (int64_t *) result->column_array, result->no_columns,
                    NULL, 0,
                    result->version);
        else
            copy_cell(cells, table_key,
                    key_path, depth,
                    (int64_t *) result->column_array, result->no_columns - 1,
                    result->column_array[result->no_columns - 1], result->last_blob_size,
                    result->version);

        return cells + 1;
    }

//  log_debug("serialize_cells:");
//  print_long_row(result);

    cell * cells_ptr = cells;
    for(snode_t * crt_cell = HEAD(result->cells); crt_cell != NULL; crt_cell = NEXT(crt_cell))
    {
        db_row_t * child = (db_row_t*) crt_cell->value;
        cells_ptr = serialize_cells(child, cells_ptr, table_key, key_path, depth+1, no_schema_keys);
    }

    return cells_ptr;
}

int get_read_response_packet(db_row_t* result, read_query * q, db_schema_t * schema, void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    range_read_response_message * m = NULL;

    if(result == NULL)
    {
        m = init_range_read_response_message(NULL, 0, q->txnid, q->nonce);
    }
    else if(result->cells == NULL || result->cells->no_items == 0)
    // Return a single cell read result
    {
        assert(result->no_columns > 0);
        assert(result->no_columns >= schema->min_no_cols);
        assert(q->cell_address->keys[q->cell_address->no_keys - 1] == (int64_t) result->key);

        cell * c = NULL;
        if(result->last_blob_size <= 0)
        {
            c = init_cell(q->cell_address->table_key,
                            (int64_t *) &result->key, 1, // Result cell always points to last (inner-most) key of the query
                            (int64_t *) result->column_array, result->no_columns,
                            NULL, 0,
                            result->version);
        }
        else
        {
            c = init_cell(q->cell_address->table_key,
                            (int64_t *) &result->key, 1, // Result cell always points to last (inner-most) key of the query
                            (int64_t *) result->column_array, result->no_columns - 1,
                            result->column_array[result->no_columns - 1], result->last_blob_size,
                            result->version);
        }

        m = init_range_read_response_message(c, 1, q->txnid, q->nonce);
    }
    else
    // Return a multi-cell read result; traverse db_row downwards and get all child cells recursively:
    {
        int schema_keys = schema->no_primary_keys + schema->min_no_clustering_keys; // We only use this schema data for sanity checking of read back results
//      int no_keys = schema_keys - q->cell_address->no_keys + 1;

        int max_depth = 1;

        int no_results = count_cells(result, &max_depth);

        cell * cells = malloc(no_results * sizeof(cell));

        int64_t * key_path = (int64_t *) malloc(max_depth * sizeof(int64_t));

        cell * last_cell_ptr = serialize_cells(result, cells, q->cell_address->table_key, key_path, 1, schema_keys); // no_keys

        assert(last_cell_ptr - cells == no_results);

        m = init_range_read_response_message(cells, no_results, q->txnid, q->nonce);
    }

#if (VERBOSE_RPC > 0)
    char print_buff[PRINT_BUFSIZE];
    to_string_range_read_response_message(m, (char *) print_buff);
    log_info("Sending range read response message: %s", print_buff);
#endif

    int ret = serialize_range_read_response_message(m, snd_buf, snd_msg_len, vc);

    free_range_read_response_message(m);

    return ret;
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

remote_server * lookup_client_by_client_socket_addr(struct sockaddr_in * client_socket_addr, skiplist_t * clients)
{
    for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt))
    {
        remote_server * rs = (remote_server *) crt->value;
        if(sockaddr_cmp((WORD) client_socket_addr, (WORD) (&(rs->client_socket_addr))) == 0)
            return rs;
    }
    return NULL;
}

int handle_gossip_listen_message(gossip_listen_message * msg, client_descriptor * cd,
        membership * m, vector_clock * my_lc, membership_agreement_msg ** amr, unsigned int * fastrandstate)
{
    struct sockaddr_in dummy_serveraddr;
    remote_server * rs = get_remote_server(msg->node_description->hostname, msg->node_description->portno, dummy_serveraddr, cd->addr, DUMMY_FD, 0, 1);
    rs->status = NODE_LIVE;

    log_info("Adding client %s:%d/%d/%d to membership.", rs->hostname, msg->node_description->portno, msg->node_description->node_id, rs->portno);

    int status = add_remote_server_to_list(rs, m->connected_clients, fastrandstate);

    return propose_local_membership(m, my_lc, amr, msg->nonce, fastrandstate);
}

int get_range_read_response_packet(snode_t* start_row, snode_t* end_row, int no_results, range_read_query * q,
                                    db_schema_t * schema, void ** snd_buf, unsigned * snd_msg_len,
                                    vector_clock * vc)
{
    int schema_keys = schema->no_primary_keys + schema->min_no_clustering_keys; // We only use this schema data for sanity checking of read back results
//  int no_keys = schema_keys - q->start_cell_address->no_keys + 1;
    range_read_response_message * m = NULL;

    if(no_results == 0)
    {
        m = init_range_read_response_message(NULL, 0, q->txnid, q->nonce);
    }
    else
    {
        assert(start_row != NULL);

        int max_range_depth = 0;
        int no_cells = 0, i=0;
        for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
        {
            db_row_t* result = (db_row_t* ) crt_row->value;
//          print_long_row(result);
            int max_depth = 1;
            no_cells += count_cells(result, &max_depth);
            max_range_depth = (max_depth > max_range_depth)?max_depth:max_range_depth;
        }

        cell * cells = malloc(no_cells * sizeof(cell));

        int64_t * key_path = (int64_t *) malloc(max_range_depth * sizeof(int64_t));

        i=0;
        cell * last_cell_ptr = cells;
        for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
        {
            db_row_t* result = (db_row_t* ) crt_row->value;
            last_cell_ptr = serialize_cells(result, last_cell_ptr, q->start_cell_address->table_key, key_path, 1, schema_keys); // no_keys
        }

        assert(last_cell_ptr - cells == no_cells);

        m = init_range_read_response_message(cells, no_cells, q->txnid, q->nonce);
    }

#if (VERBOSE_RPC > 0)
        char print_buff[PRINT_BUFSIZE];
        to_string_range_read_response_message(m, (char *) print_buff);
        log_info("Sending range read response message: %s", print_buff);
#endif

        int ret = serialize_range_read_response_message(m, snd_buf, snd_msg_len, vc);

        free_range_read_response_message(m);

        return ret;
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

// Queue message handlers:

int get_queue_ack_packet(int status, queue_query_message * q,
                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    ack_message * ack = init_ack_message(q->cell_address, status, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_ack_message(ack, (char *) print_buff);
    log_info("Sending queue ack message: %s", print_buff);
#endif

    int ret = serialize_ack_message(ack, snd_buf, snd_msg_len, vc);

    free_ack_message(ack);

    return ret;
}

int get_queue_read_response_packet(snode_t* start_row, snode_t* end_row, int no_results,
                                    int64_t new_read_head, int status, db_schema_t * schema,
                                    queue_query_message * q,
                                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    queue_query_message * m = NULL;

    if(no_results == 0)
    {
        m = init_read_queue_response(q->cell_address, NULL, 0, q->app_id, q->shard_id, q->consumer_id, q->group_id, new_read_head, (short) status, q->txnid, q->nonce);
    }
    else
    {
        assert(start_row != NULL);

        cell * cells = malloc(no_results * sizeof(cell));

        int i=0;
        int64_t prev_id = -1;
        for(snode_t * crt_row = start_row; i<no_results; crt_row = NEXT(crt_row), i++)
        {
            db_row_t* result = (db_row_t* ) crt_row->value;
            int64_t id = (int64_t) result->key;
            assert(i==0 || prev_id == (id - 1));
            prev_id = id;
            if(result->last_blob_size <= 0)
                copy_cell(cells+i, q->cell_address->table_key,
                        (int64_t *) &result->key, 1,
                        (int64_t *) result->column_array, result->no_columns,
                        NULL, 0,
                        result->version);
            else
                copy_cell(cells+i, q->cell_address->table_key,
                        (int64_t *) &result->key, 1,
                        (int64_t *) result->column_array, result->no_columns - 1,
                        result->column_array[result->no_columns - 1], result->last_blob_size,
                        result->version);
        }

        m = init_read_queue_response(q->cell_address, cells, no_results, q->app_id, q->shard_id, q->consumer_id, q->group_id, new_read_head, (short) status, q->txnid, q->nonce);
    }

#if (VERBOSE_RPC > 0)
        char print_buff[1024];
        to_string_queue_message(m, (char *) print_buff);
        log_info("Sending read queue response message: %s", print_buff);
#endif

        int ret = serialize_queue_message(m, snd_buf, snd_msg_len, 0, vc);

        free_queue_message(m);

        return ret;
}

int handle_create_queue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
    if(q->txnid == NULL) // Create queue out of txn
        return create_queue((WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], NULL, 1, db, fastrandstate);
    else // Create queue in txn
        return create_queue_in_txn((WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], q->txnid, db, fastrandstate);
}

int handle_delete_queue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
    if(q->txnid == NULL)
        return delete_queue((WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], NULL, 1, db, fastrandstate);
    else
        return delete_queue_in_txn((WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], q->txnid, db, fastrandstate);
}

int handle_subscribe_queue(queue_query_message * q, int * clientfd, int64_t * prev_read_head, int64_t * prev_consume_head, db_t * db, unsigned int * fastrandstate)
{
    if(q->txnid != NULL) // Create queue out of txn
    {
        assert(0); // Subscriptions in txns are not supported yet
        return 1;
    }
    else
        return register_remote_subscribe_queue((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id, (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], (WORD) q->group_id,
                                        clientfd, prev_read_head, prev_consume_head, 1, db, fastrandstate);
}

int handle_unsubscribe_queue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
    if(q->txnid != NULL) // Create queue out of txn
    {
        assert(0); // Unsubscriptions in txns are not supported yet
        return 1;
    }
    else
        return register_remote_unsubscribe_queue((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id, (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], (WORD) q->group_id, 1, db);
}

int handle_enqueue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
    assert(q->no_cells > 0 && q->cells != NULL);

    int status = -1;

    for(int i=0;i<q->no_cells;i++)
    {
        int total_cols = q->cells[i].no_keys + q->cells[i].no_columns;
        int total_cols_plus_blob = total_cols + ((q->cells[i].last_blob_size > 0)?(1):(0));

        int64_t * column_values = (int64_t *) malloc(total_cols_plus_blob * sizeof(int64_t));

        int j = 0;
        for(;j<q->cells[i].no_keys;j++)
            column_values[j] = q->cells[i].keys[j];
        for(;j<total_cols;j++)
            column_values[j] = q->cells[i].columns[j-q->cells[i].no_keys];

        if(q->cells[i].last_blob_size > 0)
        {
            assert(total_cols_plus_blob == total_cols + 1);
            column_values[total_cols] = (int64_t) malloc(q->cells[i].last_blob_size);
            memcpy((WORD) column_values[total_cols], q->cells[i].last_blob, q->cells[i].last_blob_size);
        }

        // Below will automatically trigger remote consumer notifications on the queue, either immediately or upon txn commit:
        if(q->txnid == NULL) // Enqueue out of txn
            status = enqueue((WORD *) column_values, total_cols_plus_blob, q->cells[i].last_blob_size, (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], 1, db, fastrandstate);
        else // Enqueue in txn
            status = enqueue_in_txn((WORD *) column_values, total_cols_plus_blob, q->cells[i].last_blob_size, (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], q->txnid, db, fastrandstate);

        if(status != 0)
            break;
    }

    return status;
}

int handle_read_queue(queue_query_message * q,
                        int * entries_read, int64_t * new_read_head, vector_clock ** prh_version,
                        snode_t** start_row, snode_t** end_row, db_schema_t ** schema,
                        db_t * db, unsigned int * fastrandstate)
{
    *schema = get_schema(db, (WORD) q->cell_address->table_key);

    if(q->txnid == NULL) // Read queue out of txn
        return read_queue((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id,
                            (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0], q->queue_index,
                            entries_read, new_read_head, prh_version,
                            start_row, end_row, 1, db);
    else // Read queue in txn
        return read_queue_in_txn((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id,
                                    (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0],
                                    (int) q->queue_index, entries_read, new_read_head,
                                    start_row, end_row, q->txnid, db, fastrandstate);
}

int handle_consume_queue(queue_query_message * q, db_t * db, unsigned int * fastrandstate)
{
    if(q->txnid == NULL) // Consume queue out of txn
        return consume_queue((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id,
                            (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0],
                            q->queue_index, db);
    else // Consume queue in txn
        return consume_queue_in_txn((WORD) q->consumer_id, (WORD) q->shard_id, (WORD) q->app_id,
                                    (WORD) q->cell_address->table_key, (WORD) q->cell_address->keys[0],
                                    q->queue_index, q->txnid, db, fastrandstate);
}

// Txn messages handlers:

int get_txn_ack_packet(int status, txn_message * q,
                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    ack_message * ack = init_ack_message(NULL, status, q->txnid, q->nonce);

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_ack_message(ack, (char *) print_buff);
    log_info("Sending txn ack message: %s", print_buff);
#endif

    int ret = serialize_ack_message(ack, snd_buf, snd_msg_len, vc);

    free_ack_message(ack);

    return ret;
}


int handle_new_txn(txn_message * q, db_t * db, unsigned int * fastrandstate)
{
    assert(q->txnid != NULL);

#if (MULTI_THREADED == 1)
    pthread_mutex_lock(db->txn_state_lock);
#endif

    txn_state * ts = get_txn_state(q->txnid, db);

    if(ts != NULL)
    {
#if (MULTI_THREADED == 1)
        pthread_mutex_unlock(db->txn_state_lock);
#endif

        return DUPLICATE_TXN; // txnid already exists on server
    }

    ts = init_txn_state();

    memcpy(&ts->txnid, q->txnid, sizeof(uuid_t));

    skiplist_insert(db->txn_state, (WORD) ts->txnid, (WORD) ts, fastrandstate);

#if (MULTI_THREADED == 1)
    pthread_mutex_unlock(db->txn_state_lock);
#endif

    return 0;
}

int handle_validate_txn(txn_message * q, db_t * db, unsigned int * fastrandstate)
{
    assert(q->txnid != NULL);
    assert(q->version != NULL);

    return validate_txn(q->txnid, q->version, db);
}

int handle_commit_txn(txn_message * q, db_t * db, unsigned int * fastrandstate)
{
    assert(q->txnid != NULL);

    txn_state * ts = get_txn_state(q->txnid, db);

    // Make sure the txn has the right commit stamp (it c'd be that the current server missed the previous validation packet so the version was not set then):

    if(ts == NULL)
        return NO_SUCH_TXN; // txnid doesn't exist on server

    set_version(ts, q->version);

    return persist_txn(ts, db, fastrandstate);
}

int handle_abort_txn(txn_message * q, db_t * db, unsigned int * fastrandstate)
{
    assert(q->txnid != NULL);

    return abort_txn(q->txnid, db);
}


int handle_socket_nop(int * childfd, int * status)
{
    struct sockaddr_in address;
    int addrlen;
    getpeername(*childfd, (struct sockaddr*)&address,
                (socklen_t*)&addrlen);
    log_info("Host disconnected, ip %s, port %d, status=%d, fd, NOP %d" ,
              inet_ntoa(address.sin_addr) , ntohs(address.sin_port), *status, *childfd);

    *status = NODE_DEAD;

    return 0;
}

int handle_socket_close(int * childfd, int * status)
{
    struct sockaddr_in address;
    int addrlen;
    getpeername(*childfd, (struct sockaddr*)&address,
                (socklen_t*)&addrlen);
    log_info("Host disconnected, ip %s, port %d, old_status=%d, closing fd %d" ,
              inet_ntoa(address.sin_addr) , ntohs(address.sin_port), *status, *childfd);

    //Close the socket and mark as -1 for reuse:
    close(*childfd);
    *childfd = -1;

    *status = NODE_DEAD;

    return 0;
}

int add_remote_server_to_list(remote_server * rs, skiplist_t * peer_list, unsigned int * seedptr)
{
    snode_t * snode = skiplist_search(peer_list, &rs->serveraddr);

    if(snode != NULL)
    {
        log_info("Server address %s:%d was already added to membership, marking it as live!", rs->hostname, rs->portno);
        remote_server * existing_rs = (remote_server *) snode->value;
        existing_rs->status = NODE_LIVE;
        // Update client socket address:
        memcpy(&(existing_rs->client_socket_addr), &(rs->client_socket_addr), sizeof(struct sockaddr_in));
        return -1;
    }

    int status = skiplist_insert(peer_list, &rs->serveraddr, rs, seedptr);

    if(status != 0)
    {
        log_error("Error adding server address %s:%d to membership!", rs->hostname, rs->portno);
        assert(0);
        return -2;
    }

    return 0;
}

int add_remote_server_to_membership(remote_server * rs, membership * m, short list, unsigned int * seedptr)
{
    switch(list)
    {
        case AGREED_PEERS:
            return add_remote_server_to_list(rs, m->agreed_peers, seedptr);
        case LOCAL_PEERS:
            return add_remote_server_to_list(rs, m->local_peers, seedptr);
        case CONNECTED_PEERS:
            return add_remote_server_to_list(rs, m->connected_peers, seedptr);
        case CONNECTED_CLIENTS:
            return add_remote_server_to_list(rs, m->connected_clients, seedptr);
    }

    return -1;
}

int add_peer_to_membership(char *hostname, unsigned short portno, struct sockaddr_in serveraddr, int serverfd, int do_connect, membership * m, short list, remote_server ** rs, unsigned int * seedptr)
{
    struct sockaddr_in dummy_serveraddr;
    *rs = get_remote_server(hostname, portno, serveraddr, dummy_serveraddr, serverfd, do_connect, 0);

    if(rs == NULL)
    {
        log_debug("ERROR: Failed joining server %s:%d (it looks down)!", hostname, portno);
            return 1;
    }

    int status = add_remote_server_to_membership(*rs, m, list, seedptr);

    if(status != 0)
    {
            assert(0);
            free_remote_server(*rs);
            *rs = NULL;
    }

    return status;
}


int handle_client_message(int childfd, int msg_len, db_t * db, membership * m, skiplist_t * clients, unsigned int * fastrandstate, vector_clock * my_lc, int my_id)
{
    void * tmp_out_buf = NULL, * q = NULL;
    unsigned snd_msg_len;
    short msg_type;
    short is_gossip_message;
    db_schema_t * schema;
    int64_t nonce = -1;
    membership_agreement_msg * amr = NULL;

    vector_clock * lc_read = NULL;
    int status = parse_message(in_buf + sizeof(int), msg_len, &q, &msg_type, &is_gossip_message, &nonce, 1, &lc_read);

    if(status != 0)
    {
            log_error( "ERROR decoding client request");
            return -1;
    }

    if(lc_read != NULL)
    {
            // If we were multi-threaded, we'd have to protect this snippet:

#if (VERBOSE_RPC > 2)
        char msg_buf[1024];
        log_debug("SERVER: Received client message with LC %s.", to_string_vc(lc_read, msg_buf));
        log_debug("SERVER: My LC before update is %s.", to_string_vc(my_lc, msg_buf));
#endif

            update_vc(my_lc, lc_read);
            increment_vc(my_lc, my_id);

            free_vc(lc_read);

#if (VERBOSE_RPC > 2)
        log_debug("SERVER: Updated local LC to %s.", to_string_vc(my_lc, msg_buf));
#endif
    }

    switch(msg_type)
    {
            case RPC_TYPE_WRITE:
            {
                status = handle_write_query((write_query *) q, db, fastrandstate);
                if(status != 0)
                {
                    log_error("handle_write_query returned %d!", status);
                    assert(0);
                }
                vector_clock * vc = (((write_query *) q)->txnid != NULL)?(copy_vc(my_lc)):(NULL);
                status = get_ack_packet(status, (write_query *) q, &tmp_out_buf, &snd_msg_len, vc);
                if(vc != NULL)
                    free_vc(vc);
                free_write_query((write_query *) q);
                break;
            }
            case RPC_TYPE_READ:
            {
                db_row_t* result = handle_read_query((read_query *) q, &schema, db, fastrandstate);
                vector_clock * vc = (((read_query *) q)->txnid != NULL)?(copy_vc(my_lc)):(NULL);
                status = get_read_response_packet(result, (read_query *) q, schema, &tmp_out_buf, &snd_msg_len, vc);
                if(vc != NULL)
                    free_vc(vc);
                free_read_query((read_query *) q);
                break;
            }
            case RPC_TYPE_RANGE_READ:
            {
                snode_t * start_row = NULL, * end_row = NULL;
                vector_clock * vc = (((range_read_query *) q)->txnid != NULL)?(copy_vc(my_lc)):(NULL);
                int no_results = handle_range_read_query((range_read_query *) q, &start_row, &end_row, &schema, db, fastrandstate);
                status = get_range_read_response_packet(start_row, end_row, no_results, (range_read_query *) q, schema, &tmp_out_buf, &snd_msg_len, vc);
                if(vc != NULL)
                    free_vc(vc);
                free_range_read_query((range_read_query *) q);
                break;
            }
            case RPC_TYPE_QUEUE:
            {
                queue_query_message * qm = (queue_query_message *) q;
                vector_clock * vc = (qm->txnid != NULL)?(copy_vc(my_lc)):(NULL);

                switch(qm->msg_type)
                {
                    case QUERY_TYPE_CREATE_QUEUE:
                    {
                        status = handle_create_queue(qm, db, fastrandstate);
//                      assert(status == 0);
                        status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len, vc);
                        break;
                    }
                    case QUERY_TYPE_DELETE_QUEUE:
                    {
                        status = handle_delete_queue(qm, db, fastrandstate);
                        assert(status == 0);
                        status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len, vc);
                        break;
                    }
                    case QUERY_TYPE_SUBSCRIBE_QUEUE:
                    {
                        int64_t prev_read_head = -1, prev_consume_head = -1;
                        status = handle_subscribe_queue(qm, &childfd, &prev_read_head, &prev_consume_head, db, fastrandstate);
//                      assert(status == 0);
                        status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len, vc);
                        break;
                    }
                    case QUERY_TYPE_UNSUBSCRIBE_QUEUE:
                    {
                        status = handle_unsubscribe_queue(qm, db, fastrandstate);
                        assert(status == 0);
                        status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len, vc);
                        break;
                    }
                    case QUERY_TYPE_ADD_QUEUE_TO_GROUP:
                    {
                        // This is handled automatically, should not be called explicitly
                        assert(0);
                        break;
                    }
                    case QUERY_TYPE_ENQUEUE:
                    {
                        status = handle_enqueue(qm, db, fastrandstate);
                        assert(status == 0);
                        status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len, vc);

                        break;
                    }
                    case QUERY_TYPE_READ_QUEUE:
                    {
                        int entries_read = 0;
                        int64_t new_read_head = -1;
                        vector_clock * prh_version = NULL;
                        snode_t * start_row = NULL, * end_row = NULL;
                        db_schema_t * schema = NULL;
                        status = handle_read_queue(qm, &entries_read, &new_read_head, &prh_version,
                                                        &start_row, &end_row, &schema, db, fastrandstate);
                        if(status != QUEUE_STATUS_READ_COMPLETE && status != QUEUE_STATUS_READ_INCOMPLETE && status != DB_ERR_NO_CONSUMER)
                        {
                            log_error("Unexpected status returned by handle_read_queue(): %d", status);
                            assert(0);
                        }
                        status = get_queue_read_response_packet(start_row, end_row, entries_read, new_read_head, status, schema, qm, &tmp_out_buf, &snd_msg_len, vc);

                        break;
                    }
                    case QUERY_TYPE_CONSUME_QUEUE:
                    {
                        status = handle_consume_queue(qm, db, fastrandstate);
//                      assert(status == (int) qm->queue_index);
                        status = get_queue_ack_packet(status, qm, &tmp_out_buf, &snd_msg_len, vc);
                        break;
                    }
                    default:
                    {
                        assert(0);
                    }
                }

                if(vc != NULL)
                    free_vc(vc);
                free_queue_message(qm);

                break;
            }
            case RPC_TYPE_TXN:
            {
                txn_message * tm = (txn_message *) q;
                assert(tm->txnid != NULL);
                vector_clock * vc = copy_vc(my_lc);

                switch(tm->type)
                {
                    case DB_TXN_BEGIN:
                    {
                        status = handle_new_txn(tm, db, fastrandstate);
                        assert(status == 0 || status == DUPLICATE_TXN);
                        status = get_txn_ack_packet(status, tm, &tmp_out_buf, &snd_msg_len, vc);

                        break;
                    }
                    case DB_TXN_VALIDATION:
                    {
                        status = handle_validate_txn(tm, db, fastrandstate);
                        if(status != VAL_STATUS_COMMIT)
                            log_warn("BACKEND: handle_validate_txn() returned %d\n", status);
                        assert(status == VAL_STATUS_COMMIT || status == VAL_STATUS_ABORT || status == VAL_STATUS_ABORT_SCHEMA || status == NO_SUCH_TXN);
                        status = get_txn_ack_packet(status, tm, &tmp_out_buf, &snd_msg_len, vc);

                        break;
                    }
                    case DB_TXN_COMMIT:
                    {
                        status = handle_commit_txn(tm, db, fastrandstate);
                        if(status != 0)
                            log_warn("BACKEND: handle_commit_txn() returned %d\n", status);
                        status = get_txn_ack_packet(status, tm, &tmp_out_buf, &snd_msg_len, vc);

                        break;
                    }
                    case DB_TXN_ABORT:
                    {
                        status = handle_abort_txn(tm, db, fastrandstate);
                        assert(status == 0);
                        status = get_txn_ack_packet(status, tm, &tmp_out_buf, &snd_msg_len, vc);

                        break;
                    }
                }

                if(vc != NULL)
                    free_vc(vc);
                free_txn_message(tm);

                break;
            }
            case RPC_TYPE_GOSSIP_LISTEN:
            {
                client_descriptor * cd = lookup_client_by_fd(childfd, clients);
                int status = handle_gossip_listen_message((gossip_listen_message *) q, cd, m, copy_vc(my_lc),
                                                            &amr, fastrandstate);

                assert(get_gossip_ack_packet(status, (gossip_listen_message *) q, &tmp_out_buf, &snd_msg_len, NULL) == 0);
            free_gossip_listen_msg(q);
                break;
            }
            case RPC_TYPE_ACK:
            {
                assert(0); // S'dn't happen currently
                break;
            }
        default:
        {
            assert(0);
        }
    }

    assert(status == 0);

    int n = write(childfd, tmp_out_buf, snd_msg_len);
    if (n < 0)
      log_error("ERROR writing to socket");

    // There might be a view notification to send to client:
    if(amr != NULL)
    {
        assert(serialize_membership_agreement_msg(amr, &tmp_out_buf, &snd_msg_len) == 0);
        free_membership_agreement(amr);
        n = write(childfd, tmp_out_buf, snd_msg_len);
        if (n < 0)
            log_error("ERROR writing to socket");
    }

    free(tmp_out_buf);

    return 0;
}

// Gossip message handling:

int get_join_packet(int status, int rack_id, int dc_id, char * hostname, unsigned short portno, int64_t nonce,
                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    membership_agreement_msg * jm = get_membership_join_msg(status, rack_id, dc_id, hostname, portno, nonce, vc);

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_membership_agreement_msg(jm, (char *) print_buff);
    log_info("Sending Join message: %s", print_buff);
#endif

    int ret = serialize_membership_agreement_msg(jm, snd_buf, snd_msg_len);

    free_membership_agreement(jm);

    return ret;
}

int get_agreement_propose_packet(int status, membership_state * membership, int64_t nonce,
                                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    membership_agreement_msg * amr = get_membership_propose_msg(status, membership, nonce, vc);

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_membership_agreement_msg(amr, (char *) print_buff);
    log_info("Sending Membership Propose message: %s", print_buff);
#endif

    int ret = serialize_membership_agreement_msg(amr, snd_buf, snd_msg_len);

    free_membership_agreement(amr);

    return ret;
}

int get_agreement_response_packet(int status, membership_state * membership, membership_agreement_msg * am,
                                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    membership_agreement_msg * amr = get_membership_response_msg(status, membership, am->nonce, copy_vc(vc));

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_membership_agreement_msg(amr, (char *) print_buff);
    log_info("Sending Membership Response message: %s", print_buff);
#endif

    int ret = serialize_membership_agreement_msg(amr, snd_buf, snd_msg_len);

    free_membership_agreement(amr);

    return ret;
}

int get_agreement_notify_packet(int status, membership_state * membership, membership_agreement_msg * am,
                                membership_agreement_msg ** amr,    void ** snd_buf, unsigned * snd_msg_len,
                                vector_clock * vc, vector_clock * prev_vc)
{
    *amr = get_membership_notify_msg(status, membership, am->nonce, copy_vc(vc));

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_membership_agreement_msg(*amr, (char *) print_buff);
    log_info("Sending Membership Notify message: %s", print_buff);
#endif

    int ret = serialize_membership_agreement_msg(*amr, snd_buf, snd_msg_len);

    return ret;
}

int get_agreement_notify_ack_packet(int status, membership_agreement_msg * am,
                                    void ** snd_buf, unsigned * snd_msg_len, vector_clock * vc)
{
    membership_agreement_msg * amr = get_membership_notify_ack_msg(status, am->nonce, copy_vc(vc));

#if (VERBOSE_RPC > 0)
    char print_buff[1024];
    to_string_membership_agreement_msg(amr, (char *) print_buff);
    log_info("Sending Membership Notify ACK message: %s", print_buff);
#endif

    int ret = serialize_membership_agreement_msg(amr, snd_buf, snd_msg_len);

    free_membership_agreement(amr);

    return ret;
}

membership_state * get_membership_state_from_server_list(skiplist_t * servers, skiplist_t * clients, vector_clock * my_lc)
{
    if(servers->no_items == 0)
        return NULL;

    node_description * nds = (node_description *) malloc(servers->no_items * sizeof(node_description));
    node_description * client_nds = (node_description *) malloc(clients->no_items * sizeof(node_description));

    int i = 0;
    for(snode_t * crt = HEAD(servers); crt!=NULL; crt = NEXT(crt), i++)
    {
        remote_server * rs = (remote_server *) crt->value;
        copy_node_description(nds+i, rs->status, get_node_id((struct sockaddr *) &(rs->serveraddr)), 0, 0, rs->hostname, rs->portno);
    }

    int j = 0;
    for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt), j++)
    {
        remote_server * rs = (remote_server *) crt->value;
        copy_node_description(client_nds+j, rs->status, get_node_id((struct sockaddr *) &(rs->serveraddr)), 0, 0, rs->hostname, rs->portno);
    }

    return init_membership_state(servers->no_items, nds, clients->no_items, client_nds, my_lc);
}

int no_live_nodes(skiplist_t * list)
{
    int no_nodes = 0;

    for(snode_t * crt = HEAD(list); crt!=NULL; crt = NEXT(crt))
    {
        remote_server * rs = (remote_server *) crt->value;

        if(rs->status == NODE_LIVE)
            no_nodes++;
    }

    return no_nodes;
}

int no_live_or_unknown_nodes(skiplist_t * list)
{
    int no_nodes = 0;

    for(snode_t * crt = HEAD(list); crt!=NULL; crt = NEXT(crt))
    {
        remote_server * rs = (remote_server *) crt->value;

        if(rs->status == NODE_LIVE)
            no_nodes++;
    }

    return no_nodes;
}

int is_min_live_node(int id, skiplist_t * list)
{
    if(list->no_items == 0)
        return -2; // empty list

    int min_id = INT_MAX, id_in_list = 0;

    for(snode_t * crt = HEAD(list); crt!=NULL; crt = NEXT(crt))
    {
        remote_server * rs = (remote_server *) crt->value;
        int node_id = get_node_id((struct sockaddr *) &(rs->serveraddr));
        if(node_id == id)
            id_in_list = 1;

        if(rs->status == NODE_LIVE)
        {
            min_id = (node_id < min_id)?node_id:min_id;
        }
    }

    if(!id_in_list)
        return -1; // Node not in list

    if(min_id == id)
        return 1;

    return 0;
}

int mark_live(membership * m, int sender_id)
{
    for(snode_t * crt = HEAD(m->local_peers); crt!=NULL; crt = NEXT(crt))
    {
        remote_server * rs = (remote_server *) crt->value;

        int node_id = get_node_id((struct sockaddr *) &(rs->serveraddr));
        if(node_id == sender_id)
        {
            rs->status = NODE_LIVE;

            return 0;
        }
    }

    return 1;
}

int mark_dead(membership * m, int sender_id)
{
    for(snode_t * crt = HEAD(m->local_peers); crt!=NULL; crt = NEXT(crt))
    {
        remote_server * rs = (remote_server *) crt->value;

        int node_id = get_node_id((struct sockaddr *) &(rs->serveraddr));
        if(node_id == sender_id)
        {
            rs->status = NODE_DEAD;

            return 0;
        }
    }

    return 1;
}

int send_join_message(int rack_id, int dc_id, char * hostname, unsigned short portno, vector_clock * my_vc, remote_server * dest_rs, unsigned int * fastrandstate)
{
    void * tmp_out_buf = NULL;
    unsigned snd_msg_len;

    if(dest_rs->status != NODE_LIVE || dest_rs->sockfd <= 0)
    {
#if (VERBOSE_RPC > 0)
        char msg_buf[1024];
        log_warn("SERVER: Not sending JOIN request to %s, as its status is %d!", dest_rs->id, dest_rs->status);
#endif

            return 1;
    }

    int status = get_join_packet(0, rack_id, dc_id, hostname, portno, _get_nonce(fastrandstate), &tmp_out_buf, &snd_msg_len, copy_vc(my_vc));

    assert(status == 0);

    int n = write(dest_rs->sockfd, tmp_out_buf, snd_msg_len);

    if (n < 0)
        error("ERROR writing to socket");

    free(tmp_out_buf);

    return 0;
}


int propose_local_membership(membership * m, vector_clock * my_vc, membership_agreement_msg ** amr, int64_t nonce, unsigned int * fastrandstate)
{
    void * tmp_out_buf = NULL;
    unsigned snd_msg_len;
    int status = 0, skip_proposal = 0;
    char msg_buf[1024];

    if(no_live_nodes(m->local_peers) <= 1)
    {
#if (VERBOSE_RPC > 0)
        log_warn("SERVER: Skipping proposing new view, as no other nodes are live in local membership!");
#endif
        skip_proposal = 1;
    }

    if(is_min_live_node(m->my_id, m->local_peers) != 1)
    {
#if (VERBOSE_RPC > 0)
        log_info("SERVER: Skipping proposing new view, as I am not the min live node!");
#endif
        skip_proposal = 1;
    }

    if(skip_proposal)
    {
        // In this case, there won't be an agreement proposal, but we must still notify the clients of the current local view if
        // this came from a listen_to_gossip(), for it to learn the client memberships in case we are the only server in the system:
        if(amr != NULL)
        {
            *amr = get_membership_notify_msg(0,
                                        get_membership_state_from_server_list(skiplist_clone(m->local_peers, fastrandstate),
                                                                             skiplist_clone(m->connected_clients, fastrandstate),
                                                                             copy_vc(my_vc)),
                                        nonce, copy_vc(my_vc));
#if (VERBOSE_RPC > 0)
        to_string_membership_agreement_msg(*amr, (char *) msg_buf);
        log_info("Sending Membership Notify message to clients: %s", msg_buf);
#endif
        }
        return SKIP_PROPOSAL_STATUS;
    }

    if(m->outstanding_view_id != NULL)
    {
#if (VERBOSE_RPC > 0)
        log_warn("SERVER: Proposing new view, aborting already outstanding view proposal %s!",
                            to_string_vc(m->outstanding_view_id, msg_buf));
#endif
        skiplist_free(m->outstanding_proposal);
        skiplist_free(m->outstanding_proposal_clients);
        free_vc(m->outstanding_view_id);
    }

    m->outstanding_proposal = skiplist_clone(m->local_peers, fastrandstate);
    m->outstanding_proposal_clients = skiplist_clone(m->connected_clients, fastrandstate);
    m->outstanding_view_id = copy_vc(my_vc);
    m->outstanding_proposal_nonce = _get_nonce(fastrandstate);
    m->proposal_status = PROPOSAL_STATUS_ACTIVE;
    m->outstanding_proposal_acks = 0;
    m->merged_responses = skiplist_clone(m->local_peers, fastrandstate); // init merged responses for server list to "my response"
    m->merged_client_responses = skiplist_clone(m->connected_clients, fastrandstate); // init merged responses for client list to "my response"

    status = get_agreement_propose_packet(0, get_membership_state_from_server_list(m->outstanding_proposal, m->outstanding_proposal_clients, copy_vc(m->outstanding_view_id)),
                                                m->outstanding_proposal_nonce, &tmp_out_buf, &snd_msg_len, copy_vc(m->outstanding_view_id));


    for(snode_t * crt = HEAD(m->local_peers); crt!=NULL; crt = NEXT(crt))
    {
        remote_server * rs = (remote_server *) crt->value;

#if (VERBOSE_RPC > 0)
        log_info("SERVER: propose_local_membership: Evaluating node (%s, %d, %d, %d), my_id = %d..", rs->id, rs->status, rs->sockfd, get_node_id((struct sockaddr *) &(rs->serveraddr)), m->my_id);
#endif

        if(rs->status == NODE_LIVE && rs->sockfd > 0 && get_node_id((struct sockaddr *) &(rs->serveraddr)) != m->my_id) // skip myself, and nodes that are "down" in membership
        {
#if (VERBOSE_RPC > 0)
            log_info("SERVER: Sending proposal!");
#endif
            int n = write(rs->sockfd, tmp_out_buf, snd_msg_len);

            if (n < 0)
              error("ERROR writing to socket");

            m->outstanding_proposal_acks++;
        }
    }

    free(tmp_out_buf);

    return 0;
}

int merge_membership_agreement_msg_to_list(membership_agreement_msg * ma, skiplist_t * merged_list, membership * m, vector_clock * my_lc, unsigned int * fastrandstate)
{
    int memberships_differ = 0;
    struct sockaddr_in dummy_serveraddr;

    for(int i=0;i<ma->membership->no_nodes;i++)
    {
        node_description nd = ma->membership->membership[i];

        remote_server * rs = get_remote_server(nd.hostname, nd.portno, dummy_serveraddr, dummy_serveraddr, DUMMY_FD, 0, 0);
        rs->status = nd.status;

        snode_t * node = skiplist_search(merged_list, &rs->serveraddr);

        if(node == NULL) // I just learned about this node
        {
#if (VERBOSE_RPC > 0)
            char msg_buf[1024];
            log_info("SERVER: merge_membership_agreement_msg_to_list: Learned about node %s from proposal, status=%d.", rs->id, rs->status);
#endif

            int status = connect_remote_server(rs);

            if(status != 0)
            {
                rs->status = NODE_DEAD;
                rs->sockfd = -1;
                if(nd.status == NODE_LIVE)
                    memberships_differ = 1;
            }

            status = add_remote_server_to_list(rs, m->local_peers, fastrandstate);
            status = add_remote_server_to_list(rs, merged_list, fastrandstate);

            assert(status == 0);
        }
        else
        {
            free_remote_server(rs);

            remote_server * rs_local = (remote_server *) node->value;

            if(rs_local->status != nd.status) // if proposed server status is different than local one, and proposed clock is newer, use proposed status
            {
                int64_t local_counter = get_component_vc(my_lc, nd.node_id);
                int64_t proposed_counter = get_component_vc(ma->vc, nd.node_id);

                if((proposed_counter > local_counter)
                        || (proposed_counter == -1 && local_counter == -1)) // -1 is for client (RTS membership entries), which do not participate in the vector_clock version
                {
#if (VERBOSE_RPC > 0)
                    log_info("SERVER: merge_membership_agreement_msg_to_list: Updating status of node %s from %d to %d, because local_counter=%" PRId64 ", proposed_counter=%" PRId64 ".", rs->id, rs_local->status, nd.status, local_counter, proposed_counter);
#endif

                    rs_local->status = nd.status;
                }
                else
                {
#if (VERBOSE_RPC > 0)
                    log_info("SERVER: merge_membership_agreement_msg_to_list: Requesting membership ammend, because for node %s, local_status=%d, proposed_status=%d, local_counter=%" PRId64 ", proposed_counter=%" PRId64 ".", rs->id, rs_local->status, nd.status, local_counter, proposed_counter);
#endif

                    memberships_differ = 1;
                }
            }
        }
    }

    return memberships_differ;
}

int merge_membership_agreement_msg_to_client_list(membership_agreement_msg * ma, skiplist_t * merged_list, membership * m, vector_clock * my_lc, unsigned int * fastrandstate)
{
    int memberships_differ = 0;
    struct sockaddr_in dummy_serveraddr;
    int reverse_connect_to_clients = 0;

    for(int i=0;i<ma->membership->no_client_nodes;i++)
    {
        node_description nd = ma->membership->client_membership[i];

        remote_server * rs = get_remote_server(nd.hostname, nd.portno, dummy_serveraddr, dummy_serveraddr, DUMMY_FD, 0, 1);
        rs->status = nd.status;

        snode_t * node = skiplist_search(merged_list, &rs->serveraddr);

        if(node == NULL) // I just learned about this client
        {
#if (VERBOSE_RPC > 0)
            char msg_buf[1024];
            log_info("SERVER: merge_membership_agreement_msg_to_list: Learned about client node %s from proposal, status=%d.", rs->id, rs->status);
#endif

            int status = 0;
            if(reverse_connect_to_clients)
            {
                status = connect_remote_server(rs);

                if(status != 0)
                {
                    rs->status = NODE_DEAD;
                    rs->sockfd = -1;
                    if(nd.status == NODE_LIVE)
                        memberships_differ = 1;
                }
            }
            else
            {
                memberships_differ = 1;
            }

            status = add_remote_server_to_list(rs, m->connected_clients, fastrandstate);
            assert(status == 0);
            status = add_remote_server_to_list(rs, merged_list, fastrandstate);
            assert(status == 0);
        }
        else
        {
            free_remote_server(rs);

            remote_server * rs_local = (remote_server *) node->value;

            if(rs_local->status != nd.status) // if proposed server status is different than local one, and proposed clock is newer, use proposed status
            {
                int64_t local_counter = get_component_vc(my_lc, nd.node_id);
                int64_t proposed_counter = get_component_vc(ma->vc, nd.node_id);

                if((proposed_counter > local_counter)
                    || (proposed_counter == -1 && local_counter == -1)) // -1 is for client (RTS membership entries), which do not participate in the vector_clock version
                {
#if (VERBOSE_RPC > 0)
                    log_info("SERVER: merge_membership_agreement_msg_to_list: Updating status of node %s from %d to %d, because local_counter=%" PRId64 ", proposed_counter=%" PRId64 ".", rs->id, rs_local->status, nd.status, local_counter, proposed_counter);
#endif

                    rs_local->status = nd.status;
                }
                else
                {
#if (VERBOSE_RPC > 0)
                    log_info("SERVER: merge_membership_agreement_msg_to_list: Requesting membership ammend, because for node %s, local_status=%d, proposed_status=%d, local_counter=%" PRId64 ", proposed_counter=%" PRId64 ".", rs->id, rs_local->status, nd.status, local_counter, proposed_counter);
#endif

                    memberships_differ = 1;
                }
            }
        }
    }

    return memberships_differ;
}

int handle_agreement_propose_message(membership_agreement_msg * ma, membership_state ** merged_membership, membership * m, db_t * db, vector_clock * my_lc, vector_clock * prev_lc, unsigned int * fastrandstate)
{
#if (VERBOSE_RPC > 0)
    char msg_buf[1024];
    log_info("SERVER: Received new view proposal %s!", to_string_membership_agreement_msg(ma, msg_buf));
#endif

    if(compare_vc(m->view_id, ma->vc) > 0)
    {
#if (VERBOSE_RPC > 0)
        log_info("SERVER: Rejecting proposed view %s because it is older than my installed view %s!",
                            to_string_vc(m->view_id, msg_buf), to_string_vc(ma->vc, msg_buf));
#endif

        *merged_membership = NULL;

        return PROPOSAL_STATUS_REJECTED;
    }

    // Copy local view to merged list:
    skiplist_t * merged_list = skiplist_clone(m->local_peers, fastrandstate);
    skiplist_t * client_merged_list = skiplist_clone(m->connected_clients, fastrandstate);

    // Merge it with proposed view:
    int memberships_differ = merge_membership_agreement_msg_to_list(ma, merged_list, m, prev_lc, fastrandstate);
    int client_memberships_differ = merge_membership_agreement_msg_to_client_list(ma, client_merged_list, m, prev_lc, fastrandstate);

    *merged_membership = get_membership_state_from_server_list(merged_list, client_merged_list, my_lc);

    return (memberships_differ | client_memberships_differ)?PROPOSAL_STATUS_AMMENDED:PROPOSAL_STATUS_ACCEPTED;
}

int handle_agreement_response_message(membership_agreement_msg * ma, membership_state ** merged_membership, membership * m, db_t * db, vector_clock * my_lc, vector_clock * prev_vc, unsigned int * fastrandstate)
{
#if (VERBOSE_RPC > 0)
    char msg_buf[1024];
    log_info("SERVER: Received agreement response message %s, outstanding_proposal_acks=%d!", to_string_membership_agreement_msg(ma, msg_buf), m->outstanding_proposal_acks);
#endif

    *merged_membership = NULL;

    if(m->outstanding_view_id == NULL)
    {
#if (VERBOSE_RPC > 0)
        log_warn("SERVER: Received Agreement Response message, but no outstanding view proposal is active!");
#endif
        return -2;
    }

    if(m->outstanding_proposal_nonce != ma->nonce)
    {
#if (VERBOSE_RPC > 0)
        log_warn("SERVER: Received Agreement Response message, but nonce (%" PRId64 ") does not match outstanding view proposal nonce (%" PRId64 ")!",
                            ma->nonce, m->outstanding_proposal_nonce);
#endif
        return -1;
    }

    m->outstanding_proposal_acks--;

    assert(m->outstanding_proposal_acks >=0);

    if(ma->ack_status == PROPOSAL_STATUS_ACCEPTED) // view identical with proposed one
    {
            if(m->outstanding_proposal_acks == 0)
            {
                if(m->proposal_status == PROPOSAL_STATUS_ACTIVE)
                    m->proposal_status = PROPOSAL_STATUS_ACCEPTED; // my proposal was accepted

                *merged_membership = get_membership_state_from_server_list(m->merged_responses, m->merged_client_responses, my_lc);
                assert(*merged_membership != NULL);
            }
    }
    else if(ma->ack_status == PROPOSAL_STATUS_AMMENDED)
    {
            int memberships_differ = merge_membership_agreement_msg_to_list(ma, m->merged_responses, m, prev_vc, fastrandstate);
            int client_memberships_differ = merge_membership_agreement_msg_to_client_list(ma, m->merged_client_responses, m, prev_vc, fastrandstate);

//          assert(memberships_differ);

            if(m->proposal_status != PROPOSAL_STATUS_REJECTED)
                m->proposal_status = PROPOSAL_STATUS_AMMENDED; // my proposal was ammended

            if(m->proposal_status == PROPOSAL_STATUS_AMMENDED && m->outstanding_proposal_acks == 0)
            {
                *merged_membership = get_membership_state_from_server_list(m->merged_responses, m->merged_client_responses, my_lc);
                assert(*merged_membership != NULL);
            }
    }
    else
    {
            assert(ma->ack_status == PROPOSAL_STATUS_REJECTED);

            m->proposal_status = PROPOSAL_STATUS_REJECTED; // my proposal was rejected
    }

    if((m->proposal_status == PROPOSAL_STATUS_AMMENDED || m->proposal_status == PROPOSAL_STATUS_ACCEPTED) && m->outstanding_proposal_acks == 0)
        assert(*merged_membership != NULL);

    return m->proposal_status;
}

int auto_update_group_queue_subscriptions(membership * m, db_t * db, unsigned int * fastrandstate)
{
    char msg_buf[4096];
    log_debug("SERVER: Auto-updating actor queue mapping to groups.");

    // Update queue group membership from gossip state:

    for(snode_t * crt_rts = HEAD(m->connected_clients); crt_rts!=NULL; crt_rts = NEXT(crt_rts))
    {
        remote_server * rts = (remote_server *) (crt_rts->value);

        int rts_id = get_node_id((struct sockaddr *) &(rts->serveraddr));

        int update_status = set_bucket_status(db->queue_groups, (WORD) rts_id, rts->status, &get_group_state_key, &get_group_state_live_field);

        if(update_status < 0) // We just learned of this group; add it
        {
            log_debug("SERVER: auto_update_group_queue_subscriptions: Bucket %d not found, adding it", rts_id);

            group_state * new_group = get_group((WORD) rts_id);

            add_bucket(db->queue_groups, new_group, &get_group_state_key, &get_group_state_live_field, fastrandstate);
        }
    }

    if(db->queue_groups->buckets->no_items < 1) // 2
    {
        log_debug("SERVER: We only have %d groups in total, nothing to do", db->queue_groups->buckets->no_items);
        return 0;
    }

    // Update cached version of queue group subscriptions for all queues in all DB tables:

    for(snode_t * crt_table = HEAD(db->tables); crt_table!=NULL; crt_table = NEXT(crt_table))
    {
        db_table_t * table = (db_table_t *) (crt_table->value);

        for(snode_t * crt_queue = HEAD(table->queues); crt_queue!=NULL; crt_queue = NEXT(crt_queue))
        {
            db_row_t * row = (db_row_t *) (crt_queue->value);

            WORD result = get_buckets_for_object(db->queue_groups, (int) row->key, db->queue_group_replication_factor,
                                        &get_group_state_key, &get_group_state_live_field,
                                        fastrandstate);

            if(db->queue_group_replication_factor > 1 && row->group_subscriptions != NULL)
            {
            	skiplist_free(row->group_subscriptions);
            }

            row->group_subscriptions = result;

            if(row->group_subscriptions != NULL)
            	create_headers_for_group_subscribers(row, db, fastrandstate);
        }
    }

    return 0;
}

int install_agreed_view(membership_agreement_msg * ma, membership * m, vector_clock * my_lc, db_t * db, unsigned int * fastrandstate)
{
#if (VERBOSE_RPC > -1)
    char msg_buf[1024];
#endif

    if(compare_vc(m->view_id, ma->vc) > 0)
    {
#if (VERBOSE_RPC > -1)
        log_info("SERVER: Skipping installing notified view %s because it is older than my installed view %s!",
                            to_string_vc(ma->vc, msg_buf), to_string_vc(m->view_id, msg_buf));
#endif

        return 1;
    }

    // If servers are already connected in local membership, copy their entries from there into agreed membership. Otherwise create new connections for them:

    skiplist_t * new_membership = create_skiplist(&sockaddr_cmp);

    int local_view_disagrees = 0;
    struct sockaddr_in dummy_serveraddr;

    for(int i=0;i<ma->membership->no_nodes;i++)
    {
        node_description nd = ma->membership->membership[i];

        remote_server * rs = get_remote_server(nd.hostname, nd.portno, dummy_serveraddr, dummy_serveraddr, DUMMY_FD, 0, 0);
        rs->status = nd.status;

        snode_t * node = skiplist_search(m->local_peers, &rs->serveraddr);

        if(node == NULL) // I just learned about this node
        {
            int status = connect_remote_server(rs);

            if(status != 0)
            {
                rs->status = NODE_DEAD;
                rs->sockfd = -1;
                local_view_disagrees = 1;
            }

            status = add_remote_server_to_list(rs, m->local_peers, fastrandstate);
            assert(status == 0);

            status = add_remote_server_to_list(rs, new_membership, fastrandstate);
            assert(status == 0);
        }
        else
        {
            free_remote_server(rs);

            remote_server * rs_local = (remote_server *) node->value;

            if(rs_local->status != nd.status) // if proposed server status is different than local one, and proposed clock is newer, use proposed status
            {
                int64_t local_counter = get_component_vc(my_lc, nd.node_id);
                int64_t proposed_counter = get_component_vc(ma->vc, nd.node_id);

                if(proposed_counter > local_counter)
                {
                    rs_local->status = nd.status;
                }
                else
                {
                    local_view_disagrees = 1;
                }
            }

            int status = add_remote_server_to_list(rs_local, new_membership, fastrandstate);
            assert(status == 0);
        }
    }

    if(m->agreed_peers != NULL)
    {
        skiplist_free(m->agreed_peers);
    }

    m->agreed_peers = new_membership;

    update_or_replace_vc(&(m->view_id), ma->vc);

    if(db->enable_auto_queue_group_subscriptions)
    {
        auto_update_group_queue_subscriptions(m, db, fastrandstate);
    }

#if (VERBOSE_RPC > -1)
    log_info("SERVER: Installed new agreed view %s, local_view_disagrees=%d",
                    to_string_membership_agreement_msg(ma, msg_buf),
                    local_view_disagrees);
#endif

    return local_view_disagrees;
}



int notify_new_view_to_clients(membership * m, membership_agreement_msg * ma, vector_clock * my_lc)
{
    void * tmp_out_buf = NULL, * q = NULL;
    unsigned snd_msg_len;
    char msg_buf[1024];

    membership_agreement_msg * amr = NULL;

    membership_state * mstate = get_membership_state_from_server_list(m->local_peers, m->connected_clients, my_lc);

    int status = get_agreement_notify_packet(PROPOSAL_STATUS_ACCEPTED, mstate, ma, &amr, &tmp_out_buf, &snd_msg_len, my_lc, my_lc);

    for(snode_t * crt = HEAD(m->connected_client_sockets); crt!=NULL; crt = NEXT(crt))
    {
        client_descriptor * cd = (client_descriptor *) crt->value;

        if(cd->sockfd == 0)
            continue;

        char client_address2[INET_ADDRSTRLEN];
        inet_ntop( AF_INET, &(cd->addr).sin_addr, client_address2, sizeof(client_address2));
        log_info("SERVER: Notifying new agreed view %s to client %s:%d, fd=%d",
                            to_string_membership_agreement_msg(ma, msg_buf),
                            client_address2, ntohs(cd->addr.sin_port), cd->sockfd);

        int n = write(cd->sockfd, tmp_out_buf, snd_msg_len);

        if (n < 0)
            error("ERROR writing to socket");
    }

    free_membership_agreement(amr);

//  free_membership_state(mstate, 0);

    free(tmp_out_buf);

    return 0;
}

int handle_agreement_notify_message(membership_agreement_msg * ma, membership * m, db_t * db, vector_clock * my_lc, vector_clock * prev_vc, unsigned int * fastrandstate)
{
    int ret = install_agreed_view(ma, m, my_lc, db, fastrandstate);
    notify_new_view_to_clients(m, ma, my_lc);
    return ret;
}

int handle_agreement_notify_ack_message(membership_agreement_msg * ma, membership * m, db_t * db, unsigned int * fastrandstate)
{
    return 0;
}

int parse_gossip_message(void * rcv_buf, size_t rcv_msg_len, membership_agreement_msg ** ma, int64_t * nonce, vector_clock ** vc)
{
    int status = deserialize_membership_agreement_msg(rcv_buf, rcv_msg_len, ma);

    if(status == 0)
    {
        *nonce = (*ma)->nonce;
        *vc = (*ma)->vc;

#if (VERBOSE_RPC > 0)
        char print_buff[PRINT_BUFSIZE];
        to_string_membership_agreement_msg((*ma), (char *) print_buff);
        log_info("Received gossip message: %s", print_buff);
#endif
    }
    else
    {
        *ma = NULL;
        *vc = NULL;
        *nonce = -1;
    }

    return status;
}

int handle_join_message(int childfd, int msg_len, membership * m, db_t * db, unsigned int * fastrandstate, vector_clock * my_lc, int old_id, int my_id, remote_server * rs)
{
    membership_agreement_msg * ma = NULL;
    int64_t nonce = -1;
    vector_clock * lc_read = NULL;
    int local_membership_changed = 0;

    int status = parse_gossip_message(in_buf + sizeof(int), msg_len, &ma, &nonce, &lc_read);

    if(status != 0)
    {
        log_error("ERROR decoding server join request");
                return -1;
    }

    if(ma->msg_type != MEMBERSHIP_AGREEMENT_JOIN)
    {
        return -1;
    }

#if (VERBOSE_RPC > 0)
    char msg_buf[1024];
    log_info("SERVER: Received new join message %s!", to_string_membership_agreement_msg(ma, msg_buf));
#endif

    // Update peer socket address to announced one:

    assert(ma->membership->no_nodes == 1);

    node_description nd = ma->membership->membership[0];
    rs->status = NODE_LIVE;

    status = update_listen_socket(rs, nd.hostname, nd.portno, 0);
    assert(status == 0);

    // Check if newly joined server already existed in local peers. If so, and if announced address differs, update it:

    snode_t * node = skiplist_search(m->local_peers, &rs->serveraddr);

    if(node != NULL)
    {
        remote_server * old_rs_local = (remote_server *) node->value;

        if(strcmp((char *) &(rs->id), (char *) &(old_rs_local->id)) != 0)
        {
#if (VERBOSE_RPC > 0)
            log_info("SERVER: Removing old peer entry from local_peers: %s", old_rs_local->id);
#endif
            skiplist_delete(m->local_peers, &rs->serveraddr);

            free_remote_server(old_rs_local);
        }
        else // I knew of this peer, and entry is the same. Nothing changes in local membership, except for possibly marking that node as live and updating socketfd, of node was previously dead:
        {
            int old_status = old_rs_local->status;
            int new_joiner_id = get_node_id((struct sockaddr *) &(old_rs_local->serveraddr));

            assert(my_id != new_joiner_id);

            if(old_status == NODE_DEAD || my_id < new_joiner_id)
            {
                assert(old_rs_local->sockfd <= 0 || my_id < new_joiner_id);

#if (VERBOSE_RPC > 0)
                log_info("SERVER: Peer %s %s. Updating its sockfd from %d to %d, old_status=%d, and marking node live!",
                                                old_rs_local->id, (old_status == NODE_DEAD)?"came back up":"sent join request",
                                                old_rs_local->sockfd, rs->sockfd, old_rs_local->status);
#endif

                old_rs_local->status = NODE_LIVE;

                old_rs_local->sockfd = rs->sockfd;
            }
            else
            {
                assert(old_rs_local->sockfd > 0);

#if (VERBOSE_RPC > 0)
                log_debug("SERVER: I am already connected to peer %s (status = %d), and my id > his id. NOT updating its sockfd from %d to %d, and closing new socket!",
                                        old_rs_local->id, old_rs_local->status, old_rs_local->sockfd, rs->sockfd);
#endif

//              close(rs->sockfd);
            }

            return old_rs_local->status != old_status; // We s'd propose new membership if local membership changed
        }
    }

    // Add newly joined peer to local_peers:

    status = add_remote_server_to_membership(rs, m, LOCAL_PEERS, fastrandstate);

    assert(status == 0);

    if(ma != NULL)
            free_membership_agreement(ma);

    return 1;
}


int handle_server_message(int childfd, int msg_len, membership * m, db_t * db, unsigned int * fastrandstate, vector_clock * my_lc, int sender_id)
{
    void * tmp_out_buf = NULL;
    unsigned snd_msg_len;
    membership_agreement_msg * ma = NULL;
    int64_t nonce = -1;
    vector_clock * lc_read = NULL;
    short output_packet = 1;
    membership_state * merged_membership = NULL;

    int status = parse_gossip_message(in_buf + sizeof(int), msg_len, &ma, &nonce, &lc_read);

    if(status != 0)
    {
        log_error("ERROR decoding server gossip message");
                return -1;
    }

    mark_live(m, sender_id);

    vector_clock * prev_vc = copy_vc(my_lc);

#ifdef UPDATE_LC_ON_GOSSIP
    if(lc_read != NULL)
    {
        // If we were multi-threaded, we'd have to protect this snippet:

#if (VERBOSE_RPC > 2)
        char msg_buf[1024];
        log_debug("SERVER: Received server message with LC %s.", to_string_vc(lc_read, msg_buf));
        log_debug("SERVER: My LC before update is %s.", to_string_vc(my_lc, msg_buf));
#endif

        update_vc(my_lc, lc_read);

#if (VERBOSE_RPC > 2)
        log_debug("SERVER: Updated local LC to %s.", to_string_vc(my_lc, msg_buf));
#endif

        increment_vc(my_lc, m->my_id);

#if (VERBOSE_RPC > 2)
        log_debug("SERVER: Incremented local LC to %s.", to_string_vc(my_lc, msg_buf));
#endif
    }
#endif

    switch(ma->msg_type)
    {
        case MEMBERSHIP_AGREEMENT_PROPOSE:
        {
            status = handle_agreement_propose_message(ma, &merged_membership, m, db, copy_vc(my_lc), prev_vc, fastrandstate);
//          assert(status == 0);
            status = get_agreement_response_packet(status, merged_membership, ma, &tmp_out_buf, &snd_msg_len, copy_vc(my_lc));
            break;
        }
        case MEMBERSHIP_AGREEMENT_RESPONSE:
        {
            status = handle_agreement_response_message(ma, &merged_membership, m, db, copy_vc(my_lc), prev_vc, fastrandstate);

            if(status < 0)
            {
                output_packet = 0;
            }
            else if(m->proposal_status == PROPOSAL_STATUS_REJECTED)
            {
                // NOP
            }
            else if(m->outstanding_proposal_acks == 0)
            {
                assert(merged_membership != NULL);

                if(m->proposal_status == PROPOSAL_STATUS_ACCEPTED)
                {
                    membership_agreement_msg * amr = NULL;

                    status = get_agreement_notify_packet(PROPOSAL_STATUS_ACCEPTED, merged_membership, ma, &amr, &tmp_out_buf, &snd_msg_len, copy_vc(my_lc), prev_vc);

                    int local_view_disagrees = install_agreed_view(amr, m, copy_vc(my_lc), db, fastrandstate);

                    notify_new_view_to_clients(m, amr, copy_vc(my_lc));

                    free_membership_agreement(amr);

                    //  if(local_view_disagrees)
                    //  {
                    //      propose_local_membership(m, copy_vc(my_lc), fastrandstate);
                    //      output_packet = 0;
                    //  }
                    //
                }
                else if(m->proposal_status == PROPOSAL_STATUS_AMMENDED) // re-propose merged membership from previous round
                {
                    m->outstanding_proposal = skiplist_clone(m->merged_responses, fastrandstate);
                    m->outstanding_proposal_clients = skiplist_clone(m->merged_client_responses, fastrandstate);
                    m->outstanding_view_id = copy_vc(my_lc);
                    m->outstanding_proposal_nonce = _get_nonce(fastrandstate);
                    m->proposal_status = PROPOSAL_STATUS_ACTIVE;
                    m->outstanding_proposal_acks = 0;

                    status = get_agreement_propose_packet(0, get_membership_state_from_server_list(m->outstanding_proposal, m->outstanding_proposal_clients, copy_vc(m->outstanding_view_id)),
                                                                m->outstanding_proposal_nonce, &tmp_out_buf, &snd_msg_len, copy_vc(m->outstanding_view_id));
                }
            }
            else
            {
                output_packet = 0;
            }
            break;
        }
        case MEMBERSHIP_AGREEMENT_NOTIFY:
        {
            int local_view_disagrees = handle_agreement_notify_message(ma, m, db, copy_vc(my_lc), prev_vc, fastrandstate);

//          if(local_view_disagrees)
//              propose_local_membership(m, copy_vc(my_lc), fastrandstate);

            status = get_agreement_notify_ack_packet(status, ma, &tmp_out_buf, &snd_msg_len, copy_vc(my_lc));
            break;
        }
        case MEMBERSHIP_AGREEMENT_RETRY_LINK:
        {
            assert(0);
            break;
        }
        case MEMBERSHIP_AGREEMENT_NOTIFY_ACK:
        {
            status = handle_agreement_notify_ack_message(ma, m, db, fastrandstate);
//          assert(status == 0);
            output_packet = 0;
            break;
        }
    }

//    assert(status == 0);

    if(output_packet)
    {
            if(ma->msg_type == MEMBERSHIP_AGREEMENT_RESPONSE) // multicast notifications
            {
                assert(merged_membership != NULL);

            for(snode_t * crt = HEAD(m->merged_responses); crt!=NULL; crt = NEXT(crt))
            {
                remote_server * rs = (remote_server *) crt->value;

                if(rs->status == NODE_LIVE && rs->sockfd > 0 && get_node_id((struct sockaddr *) &(rs->serveraddr)) != m->my_id) // skip myself, and nodes that are "down" in membership
                {
                    int n = write(rs->sockfd, tmp_out_buf, snd_msg_len);

                    if (n < 0)
                        error("ERROR writing to socket");

                    m->outstanding_proposal_acks++;
                }
            }
            }
            else // unicast back response
            {
            int n = write(childfd, tmp_out_buf, snd_msg_len);

            if (n < 0)
              error("ERROR writing to socket");
            }

        free(tmp_out_buf);
    }

//    if(merged_membership != NULL)
//          free_membership_state(merged_membership);

    if(ma != NULL)
            free_membership_agreement(ma);

    return 0;
}

typedef struct argp_arguments
{
  int verbosity;
  int portno;
  int gportno;
  char ** seeds;
  unsigned short * seed_ports;
  int no_seeds;
  char * local_iface;
  char *mon_socket_path;
  char *log_file_path;
} argp_arguments;

error_t parse_opt (int key, char *arg, struct argp_state *state)
{
    argp_arguments * arguments = (argp_arguments *) state->input;
    char tmp_buff[10];

    switch (key)
        {
        case 'v':
            arguments->verbosity = 2;
            break;
        case 'q':
            arguments->verbosity = 0;
            break;
        case 'p':
            arguments->portno = atoi(arg);
            break;
        case 'm':
            arguments->gportno = atoi(arg);
            break;
        case 's':
            assert(strnlen(arg, 256) > 1);
            arguments->no_seeds = 1;
            for (char * end_ptr = strchr(arg, ','); end_ptr != NULL; end_ptr = strchr(end_ptr + 1, ','), arguments->no_seeds++);
            arguments->seeds = (char **) malloc(arguments->no_seeds * sizeof(char *));
            arguments->seed_ports = (unsigned short *) malloc(arguments->no_seeds * sizeof(int));
            char * start_ptr = arg;
            char * end_ptr = NULL;
            int stop = 0;
            for(int i = 0; !stop; i++) {
                end_ptr = strchr(start_ptr, ',');

                char * separator_ptr = strchr(start_ptr, ':');
                assert(separator_ptr != NULL);
                *separator_ptr = '\0';

                arguments->seeds[i] = strndup(start_ptr, separator_ptr - start_ptr);
                arguments->seed_ports[i] = (unsigned short) atoi(separator_ptr + 1);

                if(end_ptr == NULL) {
                    stop = 1;
                } else {
                    *end_ptr = '\0';
                    start_ptr = end_ptr + 1;
                }
            }
            break;
        case 'S':
            arguments->mon_socket_path = arg;
            break;
        case 'i':
            assert(strnlen(arg, 256) > 1);
            arguments->local_iface = strndup(arg, 256);
            break;
        case 'l':
            arguments->log_file_path = arg;
            break;
        case ARGP_KEY_ARG:
        case ARGP_KEY_END:
            //        argp_usage (state);
            break;
        default:
            return ARGP_ERR_UNKNOWN;
        }

    return 0;
}


const char* membership_to_json (membership *m) {
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    yyjson_mut_obj_add_str(doc, root, "name", "membership");

    yyjson_mut_obj_add_int(doc, root, "pid", pid);

    struct timeval tv;
    struct tm tm;
    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &tm);
    char dt[32];    // = "YYYY-MM-ddTHH:mm:ss.SSS+0000";
    strftime(dt, 32, "%Y-%m-%dT%H:%M:%S.000%z", &tm);
    sprintf(dt + 20, "%03hu%s", (unsigned short)(tv.tv_usec / 1000), dt + 23);

    yyjson_mut_obj_add_str(doc, root, "datetime", dt);

    yyjson_mut_val *j_mbm = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, root, "membership", j_mbm);
    yyjson_mut_obj_add_int(doc, j_mbm, "my_id", m->my_id);
    char view_id[1024];
    yyjson_mut_obj_add_str(doc, j_mbm, "view_id", to_string_vc(m->view_id, view_id));

    yyjson_mut_val *j_nodes = yyjson_mut_obj(doc);
    yyjson_mut_obj_add_val(doc, root, "nodes", j_nodes);
    for(snode_t * crt = HEAD(m->agreed_peers); crt!=NULL; crt = NEXT(crt)) {
        remote_server * rs = (remote_server *) crt->value;

        yyjson_mut_val *j_node = yyjson_mut_obj(doc);
        yyjson_mut_obj_add_val(doc, j_nodes, rs->id, j_node);
        yyjson_mut_obj_add_str(doc, j_node, "type", "DDB");
        yyjson_mut_obj_add_str(doc, j_node, "hostname", rs->hostname);
        yyjson_mut_obj_add_str(doc, j_node, "status", RS_status_name[rs->status]);
    }
    for(snode_t * crt = HEAD(m->connected_clients); crt!=NULL; crt = NEXT(crt)) {
        remote_server * rs = (remote_server *) crt->value;

        yyjson_mut_val *j_node = yyjson_mut_obj(doc);
        yyjson_mut_obj_add_val(doc, j_nodes, rs->id, j_node);
        yyjson_mut_obj_add_str(doc, j_node, "type", "RTS");
        yyjson_mut_obj_add_str(doc, j_node, "hostname", rs->hostname);
        yyjson_mut_obj_add_str(doc, j_node, "status", RS_status_name[rs->status]);
    }

    const char *json = yyjson_mut_write(doc, 0, NULL);
    yyjson_mut_doc_free(doc);
    return json;
}


int main(int argc, char **argv) {
  int parentfd, gparentfd, childfd;
  int portno, gportno;
  unsigned int clientlen;
  struct sockaddr_in serveraddr, gserveraddr, clientaddr;
  struct hostent *hostp;
  char *hostaddrp;
  int optval; /* flag value for setsockopt */
  int msg_len; /* message byte size */
  fd_set readfds;
  struct timeval timeout;
  timeout.tv_sec = 3;
  timeout.tv_usec = 0;
  unsigned int seed;
  int ret = 0;
  char msg_buf[1024];
  int verbosity = SERVER_VERBOSITY;
  FILE *logf = NULL;

  pid = getpid();

  // Do line buffered output
  setlinebuf(stdout);

  const char *argp_program_version = "ddb server 1.0";

  static struct argp_option options[] =
  {
    {"verbose",         'v',        0, 0,  "Produce verbose output" },
    {"quiet",           'q',        0, 0,  "Don't produce any output" },
    {"port",            'p',   "PORT", 0,  "Port for client data requests" },
    {"mon-socket-path", 'S',   "PATH", 0,  "Path to unix socket to expose mon stats" },
    {"mport",           'm',  "MPORT", 0,  "Port for server gossip packets" },
    {"seeds",           's',  "SEEDS", 0,  "Seeds, comma-separated" },
    {"iface",           'i',  "IFACE", 0,  "Local interface to listen to" },
    {"log-file",        'l',   "FILE", 0,  "Log file" },
    { 0 }
  };

  static struct argp argp = { options, parse_opt, NULL, "ddb - standalone server module" };

  argp_arguments arguments;

  /* Default values. */
  arguments.verbosity = 1;
  arguments.portno = DEFAULT_DATA_PORT;
  arguments.gportno = -1;
  arguments.local_iface = "127.0.0.1";
  arguments.no_seeds = 0;
  arguments.mon_socket_path = NULL;
  arguments.log_file_path = NULL;

  argp_parse (&argp, argc, argv, 0, 0, &arguments);

  if (arguments.log_file_path) {
      logf = fopen(arguments.log_file_path, "w");
      if (!logf) {
          log_error("Unable to open log file (%s) for writing", arguments.log_file_path);
          exit(1);
      }
      log_add_fp(logf, LOG_DEBUG); // LOG_TRACE
      // Turn off log output on stderr
      log_set_quiet(true);
  }

  // Ignore SIGPIPE:

  struct sigaction sa;
  memset(&sa, 0, sizeof(sa));

  sa.sa_handler = SIG_IGN;

  if (sigaction(SIGPIPE, &sa, NULL) == -1)
  {
      error("sigaction(SIG_IGN SIGPIPE) failed");
  }

  portno = arguments.portno;
  // NOTE: gossip port is expected to be +1 from the RPC port. This is a current
  //  limitation of the gossip protocol as it doesn't carry information about a
  //  separate gossip port.
  if (arguments.gportno > 0) {
      // TODO: remove this once gossip protocol carries gossip port info
      if (arguments.gportno != arguments.portno+1)
          log_warn("Using a gossip port other than RPC port + 1 is not a supported configuration");
      gportno = arguments.gportno;
  } else {
      gportno = arguments.portno+1;
  }
  verbosity = arguments.verbosity;
  log_info("Using args: portno=%d, gportno=%d, verbosity=%d, seeds:", portno, gportno, verbosity);

  for(int i=0;i<arguments.no_seeds;i++)
  {
      struct hostent * host = gethostbyname(arguments.seeds[i]);
      if (host == NULL)
      {
          log_error("ERROR, no such host %s", arguments.seeds[i]);
          return -1;
      }

      free(arguments.seeds[i]);
      arguments.seeds[i] = strdup(host->h_name);

      log_info("%s:%d", arguments.seeds[i], arguments.seed_ports[i]);
  }

  skiplist_t * clients = create_skiplist(&sockaddr_cmp); // List of remote clients

  GET_RANDSEED(&seed, 0); // thread_id

  // Get db pointer:
  db_t * db = get_db();

  // Create schema:
  ret = create_state_schema(db, &seed);
  if (ret == 0) {
      log_info("State schema successfully created");
  } else {
      log_fatal("Failed to create state schema");
  }

  ret = create_queue_schema(db, &seed);
  if (ret == 0) {
      log_info("Queue schema successfully created");
  } else {
      log_fatal("Failed to create queue schema");
  }

  struct hostent * local_iface_hostent = gethostbyname(arguments.local_iface);

  // Set up main data socket:

  parentfd = socket(AF_INET, SOCK_STREAM, 0);
  if (parentfd < 0)
    error("ERROR opening socket");

  optval = 1;
  setsockopt(parentfd, SOL_SOCKET, SO_REUSEADDR, (const void *)&optval , sizeof(int));
  bzero((char *) &serveraddr, sizeof(serveraddr));
  serveraddr.sin_family = AF_INET;
  serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
  serveraddr.sin_port = htons((unsigned short) portno);

  // Set up main gossip socket:

  gparentfd = socket(AF_INET, SOCK_STREAM, 0);
  if (gparentfd < 0)
    error("ERROR opening gossip socket");

  setsockopt(gparentfd, SOL_SOCKET, SO_REUSEADDR, (const void *)&optval , sizeof(int));
  bzero((char *) &gserveraddr, sizeof(serveraddr));
  gserveraddr.sin_family = AF_INET;
  bcopy(local_iface_hostent->h_addr_list[0], (char *)&(gserveraddr.sin_addr.s_addr), local_iface_hostent->h_length);
//  gserveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
  gserveraddr.sin_port = htons((uint16_t) gportno);

  int my_id = get_node_id((struct sockaddr *) &gserveraddr);
  char * my_address = strdup(inet_ntoa(gserveraddr.sin_addr));
  unsigned short my_port = (unsigned short) ntohs(gserveraddr.sin_port);

  vector_clock * my_lc = init_local_vc_id(my_id);

  membership * m = get_membership(my_id);
  m->connected_client_sockets = clients;

  log_info("Started [%s:%d, %s:%d], my_lc = %s", inet_ntoa(serveraddr.sin_addr), ntohs(serveraddr.sin_port), my_address, my_port, to_string_vc(my_lc, msg_buf));

  // Set up monitoring socket
  int mon_sock=-1, mon_client_sock=-1;
  struct sockaddr_un mon_addr, mon_client_addr;
  char mon_rbuf[64];
  size_t mon_buf_used = 0;
  if (arguments.mon_socket_path) {
      if ((mon_sock = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
          log_error(LOGPFX "ERROR: Unable to create Monitor Socket");
          exit(1);
      }

      mon_addr.sun_family = AF_UNIX;
      strcpy(mon_addr.sun_path, arguments.mon_socket_path);
      unlink(mon_addr.sun_path);
      if (bind(mon_sock, (struct sockaddr *)&mon_addr, sizeof(mon_addr.sun_path) + sizeof(mon_addr.sun_family)) == -1) {
    log_error(LOGPFX "ERROR: Unable to bind to Monitor Socket");
          exit(1);
      }
      if (listen(mon_sock, 5) == -1) {
          log_error(LOGPFX "ERROR: Unable to listen on Monitor Socket");
          exit(1);
      }
  }

  if (bind(parentfd, (struct sockaddr *) &serveraddr, sizeof(serveraddr)) < 0)
    error("ERROR on binding client socket");

  if (listen(parentfd, 100) < 0) /* allow 100 requests to queue up */
    error("ERROR on listen client socket");

  if (bind(gparentfd, (struct sockaddr *) &gserveraddr, sizeof(serveraddr)) < 0)
    error("ERROR on binding gossip socket");

  if (listen(gparentfd, 100) < 0) /* allow 100 requests to queue up */
    error("ERROR on listen gossip socket");

  // Add myself to local membership:

  remote_server * rsp = NULL;
  ret = add_peer_to_membership(my_address, my_port, gserveraddr, OWN_FD, 0, m, LOCAL_PEERS, &rsp, &seed);

  // Now add seed servers:

  struct sockaddr_in dummy_serveraddr;

  for(int i=0;i<arguments.no_seeds;i++)
  {
      int cmp_res = strncmp(arguments.seeds[i], my_address, 256);

      // Skip connecting to myself:
      if(cmp_res == 0 && arguments.seed_ports[i] == my_port)
      {
          log_debug("SERVER: Skipping connecting to seed %s:%d (myself)", arguments.seeds[i], arguments.seed_ports[i]);
          continue;
      }

#if (DO_HIERARCHICAL_CONNECT > 0)
      // Also skip connecting to seed nodes with IP:port bigger (lexicographycally) than myself (they will connect to me as they come up):
      if(cmp_res > 0 || ((cmp_res == 0) && (arguments.seed_ports[i] > my_port)) )
      {
          log_debug("SERVER: Skipping connecting to seed %s:%d (> myself)", arguments.seeds[i], arguments.seed_ports[i]);
      }
      else
      {
#endif
          log_debug("SERVER: Connecting to %s:%d..", arguments.seeds[i], arguments.seed_ports[i]);
          ret = add_peer_to_membership(arguments.seeds[i], arguments.seed_ports[i], dummy_serveraddr, DUMMY_FD, 1, m, LOCAL_PEERS, &rsp, &seed);
          if(ret == 0 && rsp->status == NODE_LIVE)
          {
              ret = send_join_message(0, 0, my_address, my_port, my_lc, rsp, &seed);
          }
#if (DO_HIERARCHICAL_CONNECT > 0)
      }
#endif
  }

  ret = propose_local_membership(m, copy_vc(my_lc), NULL, -1, &seed);

  clientlen = sizeof(clientaddr);

  while(1)
  {
        FD_ZERO(&readfds);

        // Add parent sockets to read set:

        FD_SET(parentfd, &readfds);
        FD_SET(gparentfd, &readfds);
        int max_fd = (parentfd>gparentfd)?(parentfd):(gparentfd);

        // Add active clients to read set:

        for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt))
        {
            client_descriptor * rs = (client_descriptor *) crt->value;
            if(rs->sockfd > 0)
            {
//              log_debug("SERVER: Listening to client socket %s..", rs->id);
                FD_SET(rs->sockfd, &readfds);
                max_fd = (rs->sockfd > max_fd)? rs->sockfd : max_fd;
            }
            else
            {
//              log_debug("SERVER: Not listening to disconnected client socket %s..", rs->id);
            }
        }

        // Add active peers to read set:

        for(snode_t * crt = HEAD(m->local_peers); crt!=NULL; crt = NEXT(crt))
        {
            remote_server * rs = (remote_server *) crt->value;
            if(rs->sockfd > 0)
            {
                if(verbosity > 3)
                {
                    ret = fcntl(rs->sockfd, F_GETFL);
                    log_debug("active peer %s, sockfd=%d, status=%d, flags=%d", rs->id, rs->sockfd, rs->status, ret);
                }

//              log_debug("SERVER: Listening to peer socket %s..", rs->id);
                FD_SET(rs->sockfd, &readfds);
                max_fd = (rs->sockfd > max_fd)? rs->sockfd : max_fd;
            }
            else
            {
//              log_debug("SERVER: Not listening to disconnected peer socket %s..", rs->id);
            }
        }

        // Add active pre-joined peers to read set:

        for(snode_t * crt = HEAD(m->connected_peers); crt!=NULL; crt = NEXT(crt))
        {
            remote_server * rs = (remote_server *) crt->value;
            if(rs->sockfd > 0 && rs->status == NODE_PREJOINED)
            {
                if(verbosity > 3)
                {
                    ret = fcntl(rs->sockfd, F_GETFL);
                    log_debug("pre-joined peer %s, sockfd=%d, status=%d, flags=%d", rs->id, rs->sockfd, rs->status, ret);
                }

//              log_debug("SERVER: Listening to peer socket %s..", rs->id);
                FD_SET(rs->sockfd, &readfds);
                max_fd = (rs->sockfd > max_fd)? rs->sockfd : max_fd;
            }
            else
            {
//              log_debug("SERVER: Not listening to disconnected peer socket %s..", rs->id);
            }
        }

        // Monitor socket
        if (mon_sock > 0) {
            FD_SET(mon_sock, &readfds);
            max_fd = (mon_sock > max_fd)? mon_sock : max_fd;
        }
        if (mon_client_sock > 0) {
            FD_SET(mon_client_sock, &readfds);
            max_fd = (mon_client_sock > max_fd)? mon_client_sock : max_fd;
        }

        int status = select(max_fd + 1, &readfds, NULL, NULL, NULL); // &timeout

        if (status < 0)
        {
            if ((errno != EINTR) && (errno != EBADF))
            {
                log_error("select error, errno: %d", errno);
                assert(0); // EINVAL, ENOMEM
            }
            continue;
        }

        if(verbosity > 3)
            log_debug("select returned %d/%d!", status, errno);

        // Monitor socket
        if (mon_sock > 0 && FD_ISSET(mon_sock, &readfds)) {
            if (mon_client_sock == -1) {
                socklen_t t = sizeof(mon_client_sock);
                if ((mon_client_sock = accept(mon_sock, (struct sockaddr *)&mon_client_addr, &t)) == -1) {
                    perror("accept");
                    exit(1);
                }
            }
        }

        // Monitor socket client requests
        if (mon_client_sock > 0 && FD_ISSET(mon_client_sock, &readfds)) {
            char *buf_base, *str;
            size_t len;
            ssize_t bytes_read = recv(mon_client_sock, &mon_rbuf[mon_buf_used], sizeof(mon_rbuf) - mon_buf_used, 0);
            if (bytes_read <= 0) {
                close(mon_client_sock);
                mon_client_sock = -1;
            } else {
                mon_buf_used += bytes_read;

                buf_base = mon_rbuf;
                while (1) {
                    if (mon_buf_used == 0)
                        break;
                    int r = netstring_read(&buf_base, &mon_buf_used, &str, &len);
                    if (r != 0) {
                        log_error(LOGPFX "Mon socket: Error reading netstring: %d", r);
                        break;
                    }

                    if (memcmp(str, "membership", len) == 0) {
                        const char *json = membership_to_json(m);
                        char *send_buf = malloc(strlen(json)+14); // maximum digits for length is 9 (999999999) + : + ; + \0
                        sprintf(send_buf, "%lu:%s,", strlen(json), json);
                        int send_res = send(mon_client_sock, send_buf, strlen(send_buf), 0);
                        free((void *)json);
                        free((void *)send_buf);
                        if (send_res < 0) {
                            break;
                        }
                    }
                }

                if (buf_base > mon_rbuf && mon_buf_used > 0)
                    memmove(mon_rbuf, buf_base, mon_buf_used);
            }
        }

        // Check if there's a new connection attempt from a client:

        if(FD_ISSET(parentfd, &readfds))
        {
              childfd = accept(parentfd, (struct sockaddr *) &clientaddr, &clientlen);
              if (childfd < 0)
                error("ERROR on accept");

#ifdef MACOS
              int option_set = 1;
              if (setsockopt (childfd, SOL_SOCKET, SO_NOSIGPIPE, &option_set, sizeof (option_set)) < 0) {
                  error("setsockopt(SO_NOSIGPIPE)");
              }
#endif

              hostp = gethostbyaddr((const char *)&clientaddr.sin_addr.s_addr,
                          sizeof(clientaddr.sin_addr.s_addr), AF_INET);
              if (hostp == NULL)
                error("ERROR on gethostbyaddr");
              hostaddrp = strdup(inet_ntoa(clientaddr.sin_addr));
              if (hostaddrp == NULL)
                error("ERROR on inet_ntoa");

              log_info("SERVER: accepted connection from client: %s (%s:%d)", hostp->h_name, hostaddrp, ntohs(clientaddr.sin_port));

              ret = add_client_to_membership(clientaddr, childfd, hostaddrp, ntohs(clientaddr.sin_port), clients, &seed); // hostp->h_name

//            assert(ret == 0);
        }

        // Check if there's a new connection attempt from a peer server:

        if(FD_ISSET(gparentfd, &readfds))
        {
              childfd = accept(gparentfd, (struct sockaddr *) &clientaddr, &clientlen);
              if (childfd < 0)
                error("ERROR on accept");

#ifdef MACOS
              int option_set = 1;
              if (setsockopt (childfd, SOL_SOCKET, SO_NOSIGPIPE, &option_set, sizeof (option_set)) < 0) {
                  error("setsockopt(SO_NOSIGPIPE)");
              }
#endif

              hostp = gethostbyaddr((const char *)&clientaddr.sin_addr.s_addr,
                          sizeof(clientaddr.sin_addr.s_addr), AF_INET);
              if (hostp == NULL)
                error("ERROR on gethostbyaddr");
              hostaddrp = strdup(inet_ntoa(clientaddr.sin_addr));
              if (hostaddrp == NULL)
                error("ERROR on inet_ntoa");

              log_info("SERVER: accepted connection from peer: %s (%s:%d), sockfd=%d", hostp->h_name, hostaddrp, ntohs(clientaddr.sin_port), childfd);

              ret = add_peer_to_membership(hostaddrp, ntohs(clientaddr.sin_port), clientaddr, childfd, 0, m, CONNECTED_PEERS, &rsp, &seed); // hostp->h_name
              rsp->status = NODE_PREJOINED;

//            assert(ret == 0);

              continue;
        }

        // Check if there are messages from existing clients:

        for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt))
        {
            client_descriptor * cd = (client_descriptor *) crt->value;
            if(cd->sockfd > 0 && FD_ISSET(cd->sockfd , &readfds))
            // Received a msg from this client:
            {
                if(read_full_packet(&(cd->sockfd), (char *) in_buf, SERVER_BUFSIZE, &msg_len, &ret, &handle_socket_close))
                {
                    if(ret == NODE_DEAD)
                    {
                        remote_server * rs = lookup_client_by_client_socket_addr(&(cd->addr), m->connected_clients);
                        if(rs != NULL)
                        {
                            rs->status = NODE_DEAD;
                            log_debug("Client %s, sockfd=%d, status=%d went dead, proposing new membership..", rs->id, rs->sockfd, rs->status);
                            propose_local_membership(m, copy_vc(my_lc), NULL, -1, &seed);
                        }
                    }
                    continue;
                }

                if(handle_client_message(cd->sockfd, msg_len, db, m, clients, &seed, my_lc, my_id))
                        continue;
            }
        }

        // Check if there are messages from peer servers:

        for(snode_t * crt = HEAD(m->local_peers); crt!=NULL; crt = NEXT(crt))
        {
            remote_server * rs = (remote_server *) crt->value;

            if(rs->sockfd > 0 && FD_ISSET(rs->sockfd , &readfds))
            // Received a msg from this server:
            {
                log_trace("active peer %s, sockfd=%d, status=%d is ready for reading", rs->id, rs->sockfd, rs->status);

                ret = read_full_packet(&(rs->sockfd), (char *) in_buf, SERVER_BUFSIZE, &msg_len, &(rs->status), &handle_socket_close);

                if(rs->status == NODE_DEAD || rs->sockfd <= 0) // If peer closed socket, propose it removed from agreed membership
                {
                    propose_local_membership(m, copy_vc(my_lc), NULL, -1, &seed);
                    continue;
                }

                int sender_id = get_node_id((struct sockaddr *) &(rs->serveraddr));
                if(handle_server_message(rs->sockfd, msg_len, m, db, &seed, my_lc, sender_id))
                        continue;
            }
        }

        // Check if there are messages from pre-joined peer servers:

        for(snode_t * crt = HEAD(m->connected_peers); crt!=NULL; crt = NEXT(crt))
        {
            remote_server * rs = (remote_server *) crt->value;

            if(rs->status == NODE_PREJOINED && rs->sockfd > 0 && FD_ISSET(rs->sockfd , &readfds))
            // Received a msg from this server:
            {
                log_trace("pre-joined peer %s, sockfd=%d, status=%d is ready for reading", rs->id, rs->sockfd, rs->status);

                ret = read_full_packet(&(rs->sockfd), (char *) in_buf, SERVER_BUFSIZE, &msg_len, &(rs->status), &handle_socket_nop);

                // TO DO: If peer closed socket before it sent join packet (rs->status == NODE_DEAD || rs->sockfd <= 0), also remove it from connected list to save some space

                if(ret)
                    continue;

                int sender_id = get_node_id((struct sockaddr *) &(rs->serveraddr));
                if((ret=handle_join_message(rs->sockfd, msg_len, m, db, &seed, my_lc, sender_id, my_id, rs)) < 0)
                {
                    if(handle_server_message(rs->sockfd, msg_len, m, db, &seed, my_lc, sender_id))
                        continue;
                }


                if(ret > 0) // local membership changed
                {
                    if(verbosity > 0)
                        log_debug("Membership changed after %s/%d/%d joined, proposing new membership", rs->id, rs->sockfd, rs->status);

                    propose_local_membership(m, copy_vc(my_lc), NULL, -1, &seed);
                }
                else
                {
                    if(verbosity > 0)
                        log_debug("Membership did not change after %s/%d/%d joined, NOT proposing new membership", rs->id, rs->sockfd, rs->status);
                }
            }
        }
  }

  // Close sockets to clients and peers:

    for(snode_t * crt = HEAD(clients); crt!=NULL; crt = NEXT(crt))
    {
        client_descriptor * rs = (client_descriptor *) crt->value;
        if(rs->sockfd > 0)
        {
            close(rs->sockfd);
        }
    }

    for(snode_t * crt = HEAD(m->local_peers); crt!=NULL; crt = NEXT(crt))
    {
        remote_server * rs = (remote_server *) crt->value;
        if(rs->sockfd > 0)
        {
            close(rs->sockfd);
        }
    }

    if (logf) {
        fclose(logf);
    }
}
