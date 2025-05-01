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
 * client_api.h
 *
 *      Author: aagapi
 */

#ifndef BACKEND_CLIENT_API_H_
#define BACKEND_CLIENT_API_H_

#include "backend/db.h"
#include "backend/queue_callback.h"
#include "backend/failure_detector/db_queries.h"
#include "backend/failure_detector/fd.h"
#include "backend/fastrand.h"
#include "backend/comm.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <uuid/uuid.h>
#include <errno.h>

#define RANDOM_NONCES

#define CLIENT_VERBOSITY 0
#define CLIENT_LOCK_VERBOSITY 0
#define SYNC_SOCKET 1

#define NO_QUORUM_ERR -1
#define NO_SUCH_MSG_CALLBACK -2
#define NO_SUCH_TXN -3
#define SUBSCRIPTION_EXISTS -4
#define NO_SUBSCRIPTION_EXISTS -5

#define DEBUG_BLOBS 0

/*
 * Statistics per type of DB operation
 *
 * When a DB operation is called through the client API, we immediately
 * increment the "called" counter. When an operation ends successfully we
 * increment the success counter or if it fails, we increment the error counter.
 * This way, we can see the number of outstanding requests through
 * called-(success+error). no_quorum is a specific error type, and such errors
 * are counted both using the generic "error" counter as well as the more
 * specific no_quorum counter. The timing of each operation is recorded in the
 * time_ buckets, forming a histogram.
 */
struct dbc_ops_stat {
    const char *name;
    unsigned long long called;     // Number of calls of this op, incremented on start
    unsigned long long completed;  // Number of completed, incremented on completion
    // Currently outstanding / in-flight ops is the diff between called - completed
    unsigned long long success;    // Number of successful calls of this op
    unsigned long long error;      // Number of failed calls of this op, includes all errors
    // success + error = completed
    unsigned long long no_quorum;  // Number of failed calls with NO_QUORUM_ERR
    unsigned long long time_sum;   // nanoseconds spent waiting for op to complete
    unsigned long long time_100ns; // bucket for <100ns
    unsigned long long time_1us;   // bucket for <1us
    unsigned long long time_10us;  // bucket for <10us
    unsigned long long time_100us; // bucket for <100us
    unsigned long long time_1ms;   // bucket for <1ms
    unsigned long long time_10ms;  // bucket for <10ms
    unsigned long long time_100ms; // bucket for <100ms
    unsigned long long time_1s;    // bucket for <1s
    unsigned long long time_10s;   // bucket for <10s
    unsigned long long time_100s;  // bucket for <100s
    unsigned long long time_inf;   // bucket for <+Inf
};

// List of operation types
#define LIST_OF_DBC_OPS \
    X(remote_insert_in_txn) \
    X(remote_update_in_txn) \
    X(remote_delete_row_in_txn) \
    X(remote_delete_cell_in_txn) \
    X(remote_delete_by_index_in_txn) \
    X(remote_search_in_txn) \
    X(remote_search_clustering_in_txn) \
    X(remote_search_columns_in_txn) \
    X(remote_search_index_in_txn) \
    X(remote_range_search_in_txn) \
    X(remote_range_search_clustering_in_txn) \
    X(remote_range_search_index_in_txn) \
    X(remote_read_full_table_in_txn) \
    X(remote_create_queue_in_txn) \
    X(remote_delete_queue_in_txn) \
    X(remote_enqueue_in_txn) \
    X(remote_read_queue_in_txn) \
    X(remote_consume_queue_in_txn) \
    X(remote_subscribe_queue) \
    X(remote_unsubscribe_queue) \
    X(remote_subscribe_queue_in_txn) \
    X(remote_unsubscribe_queue_in_txn) \
    X(subscribe_queue_client) \
    X(unsubscribe_queue_client) \
    X(remote_validate_txn) \
    X(remote_abort_txn) \
    X(remote_commit_txn) \
    X(close_client_txn)

// DB client statistics per operation type
struct dbc_stat {
    // Generate the DB client stats structure, based on the list of operations
#define X(ops_name) \
    struct dbc_ops_stat *ops_name;
LIST_OF_DBC_OPS
#undef X
};

void init_dbc_stats();

// Remote DB API:

typedef struct msg_callback
{
    void (*callback)(void *);
    WORD client_id;
    int64_t nonce;
    pthread_mutex_t * lock;
    pthread_cond_t * signal;

    pthread_mutex_t * reply_lock;
    void ** replies;
    short * reply_types;
    short no_replies;
    short no_valid_replies;
} msg_callback;

msg_callback * get_msg_callback(int64_t nonce, WORD client_id, void (*callback)(void *), int replication_factor);
int add_reply_to_msg_callback(void * reply, short reply_type, msg_callback * mc);
void free_msg_callback(msg_callback * mc);

typedef struct remote_db {
    int db_id;
    skiplist_t * servers; // List of remote database servers
    skiplist_t * rtses; // List of connected rts-es
    skiplist_t * actors; // List of actors to be deployed in the system

    skiplist_t * txn_state; // Client cache of txn state
    skiplist_t * queue_subscriptions; // Client queue subscriptions
    skiplist_t * group_queue_subscriptions; // Group client queue subscriptions
    skiplist_t * msg_callbacks; // Client msg callbacks
    pthread_mutex_t* subscribe_lock;
    pthread_mutex_t* msg_callbacks_lock;
    pthread_mutex_t* txn_state_lock;

    int replication_factor;
    int quorum_size;
    int rpc_timeout;
    int actor_replication_factor;

    pthread_t comm_thread;
    short stop_comm;
    fd_set readfds;
    int wakeup_pipe[2];

    int64_t requests;
    unsigned int fastrandstate;

    pthread_mutex_t* lc_lock;
    vector_clock * my_lc;

    vector_clock * current_view_id;
    pthread_mutex_t * gossip_lock;
    pthread_cond_t * gossip_signal;

    hash_ring * _rts_ring; // Consistent hashing ring for actor-to-rts placement
    int local_rts_id;
} remote_db_t;

typedef struct gossip_callback_args
{
    membership_state * membership;
    int status;
} gossip_callback_args;

typedef struct gossip_callback
{
    void (*callback)(gossip_callback_args *);
    pthread_mutex_t * lock;
    pthread_cond_t * signal;
} gossip_callback;

remote_db_t * get_remote_db(int replication_factor, int rack_id, int dc_id, char * hostname, unsigned short local_rts_id,
                            int no_seeds, char ** seed_hosts, int * seed_ports, unsigned int * seedptr);
int add_server_to_membership(char *hostname, int portno, int status, remote_db_t * db, unsigned int * seedptr);
msg_callback * add_msg_callback(int64_t nonce, void (*callback)(void *), remote_db_t * db);
int delete_msg_callback(int64_t nonce, remote_db_t * db);
int wait_on_msg_callback(msg_callback * mc, remote_db_t * db);
int add_reply_to_nonce(void * reply, short reply_type, int64_t nonce, remote_db_t * db);
int64_t get_nonce(remote_db_t * db);
vector_clock * get_lc(remote_db_t * db);
vector_clock * get_and_increment_lc(remote_db_t * db, int node_id);
int update_lc_protected(remote_db_t * db, vector_clock * vc_in);
int free_remote_db(remote_db_t * db);
int close_remote_db(remote_db_t * db);
int sockaddr_cmp(WORD a1, WORD a2);
int queue_callback_cmp(WORD e1, WORD e2);

// Write ops:

int remote_insert_in_txn(WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys,
                        WORD blob, size_t blob_size, WORD table_key, int * minority_status,
                        uuid_t * txnid, remote_db_t * db);
int remote_update_in_txn(int * col_idxs, int no_cols, WORD * column_values, WORD blob, size_t blob_size,
                        WORD table_key, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_delete_row_in_txn(WORD * column_values, int no_primary_keys, WORD table_key, int * minority_status,
                                uuid_t * txnid, remote_db_t * db);
int remote_delete_cell_in_txn(WORD * column_values, int no_primary_keys, int no_clustering_keys,
                               WORD table_key, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_delete_by_index_in_txn(WORD index_key, int idx_idx, WORD table_key, int * minority_status, uuid_t * txnid, remote_db_t * db);

// Read ops:

int remote_search_in_txn(WORD* primary_keys, int no_primary_keys, db_row_t** result_row, WORD table_key,
                        int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
                                    db_row_t** result_row, WORD table_key, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
                                    WORD* col_keys, int no_columns, db_row_t** result_row, WORD table_key,
                                    int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_search_index_in_txn(WORD index_key, int idx_idx, db_row_t** result_row, WORD table_key,
                                int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_range_search_in_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
                            snode_t** start_row, snode_t** end_row,
                            WORD table_key, int * no_items, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_range_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys,
                                     WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
                                     snode_t** start_row, snode_t** end_row,
                                     WORD table_key, int * no_items, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_range_search_index_in_txn(int idx_idx, WORD start_idx_key, WORD end_idx_key,
                                    snode_t** start_row, snode_t** end_row,
                                    WORD table_key, int * no_items, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_read_full_table_in_txn(snode_t** start_row, snode_t** end_row, WORD table_key,
                                    int * no_items, int * minority_status, uuid_t * txnid, remote_db_t * db);
void remote_print_long_table(WORD table_key, remote_db_t * db);

// Queue ops:

int remote_create_queue_in_txn(WORD table_key, WORD queue_id, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_delete_queue_in_txn(WORD table_key, WORD queue_id, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_enqueue_in_txn(WORD * column_values, int no_cols, WORD blob, size_t blob_size, WORD table_key,
                        WORD queue_id, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_read_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int max_entries, int * entries_read, int64_t * new_read_head,
        snode_t** start_row, snode_t** end_row, int * minority_status, uuid_t * txnid,
        remote_db_t * db);
int remote_consume_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                    int64_t new_consume_head, int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
                        int * minority_status, remote_db_t * db);
int remote_unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        int * minority_status, remote_db_t * db);
int remote_subscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
                        int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_unsubscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        int * minority_status, uuid_t * txnid, remote_db_t * db);
int remote_subscribe_group(WORD consumer_id, WORD shard_id, WORD app_id, WORD group_id,
                        queue_callback * callback, int * minority_status, remote_db_t * db);
int remote_unsubscribe_group(WORD consumer_id, WORD shard_id, WORD app_id, WORD group_id,
                        int * minority_status, remote_db_t * db);

// Subscription handling client-side:

queue_callback * get_queue_client_callback(WORD consumer_id, WORD shard_id, WORD app_id, WORD group_id, WORD table_key, WORD queue_id,
                    short use_lock, remote_db_t * db);
int subscribe_queue_client(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                    queue_callback * callback, short use_lock, remote_db_t * db);
int unsubscribe_queue_client(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        short use_lock, remote_db_t * db);
int remote_add_queue_to_group(WORD table_key, WORD queue_id, WORD group_id, short use_lock, remote_db_t * db);
int remote_remove_queue_from_group(WORD table_key, WORD queue_id, WORD group_id, short use_lock, remote_db_t * db);
int subscribe_to_group(WORD consumer_id, WORD shard_id, WORD app_id, WORD group_id,
                            queue_callback * callback, short use_lock, remote_db_t * db);
int unsubscribe_from_group(WORD consumer_id, WORD shard_id, WORD app_id, WORD group_id,
                            short use_lock, remote_db_t * db);


// Txn mgmt:

uuid_t * remote_new_txn(remote_db_t * db);
int remote_validate_txn(uuid_t * txnid, int * minority_status, remote_db_t * db);
int remote_abort_txn(uuid_t * txnid, remote_db_t * db);
int remote_commit_txn(uuid_t * txnid, int * minority_status, remote_db_t * db);

// Txn state handling client-side:

txn_state * get_client_txn_state(uuid_t txnid, remote_db_t * db);
uuid_t * new_client_txn(remote_db_t * db, unsigned int * seedptr);
int close_client_txn(uuid_t txnid, remote_db_t * db);

// Gossip listener:

gossip_callback_args * get_gossip_callback_args(membership_state * ms, int status);
void free_gossip_callback_args(gossip_callback_args * qca);
gossip_callback * get_gossip_callback(void (*callback)(gossip_callback_args *));
int wait_on_gossip_callback(gossip_callback *);
void free_gossip_callback(gossip_callback * qc);
int listen_to_gossip(int status, int rack_id, int dc_id, char * hostname, unsigned short local_rts_id, remote_db_t * db);

// RTS mgmt:

typedef struct rts_descriptor
{
    char id[262];
    int rack_id;
    int dc_id;
    char * hostname;
    unsigned short local_rts_id;
    unsigned int _local_rts_index;
    struct sockaddr_in addr;
    int status;
} rts_descriptor;

rts_descriptor * get_rts_descriptor(int rack_id, int dc_id, char *hostname, int local_rts_id, int status);
void free_rts_descriptor(WORD rts_d);
WORD get_rts_key(WORD);
WORD get_rts_live_field(WORD);
int add_rts_to_membership(int rack_id, int dc_id, char *hostname, int local_rts_id, int node_status, skiplist_t * rtss, hash_ring * _rts_ring, unsigned int * seedptr);
char * to_string_rts_membership(remote_db_t * db, char * msg_buff);

// Actor mgmt:

#define ACTOR_STATUS_RUNNING 0
#define ACTOR_STATUS_MIGRATING 1
#define ACTOR_STATUS_STOPPED 2
static const char *Actor_status_name[] = {"running", "migrating", "stopped"};


typedef struct actor_descriptor
{
    long actor_id;
    rts_descriptor * host_rts;
    int is_local;
    int status;
} actor_descriptor;

actor_descriptor * get_actor_descriptor(long actor_id, rts_descriptor * host_rts, int is_local, int status);
void free_actor_descriptor(actor_descriptor * a);
int add_actor_to_membership(long actor_id, remote_db_t * db);
int update_actor_placement(remote_db_t * db);
int is_actor_local(long actor_id, remote_db_t * db);
skiplist_t * get_rtses_for_actor(long actor_id, remote_db_t * db);
rts_descriptor * get_first_rts_for_actor(long actor_id, remote_db_t * db);
skiplist_t * get_local_actors(remote_db_t * db);
skiplist_t * get_remote_actors(remote_db_t * db);
char * to_string_actor_membership(remote_db_t * db, char * msg_buff);

#endif /* BACKEND_CLIENT_API_H_ */
