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
 * queue.h
 *
 *      Author: aagapi
 */

#ifndef BACKEND_QUEUE_H_
#define BACKEND_QUEUE_H_

#include "backend/db.h"
#include "backend/queue_callback.h"
#include "backend/consumer_state.h"
#include "backend/queue_groups.h"

#define DB_ERR_NO_TABLE -1
#define DB_ERR_NO_QUEUE -2
#define DB_ERR_NO_CONSUMER -3
#define DB_ERR_QUEUE_COMPLETE -4
#define DB_ERR_QUEUE_HEAD_INVALID -5
#define DB_ERR_DUPLICATE_QUEUE -6
#define DB_ERR_DUPLICATE_CONSUMER -7
#define DB_ERR_NO_GROUP -8

#define QUEUE_STATUS_READ_INCOMPLETE 0
#define QUEUE_STATUS_READ_COMPLETE 1

#define QUEUE_NOTIF_ENQUEUED 0
#define QUEUE_NOTIF_DELETED 1
#define GROUP_NOTIF_ENQUEUED 2

#define READ_HEAD(cs) (cs->private_read_head)
#define CONSUME_HEAD(cs) (cs->private_consume_head)
#define READ_HEAD_VERSION(cs) (cs->prh_version)
#define CONSUME_HEAD_VERSION(cs) (cs->pch_version)

int enqueue(WORD * column_values, int no_cols, size_t last_blob_size, WORD table_key, WORD queue_id, short use_lock, db_t * db, unsigned int * fastrandstate);
int read_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int max_entries, int * entries_read, int64_t * new_read_head, vector_clock ** prh_version,
        snode_t** start_row, snode_t** end_row, short use_lock,
        db_t * db);
int peek_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int max_entries, int64_t offset, int * entries_read, int64_t * new_read_head, vector_clock ** prh_version,
        snode_t** start_row, snode_t** end_row, db_t * db);
int replay_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int64_t replay_offset, int max_entries,
        int * entries_read, int64_t * new_replay_offset,
        snode_t** start_row, snode_t** end_row,
        db_t * db);
int consume_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                    int64_t new_consume_head, db_t * db);
int subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
                        short use_lock, db_t * db, unsigned int * fastrandstate);
int __subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, db_row_t * db_row, WORD group_id,
                    queue_callback * callback, int * sockfd, int64_t * prev_read_head, int64_t * prev_consume_head,
                    short use_lock, db_t * db, unsigned int * fastrandstate);
int register_remote_subscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, WORD group_id,
                    int * sockfd, int64_t * prev_read_head, int64_t * prev_consume_head,
                    short use_lock, db_t * db, unsigned int * fastrandstate);
int register_remote_unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, WORD group_id,
                                        short use_lock, db_t * db);
int unsubscribe_queue(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, WORD group_id,
                        short use_lock, db_t * db);
void create_headers_for_group_subscribers(db_row_t * db_row, db_t * db, unsigned int * fastrandstate);
int create_queue(WORD table_key, WORD queue_id, vector_clock * version, short use_lock,
                    db_t * db, unsigned int * fastrandstate);
int delete_queue(WORD table_key, WORD queue_id, vector_clock * version, short use_lock, db_t * db, unsigned int * fastrandstate);
int create_queue_table(WORD table_id, int no_cols, int * col_types,
                        db_t * db, unsigned int * fastrandstate);
int set_private_read_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                            int64_t new_read_head, vector_clock * version, short use_lock, db_t * db);
int set_private_consume_head(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                            int64_t new_consume_head, vector_clock * version, db_t * db);

consumer_state * get_consumer_state(WORD consumer_id, WORD shard_id, WORD app_id, WORD group_id, queue_callback* callback, int * sockfd, int is_group_subscription);
void free_consumer_state(consumer_state * cs);
void free_consumer_state_sl(void * cs);

int add_consumer_state_to_group(WORD queue_id, consumer_state * cs, group_state *gs, unsigned int * fastrandstate);
int get_consumer_state_from_group(WORD queue_id, group_state * gs, consumer_state ** cs);
int pop_consumer_state_from_group(WORD queue_id, group_state * gs, consumer_state ** cs);

void free_queue_table_state(WORD queue_table_state);

#endif /* BACKEND_QUEUE_H_ */
