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

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <assert.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <time.h>

#include "backend/queue.h"

int no_cols = 2;
int no_items = 10;
WORD table_key = (WORD) 0;
WORD queue_id = (WORD) 0;

typedef struct actor_collection_item {
    int item_id;
    int item_value;
} actor_collection_item_t;

typedef struct producer_args
{
    db_t * db;
    WORD table_key;
    WORD queue_id;
    int no_enqueues;

    int successful_enqueues;
    int status;
} producer_args;

typedef struct consumer_args
{
    db_t * db;
    WORD table_key;
    WORD queue_id;
    int no_enqueues;

    WORD consumer_id;
    WORD shard_id;
    WORD app_id;

    int successful_dequeues;
    int successful_consumes;
    int successful_replays;

    int64_t read_head;
    int64_t read_head_after_replay;

    int status;
} consumer_args;


// Enqueue test:

int do_enqueues(db_t * db, WORD table_id, WORD queue_id, int no_enqueues, int rand_sleep, int * successful_enqueues, unsigned int * fastrandstate) {
    unsigned int randno;

    for(int64_t iid=0;iid<no_enqueues;iid++)
    {
        WORD * column_values = (WORD *) malloc(no_cols * sizeof(WORD));

        column_values[0] = (WORD) iid;
        column_values[1] = (WORD) iid + 1;

        int ret = enqueue(column_values, no_cols, 0, table_id, queue_id, 1, db, fastrandstate);
        if(ret != 0)
            return ret;

        (*successful_enqueues)++;

        if(rand_sleep)
        {
            FASTRAND(fastrandstate, randno);
            sleep((randno % 10) * 0.2);
        }
    }

    return 0;
}

void * producer(void * pargs)
{
    unsigned int seed;
    int ret = 0;

    producer_args * pa = (producer_args *) pargs;

    GET_RANDSEED(&seed, 0); // thread_id

    ret = do_enqueues(pa->db, pa->table_key, pa->queue_id, pa->no_enqueues, 1, &(pa->successful_enqueues), &seed);

    pa->status = ret;

    return (void *) pa->status;
}

void consumer_callback(queue_callback_args * qca)
{
    printf("Consumer %" PRId64 "/%" PRId64 "/%" PRId64 " received notification for queue %" PRId64 "/%" PRId64 ", status %d\n",
            (int64_t) qca->app_id, (int64_t) qca->shard_id, (int64_t) qca->consumer_id,
            (int64_t) qca->table_key, (int64_t) qca->queue_id,
            qca->status);
}

int read_queue_while_not_empty(consumer_args * ca, int * entries_read)
{
    snode_t * start_row, * end_row;
    int read_status = QUEUE_STATUS_READ_INCOMPLETE;
    vector_clock * prh_version;

    while(read_status != QUEUE_STATUS_READ_COMPLETE)
    {
        read_status = read_queue(ca->consumer_id, ca->shard_id, ca->app_id,
                        ca->table_key, ca->queue_id,
                        2, entries_read, &ca->read_head, &prh_version,
                        &start_row, &end_row, 1, ca->db);

        if(read_status < 0)
        {
            printf("ERROR: read_queue returned %d\n", read_status);
            return read_status;
        }
        else
        {
            assert(read_status == QUEUE_STATUS_READ_COMPLETE || read_status == QUEUE_STATUS_READ_INCOMPLETE);

            ca->successful_dequeues += (*entries_read);

            if((*entries_read) > 0)
            {
                printf("CONSUMER %" PRId64 ": successful_dequeues=%d, last_entry_id=%" PRId64 "\n",
                        (int64_t) ca->consumer_id, ca->successful_dequeues, (int64_t) end_row->key);

                if(((int64_t) end_row->key) != ca->successful_dequeues - 1)
                    printf("Test %s - FAILED (%" PRId64 " != %d)\n", "last_entry_id", (int64_t) end_row->key, ca->successful_dequeues - 1);
            }
        }
    }

    return read_status;
}

void * consumer(void * cargs)
{
    unsigned int seed;
    int ret = 0;
    snode_t * start_row, * end_row;

    consumer_args * ca = (consumer_args *) cargs;

    pthread_cond_t signal = PTHREAD_COND_INITIALIZER;
    pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

    GET_RANDSEED(&seed, 0); // thread_id

    queue_callback qc;
    qc.lock = &lock;
    qc.signal = &signal;
    qc.callback = consumer_callback;

    int64_t prev_read_head = -1, prev_consume_head = -1;
    ret = subscribe_queue(ca->consumer_id, ca->shard_id, ca->app_id, ca->table_key, ca->queue_id, &qc,
                            &prev_read_head, &prev_consume_head, 1, ca->db, &seed);
    printf("Test %s - %s (%d)\n", "subscribe_queue", ret==0?"OK":"FAILED", ret);
    if(ret)
        return NULL;

    int entries_read = (int) prev_read_head + 1;

    int read_status = read_queue_while_not_empty(ca, &entries_read);
    if(read_status < 0)
    {
        return (void *) read_status;
    }

    // Add app-specific message processing work here

    ret = consume_queue(ca->consumer_id, ca->shard_id, ca->app_id,
                        ca->table_key, ca->queue_id,
                        (int64_t) ca->read_head, ca->db);

    if(ret < 0 && ret != DB_ERR_QUEUE_COMPLETE)
        printf("ERROR: consume_queue returned %d\n", ret);
    else
        ca->successful_consumes = ca->successful_dequeues;

    printf("CONSUMER %" PRId64 ": successful_dequeues=%d, successful_consumes=%d\n",
            (int64_t) ca->consumer_id, ca->successful_dequeues, ca->successful_consumes);

    while(ca->successful_consumes < ca->no_enqueues)
    {
        pthread_mutex_lock(&lock);
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += 3;
        pthread_cond_timedwait(&signal, &lock, &ts);
        // Received queue notification, reading:

        read_status = read_queue_while_not_empty(ca, &entries_read);
        if(read_status < 0)
        {
            return (void *) read_status;
        }

        // Add app-specific message processing work here

        ret = consume_queue(ca->consumer_id, ca->shard_id, ca->app_id,
                            ca->table_key, ca->queue_id,
                            (int64_t) ca->read_head, ca->db);

        if(ret < 0 && ret != DB_ERR_QUEUE_COMPLETE)
            printf("ERROR: consume_queue returned %d\n", ret);
        else
            ca->successful_consumes = ca->successful_dequeues;

        printf("CONSUMER %" PRId64 ": successful_dequeues=%d, successful_consumes=%d\n",
                (int64_t) ca->consumer_id, ca->successful_dequeues, ca->successful_consumes);

        pthread_mutex_unlock(&lock);
    }

    ret = unsubscribe_queue(ca->consumer_id, ca->shard_id, ca->app_id, ca->table_key, ca->queue_id, 1, ca->db);
    printf("Test %s - %s (%d)\n", "unsubscribe_queue", ret==0?"OK":"FAILED", ret);

    return (void *) ret;
}

void * consumer_replay(void * cargs)
{
    unsigned int seed;
    int ret = 0;
    snode_t * start_row, * end_row;

    consumer_args * ca = (consumer_args *) cargs;

    pthread_cond_t signal = PTHREAD_COND_INITIALIZER;
    pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

    GET_RANDSEED(&seed, 0); // thread_id

    queue_callback qc;
    qc.lock = &lock;
    qc.signal = &signal;
    qc.callback = consumer_callback;

    int64_t prev_read_head = -1, prev_consume_head = -1;
    ret = subscribe_queue(ca->consumer_id, ca->shard_id, ca->app_id, ca->table_key, ca->queue_id, &qc,
                            &prev_read_head, &prev_consume_head, 1, ca->db, &seed);

    int entries_read = (int) prev_read_head + 1;

    int read_status = read_queue_while_not_empty(ca, &entries_read);
    if(read_status < 0)
    {
        return (void *) read_status;
    }

    while(ca->successful_dequeues < ca->no_enqueues)
    {
        pthread_mutex_lock(&lock);
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += 3;
        pthread_cond_timedwait(&signal, &lock, &ts);

        // Received queue notification, reading:

        read_status = read_queue_while_not_empty(ca, &entries_read);
        if(read_status < 0)
        {
            return (void *) read_status;
        }

        pthread_mutex_unlock(&lock);
    }

    ret = replay_queue(ca->consumer_id, ca->shard_id, ca->app_id,
            ca->table_key, ca->queue_id,
            0, ca->successful_dequeues,
            &ca->successful_replays, &ca->read_head_after_replay,
            &start_row, &end_row, ca->db);

    assert(ret == QUEUE_STATUS_READ_COMPLETE);

    ret = unsubscribe_queue(ca->consumer_id, ca->shard_id, ca->app_id, ca->table_key, ca->queue_id, 1, ca->db);
    printf("Test %s - %s (%d)\n", "unsubscribe_queue", ret==0?"OK":"FAILED", ret);

    return (void *) ret;
}


int main(int argc, char **argv) {
    unsigned int seed;
    int ret = 0;

    GET_RANDSEED(&seed, 0); // thread_id

    // Get db pointer:

    db_t * db = get_db();

    // Create queue table:

    int * col_types = (int *) malloc(no_cols * sizeof(int));
    for(int i=0;i<no_cols;i++)
        col_types[i] = DB_TYPE_INT64;

    ret = create_queue_table(table_key, no_cols, col_types, db,  &seed);
    printf("Test %s - %s (%d)\n", "create_queue_table", ret==0?"OK":"FAILED", ret);

    // Create queue:

    ret = create_queue(table_key, queue_id, NULL, 1, db, &seed);
    printf("Test %s - %s (%d)\n", "create_queue", ret==0?"OK":"FAILED", ret);

    // Create and run producer and consumer threads (also test subscribe / unsubscribe):

    pthread_t producer_t, consumer_t, consumer2_t;

    producer_args pargs;
    memset(&pargs, 0, sizeof(producer_args));
    pargs.db = db;
    pargs.table_key = table_key;
    pargs.queue_id = queue_id;
    pargs.no_enqueues = no_items;

    consumer_args cargs;
    memset(&cargs, 0, sizeof(consumer_args));
    cargs.db = db;
    cargs.table_key = table_key;
    cargs.queue_id = queue_id;
    cargs.no_enqueues = no_items;
    cargs.app_id = (WORD) 0;
    cargs.shard_id = (WORD) 0;
    cargs.consumer_id = (WORD) 0;

    consumer_args cargs_replay;
    memset(&cargs_replay, 0, sizeof(consumer_args));
    cargs_replay.db = db;
    cargs_replay.table_key = table_key;
    cargs_replay.queue_id = queue_id;
    cargs_replay.no_enqueues = no_items;
    cargs_replay.app_id = (WORD) 0;
    cargs_replay.shard_id = (WORD) 0;
    cargs_replay.consumer_id = (WORD) 1;

    ret = pthread_create(&producer_t, NULL, producer, &pargs);
    printf("Test %s - %s (%d)\n", "create_producer_thread", ret==0?"OK":"FAILED", ret);
    if(ret)
        return -1;

    ret = pthread_create(&consumer_t, NULL, consumer, &cargs);
    printf("Test %s - %s (%d)\n", "create_consumer_thread", ret==0?"OK":"FAILED", ret);
    if(ret)
        return -1;

    ret = pthread_create(&consumer2_t, NULL, consumer_replay, &cargs_replay);
    printf("Test %s - %s (%d)\n", "create_consumer_replay_thread", ret==0?"OK":"FAILED", ret);
    if(ret)
        return -1;

    ret = pthread_join(producer_t, NULL);
    printf("Test %s - %s (%d)\n", "join_producer_thread", ret==0?"OK":"FAILED", ret);
    if(ret)
        return -2;

    ret = pthread_join(consumer_t, NULL);
    printf("Test %s - %s (%d)\n", "join_consumer_thread", ret==0?"OK":"FAILED", ret);
    if(ret)
        return -2;

    ret = pthread_join(consumer2_t, NULL);
    printf("Test %s - %s (%d)\n", "join_consumer_replay_thread", ret==0?"OK":"FAILED", ret);
    if(ret)
        return -2;

    // Test enqueues:
    printf("Test %s - %s (%d)\n", "enqueue", pargs.successful_enqueues==pargs.no_enqueues?"OK":"FAILED", ret);

    // Test dequeues:
    printf("Test %s - %s (%d)\n", "dequeue", cargs.successful_dequeues==cargs.no_enqueues?"OK":"FAILED", ret);

    // Test read head sanity:
    printf("Test %s - %s (%d)\n", "read_head", ((int) cargs.read_head)==(cargs.no_enqueues - 1)?"OK":"FAILED", ret);

    // Test consumes:
    printf("Test %s - %s (%d)\n", "consume", cargs.successful_consumes==cargs.no_enqueues?"OK":"FAILED", ret);


    // Test dequeues on C2:
    printf("Test %s - %s (%d)\n", "dequeue", cargs_replay.successful_dequeues==cargs_replay.no_enqueues?"OK":"FAILED", ret);

    // Test read head sanity on C2:
    printf("Test %s - %s (%d)\n", "read_head", ((int) cargs_replay.read_head)==(cargs_replay.no_enqueues - 1)?"OK":"FAILED", ret);

    // Test replays on C2:
    printf("Test %s - %s (%d)\n", "replay", cargs_replay.successful_replays==cargs_replay.no_enqueues?"OK":"FAILED", ret);

    // Test read head sanity after replay on C2:
    printf("Test %s - %s (%d)\n", "read_head_replay", ((int) cargs_replay.read_head_after_replay)==(cargs_replay.no_enqueues - 1)?"OK":"FAILED", ret);

    // Test delete queue:

    ret = delete_queue(table_key, queue_id, NULL, 1, db, &seed);
    printf("Test %s - %s (%d)\n", "delete_queue", ret==0?"OK":"FAILED", ret);

    return 0;
}




