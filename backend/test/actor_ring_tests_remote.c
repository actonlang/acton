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

// #include "txns.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <uuid/uuid.h>

#include "backend/client_api.h"
#include "backend/fastrand.h"


#define COLLECTION_ID_0 0 // rcv_counters map
#define COLLECTION_ID_1 1 // snd_counters map
#define COLLECTION_ID_2 2 // local counter variable as int
#define COLLECTION_ID_3 3 // local counter variable as char blob

int no_actors = 2;
int no_items = 20;

int min_no_state_cols = 3;
int no_state_primary_keys = 1;
int min_state_clustering_keys = 1;
int no_state_index_keys = 1;

int no_queue_cols = 2;

WORD state_table_key = (WORD) 0;
WORD queue_table_key = (WORD) 1;

int rand_sleep = 1;

int debug = 1;
int debug_lock = 0;

typedef struct actor_collection_item {
    int actor_id;
    int collection_id;
    int item_id;
    int item_value;
} actor_collection_item_t;

typedef struct actor_queue_item {
    int sender_id;
    int item_value;
} actor_queue_item_t;

typedef struct actor_args
{
    remote_db_t * db;
    WORD state_table_key;
    WORD queue_table_key;
    WORD queue_id;
    int no_enqueues;

    WORD consumer_id;
    WORD shard_id;
    WORD app_id;

    skiplist_t * rcv_counters;
    skiplist_t * snd_counters;
    int64_t total_rcv;
    int64_t total_snd;

    int successful_enqueues;
    int successful_dequeues;
    int successful_consumes;
    int successful_replays;

    int64_t read_head;
    int64_t read_head_after_replay;

    queue_callback * qc;

    db_schema_t * schema;

    int status;
} actor_args;

// Create schema:

db_schema_t* create_state_schema()
{
    int primary_key_idx = 0;
    int clustering_key_idxs[2];
    clustering_key_idxs[0]=1;
    clustering_key_idxs[1]=2;
    int index_key_idx=3;

    int * col_types = NULL;

    //  Col types are not enforced:
    /*
    col_types = (int *) malloc((no_state_cols+1) * sizeof(int));

    for(int i=0;i<no_state_cols;i++)
        col_types[i] = DB_TYPE_INT32;

    col_types[no_state_cols] = DB_TYPE_BLOB; // Include blob
    */

    db_schema_t* db_schema = db_create_schema(col_types, min_no_state_cols, &primary_key_idx, no_state_primary_keys, clustering_key_idxs, min_state_clustering_keys, &index_key_idx, no_state_index_keys);

    assert(db_schema != NULL && "Schema creation failed");

    return db_schema;
}

int create_queue_schema(remote_db_t * db, unsigned int * fastrandstate)
{
    assert(no_queue_cols == 2);
    int minority_status = 0;

    // Create input queues for all actors:

    int ret = 0;

    for(int64_t queue_id=0;queue_id<no_actors;queue_id++)
    {
        ret = remote_create_queue_in_txn(queue_table_key, (WORD) queue_id, &minority_status, NULL, db);
        printf("Test %s - %s (%d)\n", "create_queue", ret==0?"OK":"FAILED", ret);
    }

    return ret;
}


void consumer_callback(queue_callback_args * qca)
{
    printf("Consumer %" PRId64 "/%" PRId64 "/%" PRId64 " received notification for queue %" PRId64 "/%" PRId64 ", status %d\n",
            (int64_t) qca->app_id, (int64_t) qca->shard_id, (int64_t) qca->consumer_id,
            (int64_t) qca->table_key, (int64_t) qca->queue_id,
            qca->status);
}

int read_queue_while_not_empty(actor_args * ca, int * entries_read, snode_t ** start_row, snode_t ** end_row)
{
    int read_status = QUEUE_STATUS_READ_INCOMPLETE, minority_status = 0;

    while(read_status != QUEUE_STATUS_READ_COMPLETE)
    {
        read_status = remote_read_queue_in_txn(ca->consumer_id, ca->shard_id, ca->app_id,
                        ca->queue_table_key, ca->queue_id,
                        2, entries_read, &ca->read_head,
                        start_row, end_row, &minority_status, NULL, ca->db);

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
                        (int64_t) ca->consumer_id, ca->successful_dequeues, (int64_t) (*end_row)->key);

                if(((int64_t) (*end_row)->key) != ca->successful_dequeues - 1)
                    printf("Test %s - FAILED (%" PRId64 " != %d)\n", "last_entry_id", (int64_t) (*end_row)->key, ca->successful_dequeues - 1);
            }
        }
    }

    return read_status;
}

char digits[10][10] = { "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };

int checkpoint_local_state(actor_args * ca, uuid_t * txnid, unsigned int * fastrandstate)
{
    int ret = 0, minority_status = 0;

    WORD * column_values = (WORD *) malloc(4 * sizeof(WORD));

    column_values[0] = ca->consumer_id;

    // Checkpoint the 2 maps:

    for(snode_t * node = HEAD(ca->rcv_counters); node != NULL;node = NEXT(node))
    {
        column_values[1] = (WORD) COLLECTION_ID_0;
        column_values[2] = node->key;
        column_values[3] = node->value;
        char * str_value = ((int) node->value <= 9)?(digits[(int) node->value]):"NaN";

        ret = remote_insert_in_txn(column_values, 4, no_state_primary_keys, 2,
                                    (WORD) str_value, strnlen((const char *) str_value, 10) + 1,
                                    ca->state_table_key, &minority_status, txnid, ca->db);

        assert(ret == 0);
    }

    for(snode_t * node = HEAD(ca->snd_counters);node != NULL;node = NEXT(node))
    {
        column_values[1] = (WORD) COLLECTION_ID_1;
        column_values[2] = node->key;
        column_values[3] = node->value;
        char * str_value = ((int) node->value <= 9)?(digits[(int) node->value]):"NaN";

        ret = remote_insert_in_txn(column_values, 4, no_state_primary_keys, 2,
                                    (WORD) str_value, strnlen((const char *) str_value, 10) + 1,
                                    ca->state_table_key, &minority_status, txnid, ca->db);

        assert(ret == 0);
    }

    // Checkpoint the standalone counter variable, once in an int column, once in a char blob column:

    column_values[1] = (WORD) COLLECTION_ID_2;
    column_values[2] = (WORD) ca->total_rcv;

    ret = remote_insert_in_txn(column_values, 3, no_state_primary_keys, 1,
                                NULL, 0,
                                ca->state_table_key, &minority_status, txnid, ca->db);

    char * str_value = ((int) ca->total_rcv <= 9)?(digits[(int) ca->total_rcv]):"NaN";

    column_values[1] = (WORD) COLLECTION_ID_3;

    ret = remote_insert_in_txn(column_values, 2, no_state_primary_keys, 1,
                                (WORD) str_value, strnlen((const char *) str_value, 10) + 1,
                                ca->state_table_key, &minority_status, txnid, ca->db);

    free(column_values);

    return 0;
}


int send_seed_msgs(actor_args * ca, int * msgs_sent, unsigned int * fastrandstate)
{
    int ret = 0;
    int64_t dest_id = ((int64_t) ca->consumer_id < no_actors - 1)? ((int64_t) ca->consumer_id + 1) : 0;

    int no_outgoing_counters = 2;

    *msgs_sent=0;

    assert(no_queue_cols == 2);

    WORD * column_values = (WORD *) malloc(no_queue_cols * sizeof(WORD));
    int minority_status = 0;

    for(int i=0;i<no_outgoing_counters;i++)
    {
        column_values[0] = ca->consumer_id;
        column_values[1] = (WORD) i;
        char * str_value = (i <= 9)?(digits[i]):"NaN";

        ret = remote_enqueue_in_txn(column_values, no_queue_cols, (WORD) str_value, strnlen((const char *) str_value, 10) + 1, ca->queue_table_key, (WORD) dest_id, &minority_status, NULL, ca->db);

        assert(ret == 0);

        (*msgs_sent)++;

        skiplist_insert(ca->snd_counters, (WORD) dest_id, (WORD) i, fastrandstate);

        ca->total_snd++;
    }

    free(column_values);

    return 0;
}


int send_outgoing_msgs(actor_args * ca, int outgoing_counters[], int no_outgoing_counters, int * msgs_sent, uuid_t * txnid, unsigned int * fastrandstate)
{
    int ret = 0;
    int64_t dest_id = ((int64_t) ca->consumer_id < no_actors - 1)? ((int64_t) ca->consumer_id + 1) : 0;

/*
    if(debug)
        printf("ACTOR %" PRId64 ": Sending %d msgs to ACTOR %" PRId64 ".\n", (int64_t) ca->consumer_id, no_outgoing_counters, dest_id);
*/

    assert(no_queue_cols == 2);

    *msgs_sent=0;

    WORD * column_values = (WORD *) malloc(no_queue_cols * sizeof(WORD));
    int minority_status = 0;

    for(int i=0;i<no_outgoing_counters;i++)
    {
        column_values[0] = ca->consumer_id;
        column_values[1] = (WORD) outgoing_counters[i];
        char * str_value = (outgoing_counters[i] <= 9)?(digits[outgoing_counters[i]]):"NaN";

        ret = remote_enqueue_in_txn(column_values, no_queue_cols, (WORD) str_value, strnlen((const char *) str_value, 10) + 1, ca->queue_table_key, (WORD) dest_id, &minority_status, txnid, ca->db);

        assert(ret == 0);

        (*msgs_sent)++;
    }

    free(column_values);

    return 0;
}

int process_messages(snode_t * start_row, snode_t * end_row, int entries_read,
                        int outgoing_counters[], int * no_outgoing_counters,
                        actor_args * ca, unsigned int * fastrandstate)
{
    int ret = 0;
    int processed = 0;
    snode_t * crt_row = NULL;

    if(entries_read == 0 || start_row == NULL)
    {
        printf("ACTOR %" PRId64 ": No msgs to process!\n", (int64_t) ca->consumer_id);
        return 0;
    }

    if(debug)
        printf("ACTOR %" PRId64 ": %d msgs to process.\n", (int64_t) ca->consumer_id, entries_read);

    for(crt_row = start_row; processed<entries_read; crt_row = NEXT(crt_row), processed++)
    {
        db_row_t * db_row = (db_row_t *) crt_row->value;
//      print_long_row(db_row);

        int64_t queue_entry_id = (int64_t) db_row->key;
        assert(db_row->no_columns == 3);
        int64_t sender_id = (int64_t) db_row->column_array[0];
        int counter_val = (int) db_row->column_array[1];
        char * str_value = (char *) db_row->column_array[2];

        printf("ACTOR %" PRId64 ": Read queue entry: (id=%" PRId64 ", snd=%" PRId64 ", val=%d, str=%s)\n", (int64_t) ca->consumer_id, queue_entry_id, sender_id, counter_val, str_value);

//                  skiplist_search(ca->rcv_counters, COLLECTION_ID_0, (WORD) entries_read);

        skiplist_insert(ca->rcv_counters, (WORD) sender_id, (WORD) counter_val, fastrandstate);
        ca->total_rcv++;

        counter_val++;
        outgoing_counters[*no_outgoing_counters] = counter_val;
        *no_outgoing_counters = (*no_outgoing_counters) + 1;

        int64_t dest_id = ((int64_t) ca->consumer_id < no_actors - 1)? ((int64_t) ca->consumer_id + 1) : 0;

        skiplist_insert(ca->snd_counters, (WORD) dest_id, (WORD) counter_val, fastrandstate);
        ca->total_snd++;

        assert(processed < entries_read-1 || crt_row == end_row);
    }

    assert(processed == entries_read);

    return processed;
}

int produce_effects(uuid_t * txnid, actor_args * ca,
                    int * msgs_sent, int outgoing_counters[], int no_outgoing_counters,
                    unsigned int * fastrandstate)
{
    // Checkpoint local state in txn:

    int ret = checkpoint_local_state(ca, txnid, fastrandstate);

    assert(ret == 0);

    if(debug)
        printf("ACTOR %" PRId64 ": Chekpointed local state in txn.\n", (int64_t) ca->consumer_id);

    // Send outgoing msgs in txn:
    ret = send_outgoing_msgs(ca, outgoing_counters, no_outgoing_counters, msgs_sent, txnid, fastrandstate);

    assert(ret == 0);

    if(debug)
        printf("ACTOR %" PRId64 ": Sent %d outgoing msgs in txn.\n", (int64_t) ca->consumer_id, *msgs_sent);

    return 0;
}

void * actor(void * cargs)
{
    unsigned int seed, randno;
    int ret = 0, minority_status = 0;
    snode_t * start_row, * end_row;
    int msgs_sent = 0;
    int outgoing_counters[100];
    int no_outgoing_counters = 0;

    actor_args * ca = (actor_args *) cargs;

    queue_callback * qc = ca->qc;

    GET_RANDSEED(&seed, 0); // thread_id

    int64_t prev_read_head = -1, prev_consume_head = -1;
    int minority_status = 0;
    ret = remote_subscribe_queue(ca->consumer_id, ca->shard_id, ca->app_id, ca->queue_table_key, ca->queue_id, qc,
                            &prev_read_head, &prev_consume_head, &minority_status, ca->db);
    printf("Test %s - %s (%d)\n", "subscribe_queue", ret==0?"OK":"FAILED", ret);
    if(ret)
        return NULL;

    if(debug)
        printf("ACTOR %" PRId64 ": Subscribed to queue %" PRId64 "/%" PRId64 " with callback (%p/%p/%p/%p)\n", (int64_t) ca->consumer_id, (int64_t) ca->queue_table_key, (int64_t) ca->queue_id, qc, qc->lock, qc->signal, qc->callback);

    ca->rcv_counters = create_skiplist_long();
    ca->snd_counters = create_skiplist_long();

    int entries_read = (int) prev_read_head + 1;

    if((int64_t) ca->consumer_id == 0)
    {
        ret = send_seed_msgs(ca, &msgs_sent, &seed);
        ca->successful_enqueues += msgs_sent;
        if(debug)
            printf("ACTOR %" PRId64 ": sent %d seed outgoing msgs (status = %d).\n", (int64_t) ca->consumer_id, msgs_sent, ret);
//      remote_print_long_table(state_table_key, ca->db);
//      remote_print_long_table(queue_table_key, ca->db);
    }

    int read_status = read_queue_while_not_empty(ca, &entries_read, &start_row, &end_row);
    if(read_status < 0)
    {
        return (void *) read_status;
    }

    // Add app-specific message processing work here:

    no_outgoing_counters = 0;
    ret = process_messages(start_row, end_row, entries_read, outgoing_counters, &no_outgoing_counters, ca, &seed);
    int minority_status = 0;

    if(entries_read > 0)
    {
        int checkpoint_success = 0;
        while(!checkpoint_success)
        {
            uuid_t * txnid = remote_new_txn(ca->db);

            ret = produce_effects(txnid, ca, &msgs_sent, outgoing_counters, no_outgoing_counters, &seed);

            assert(ret == 0);

            // Consume input queue in same txn:

            ret = remote_consume_queue_in_txn(ca->consumer_id, ca->shard_id, ca->app_id, ca->queue_table_key, ca->queue_id,
                                        (int64_t) ca->read_head, &minority_status, txnid, ca->db);

            if(ret < 0 && ret != DB_ERR_QUEUE_COMPLETE)
                printf("ERROR: consume_queue returned %d\n", ret);

            if(debug)
                printf("ACTOR %" PRId64 ": consumed input queue up to %" PRId64 " in txn.\n", (int64_t) ca->consumer_id, (int64_t) ca->read_head);

            ret = remote_commit_txn(txnid, &minority_status, ca->db);

            if(debug)
                printf("ACTOR %" PRId64 ": Commit returned %d.\n", (int64_t) ca->consumer_id, ret);

            checkpoint_success = (ret == VAL_STATUS_COMMIT);
        }

//      remote_print_long_table(state_table_key, ca->db);
//      remote_print_long_table(queue_table_key, ca->db);

        ca->successful_consumes = ca->successful_dequeues;
        ca->successful_enqueues += msgs_sent;

        printf("ACTOR %" PRId64 ": successful_dequeues=%d, successful_consumes=%d, no_enqueues=%d\n",
                (int64_t) ca->consumer_id, ca->successful_dequeues, ca->successful_consumes, ca->no_enqueues);
    }

    while(ca->successful_consumes < ca->no_enqueues)
    {
        if(debug)
            printf("ACTOR %" PRId64 ": Blocking for input (successful_consumes=%d, no_enqueues=%d)\n", (int64_t) ca->consumer_id, ca->successful_consumes, ca->no_enqueues);

        ret = wait_on_queue_callback(qc);

        if(ret == 0)
        {
            if(debug)
                printf("ACTOR %" PRId64 ": Was signaled, status=%d, reading queue..\n", (int64_t) ca->consumer_id, ret);
        }
        else
        {
            if(debug)
                printf("ACTOR %" PRId64 ": Wait timed out, status=%d, reading queue..\n", (int64_t) ca->consumer_id, ret);
        }

        // Received queue notification, reading:

        read_status = read_queue_while_not_empty(ca, &entries_read, &start_row, &end_row);
        if(read_status < 0)
        {
            return (void *) read_status;
        }

        no_outgoing_counters = 0;
        ret = process_messages(start_row, end_row, entries_read, outgoing_counters, &no_outgoing_counters, ca, &seed);

        if(entries_read > 0)
        {
            int checkpoint_success = 0;
            while(!checkpoint_success)
            {
                uuid_t * txnid = remote_new_txn(ca->db);

                ret = produce_effects(txnid, ca, &msgs_sent, outgoing_counters, no_outgoing_counters, &seed);

                // Consume input queue in same txn:

                ret = remote_consume_queue_in_txn(ca->consumer_id, ca->shard_id, ca->app_id, ca->queue_table_key, ca->queue_id,
                                            (int64_t) ca->read_head, &minority_status, txnid, ca->db);

                if(ret < 0 && ret != DB_ERR_QUEUE_COMPLETE)
                    printf("ERROR: consume_queue returned %d\n", ret);

                if(debug)
                    printf("ACTOR %" PRId64 ": consumed input queue up to %" PRId64 " in txn.\n", (int64_t) ca->consumer_id, (int64_t) ca->read_head);

                ret = remote_commit_txn(txnid, ca->db);

                if(debug)
                    printf("ACTOR %" PRId64 ": Commit returned %d.\n", (int64_t) ca->consumer_id, ret);

                checkpoint_success = (ret == VAL_STATUS_COMMIT);
            }

//          remote_print_long_table(state_table_key, ca->db);
//          remote_print_long_table(queue_table_key, ca->db);

            ca->successful_consumes = ca->successful_dequeues;
            ca->successful_enqueues += msgs_sent;

            printf("ACTOR %" PRId64 ": successful_dequeues=%d, successful_consumes=%d, successful_enqueues=%d, private_read_head=%" PRId64 ", no_enqueues=%d\n",
                    (int64_t) ca->consumer_id, ca->successful_dequeues, ca->successful_consumes, ca->successful_enqueues, ca->read_head, ca->no_enqueues);

//          print_long_db(ca->db);
        }

        if(rand_sleep)
        {
            FASTRAND(&seed, randno);
            sleep((randno % 10) * 0.2);
        }
    }

    ret = remote_unsubscribe_queue(ca->consumer_id, ca->shard_id, ca->app_id, ca->queue_table_key, ca->queue_id, &minority_status, ca->db);
    printf("Test %s - %s (%d)\n", "unsubscribe_queue", ret==0?"OK":"FAILED", ret);

//  free_queue_callback(qc);

    return (void *) ret;
}

int main(int argc, char **argv) {
    char *hostname;
    int portno;
    unsigned int seed;
    int ret = 0;

    GET_RANDSEED(&seed, 0); // thread_id

    // Get db pointer:

    /* check command line arguments */
    if (argc != 5) {
       fprintf(stderr,"usage: %s <hostname> <port> <no_actors> <no_enqueues>\n", argv[0]);
       exit(0);
    }
    hostname = argv[1];
    portno = atoi(argv[2]);
    no_actors = atoi(argv[3]);
    no_items = atoi(argv[4]);

    remote_db_t * db = get_remote_db(1);

    add_server_to_membership(hostname, portno, db, &seed);
//    add_server_to_membership(hostname, portno+1, db, &seed);
//    add_server_to_membership(hostname, portno+2, db, &seed);

    // Create state table:

    db_schema_t* schema = create_state_schema();

    // Create queue table:

    ret = create_queue_schema(db, &seed);

    // Create and run producer and consumer threads (also test subscribe / unsubscribe):

    pthread_t actor_ts[50];
    actor_args cargs[50];
    int node_ids[50];
    memset(&node_ids, 0, 50*sizeof(int));
    int64_t counters[50];
    memset(&counters, 0, 50*sizeof(int64_t));

    for(int i=0;i<no_actors;i++)
    {
        node_ids[i] = i;
    }

    for(int i=0;i<no_actors;i++)
    {
        memset(&(cargs[i]), 0, sizeof(actor_args));
        cargs[i].db = db;
        cargs[i].app_id = (WORD) 0;
        cargs[i].shard_id = (WORD) 0;
        cargs[i].consumer_id = (WORD) node_ids[i];
        cargs[i].state_table_key = state_table_key;
        cargs[i].queue_table_key = queue_table_key;
        cargs[i].queue_id = cargs[i].consumer_id;
        cargs[i].no_enqueues = no_items;
        cargs[i].qc = get_queue_callback(consumer_callback);
        cargs[i].schema = schema;

        ret = pthread_create(actor_ts+i, NULL, actor, &(cargs[i]));
        printf("Test %s (%d) - %s (%d)\n", "create_actor_thread", i, ret==0?"OK":"FAILED", ret);
        if(ret)
            return -1;
    }

    for(int i=0;i<no_actors;i++)
    {
        ret = pthread_join(actor_ts[i], NULL);
        printf("Test %s (%d) - %s (%d)\n", "join_actor_thread", i, ret==0?"OK":"FAILED", ret);
        if(ret)
            return -2;
    }

    for(int i=0;i<no_actors;i++)
    {
        int num_enqueues = cargs[i].no_enqueues+((i==0)?2:0);

        // Test enqueues:
        printf("Test %s (%d) - %s (%d)\n", "enqueue", i, (cargs[i].successful_enqueues>=num_enqueues && cargs[i].successful_enqueues<=num_enqueues+1)?"OK":"FAILED", ret);

        // Test dequeues:
        printf("Test %s (%d) - %s (%d)\n", "dequeue", i, cargs[i].successful_dequeues==cargs[i].successful_enqueues-((i==0)?2:0)?"OK":"FAILED", ret);

        // Test read head sanity:
        printf("Test %s (%d) - %s (%d)\n", "read_head", i, ((int) cargs[i].read_head)==(cargs[i].successful_dequeues - 1)?"OK":"FAILED", ret);

        // Test consumes:
        printf("Test %s (%d) - %s (%d)\n", "consume", i, cargs[i].successful_consumes==cargs[i].successful_dequeues?"OK":"FAILED", ret);
    }

    remote_print_long_table(state_table_key, db);
    remote_print_long_table(queue_table_key, db);

    return 0;
}



