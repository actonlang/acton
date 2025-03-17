/*
 * txn_state.h
 */

#ifndef BACKEND_TXN_STATE_H_
#define BACKEND_TXN_STATE_H_

#include <uuid/uuid.h>

#include "backend/db.h"
#include "backend/queue_callback.h"

#define TXN_STATUS_ACTIVE 0
#define TXN_STATUS_VALIDATED 1
// #define TXN_STATUS_COMMITTED 2
// #define TXN_STATUS_ABORTED 3


typedef struct txn_write
{
    short query_type;
    WORD table_key;

    WORD * column_values;
    int no_cols;
    int no_primary_keys;
    int no_clustering_keys;
    size_t blob_size;

    // For queue ops:

    WORD queue_id;
    WORD consumer_id;
    WORD shard_id;
    WORD app_id;

    int64_t new_read_head;  // read_queue (out)
    int64_t new_consume_head; // consume_queue (in)

    vector_clock * prh_version;
//  vector_clock * pch_version;

    int64_t local_order;
} txn_write;

typedef struct txn_read
{
    WORD table_key;

    WORD* start_primary_keys;
    WORD* end_primary_keys;
    int no_primary_keys;

    WORD* start_clustering_keys;
    WORD* end_clustering_keys;
    int no_clustering_keys;

    WORD* col_keys;
    int no_col_keys;

    // For idx queries:

    int idx_idx;

    short query_type;

    // For non-range queries:

    vector_clock * result_version;

    // For range queries:

    int64_t * range_result_keys;
    vector_clock ** range_result_versions;
    int no_range_results;

    snode_t* start_row;
    snode_t* end_row;

    int64_t local_order;
} txn_read;

typedef struct txn_state
{
    uuid_t txnid;
    skiplist_t * read_set;
    skiplist_t * write_set;
    short state;

    vector_clock * version;
} txn_state;

int txn_write_cmp(WORD e1, WORD e2);
int txn_read_cmp(WORD e1, WORD e2);

txn_state * init_txn_state();
void free_txn_state(txn_state * ts);
void set_version(txn_state * ts, vector_clock * vc);

txn_write * get_txn_write(short query_type, WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, size_t blob_size, WORD table_key, int64_t local_order);
txn_write * get_dummy_txn_write(short query_type, WORD * primary_keys, int no_primary_keys, WORD * clustering_keys, int no_clustering_keys, WORD table_key, int64_t local_order);
void free_txn_write(txn_write * tw);
txn_read * get_txn_read(short query_type,
                        WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
                        WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
                        WORD* col_keys, int no_col_keys,
                        int idx_idx,
                        vector_clock * result_version,
                        int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results,
                        WORD table_key, int64_t local_order);
void free_txn_read(txn_read * tr);

// Txn ops mgmt API:

int add_write_to_txn(short query_type, WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, size_t blob_size,
                    WORD table_key, txn_state * ts, unsigned int * fastrandstate);
int add_row_read_to_txn(WORD* primary_keys, int no_primary_keys,
                        WORD table_key, db_row_t* result,
                        txn_state * ts, unsigned int * fastrandstate);
int add_row_range_read_to_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys, WORD table_key,
                                snode_t* start_row, snode_t* end_row, int no_results,
                                txn_state * ts, unsigned int * fastrandstate);
int add_cell_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
                                WORD table_key, db_row_t* result,
                                txn_state * ts, unsigned int * fastrandstate);
int add_cell_range_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys,
                                WORD* end_clustering_keys, int no_clustering_keys,
                                WORD table_key,
                                snode_t* start_row, snode_t* end_row, int no_results,
                                txn_state * ts, unsigned int * fastrandstate);
int add_col_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
                                WORD* col_keys, int no_columns,
                                WORD table_key, db_row_t* result,
                                txn_state * ts, unsigned int * fastrandstate);
int add_index_read_to_txn(WORD* index_key, int idx_idx, WORD table_key, db_row_t* result, txn_state * ts, unsigned int * fastrandstate);
int add_index_range_read_to_txn(int idx_idx, WORD* start_idx_key, WORD* end_idx_key, snode_t* start_row, snode_t* end_row, int no_results, WORD table_key, txn_state * ts, unsigned int * fastrandstate);

// Queue ops:

int add_enqueue_to_txn(WORD * column_values, int no_cols, size_t blob_size, WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate);
int add_read_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                            int64_t new_read_head, vector_clock * prh_version, txn_state * ts, unsigned int * fastrandstate);
int add_consume_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                    int64_t new_consume_head, txn_state * ts, unsigned int * fastrandstate);
int add_create_queue_to_txn(WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate);
int add_delete_queue_to_txn(WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate);
int add_subscribe_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
                        txn_state * ts, unsigned int * fastrandstate);
int add_unsubscribe_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id, txn_state * ts);

#endif /* BACKEND_TXN_STATE_H_ */
