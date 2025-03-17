/*
 * txns.h
 *
 *      Author: aagapi
 */

#ifndef BACKEND_TXNS_H_
#define BACKEND_TXNS_H_

#include "backend/txn_state.h"
#include "backend/queue.h"

#define VAL_STATUS_COMMIT 0
#define VAL_STATUS_ABORT 1

#define DUPLICATE_TXN -2
#define NO_SUCH_TXN -3


// DB queries:

txn_state * get_txn_state(uuid_t * txnid, db_t * db);
uuid_t * new_txn(db_t * db, unsigned int * seedptr);
int close_txn(uuid_t * txnid, db_t * db);
int close_txn_state(txn_state * ts, db_t * db);
int validate_txn(uuid_t * txnid, vector_clock * version, db_t * db);
int abort_txn(uuid_t * txnid, db_t * db);
int commit_txn(uuid_t * txnid, vector_clock * version, db_t * db, unsigned int * fastrandstate);

int db_insert_in_txn(WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, size_t blob_size, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);

db_row_t* db_search_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);

db_row_t* db_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key,
                                        uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
db_row_t* db_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
                                    WORD* col_keys, int no_columns, WORD table_key,
                                    uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
db_row_t* db_search_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);

int db_range_search_in_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
                            snode_t** start_row, snode_t** end_row,
                            WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int db_range_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys,
                                     WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
                                     snode_t** start_row, snode_t** end_row,
                                     WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int db_range_search_index_in_txn(int idx_idx, WORD start_idx_key, WORD end_idx_key,
                                snode_t** start_row, snode_t** end_row,
                                WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);

int db_delete_row_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int db_delete_cell_in_txn(WORD* keys, int no_primary_keys, int no_clustering_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int db_delete_by_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int db_update_in_txn(int * col_idxs, int no_cols, size_t blob_size, WORD * column_values, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);

// Queue ops:

int enqueue_in_txn(WORD * column_values, int no_cols, size_t blob_size, WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int read_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int max_entries, int * entries_read, int64_t * new_read_head,
        snode_t** start_row, snode_t** end_row, uuid_t * txnid,
        db_t * db, unsigned int * fastrandstate);
int consume_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                    int64_t new_consume_head, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int subscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
                        uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int unsubscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                                uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int create_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);
int delete_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate);

// Lower level API:

int persist_write(txn_write * tw, vector_clock * version, db_t * db, unsigned int * fastrandstate);
int persist_txn(txn_state * ts, db_t * db, unsigned int * fastrandstate);

#endif /* BACKEND_TXNS_H_ */
