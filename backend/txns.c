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
 * txns.c
 *
 *      Author: aagapi
 */

#include <stdio.h>

#include "backend/txns.h"
#include "backend/log.h"

#define VERBOSE_TXNS 1
#define VERBOSE_TXNS_PERSIST 0

// DB queries:

txn_state * get_txn_state(uuid_t * txnid, db_t * db)
{
    snode_t * txn_node = (snode_t *) skiplist_search(db->txn_state, (WORD) (*txnid));

    return (txn_node != NULL)? (txn_state *) txn_node->value : NULL;
}

uuid_t * new_txn(db_t * db, unsigned int * seedptr)
{
    txn_state * ts = NULL, * previous = NULL;

#if (MULTI_THREADED == 1)
    pthread_mutex_lock(db->txn_state_lock);
#endif

    while(ts == NULL)
    {
        ts = init_txn_state();
        previous = get_txn_state(&(ts->txnid), db);
        if(previous != NULL)
        {
            free_txn_state(ts);
            ts = NULL;
        }
    }

    skiplist_insert(db->txn_state, (WORD) ts->txnid, (WORD) ts, seedptr);

#if (MULTI_THREADED == 1)
    pthread_mutex_unlock(db->txn_state_lock);
#endif

    return &(ts->txnid);
}

int close_txn_state(txn_state * ts, db_t * db)
{
#if (MULTI_THREADED == 1)
    pthread_mutex_lock(db->txn_state_lock);
#endif

    skiplist_delete(db->txn_state, ts->txnid);
    free_txn_state(ts);

#if (MULTI_THREADED == 1)
    pthread_mutex_unlock(db->txn_state_lock);
#endif

    return 0;
}

int close_txn(uuid_t * txnid, db_t * db)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    return close_txn_state(ts, db);
}

int key_path_overlaps(txn_read * tr, txn_write * tw)
{
    // Writes must be at least as specific as reads:
    assert(tr->no_primary_keys + tr->no_clustering_keys <= tw->no_primary_keys + tw->no_clustering_keys);

    int is_exact_read_query = (tr->query_type == QUERY_TYPE_READ_COLS || tr->query_type == QUERY_TYPE_READ_CELL || tr->query_type == QUERY_TYPE_READ_ROW);

    if(tr->query_type == QUERY_TYPE_READ_INDEX)
    {
        if((int64_t) tw->column_values[tr->idx_idx] == (int64_t) tr->start_primary_keys[0])
            return 1;
        return 0;
    }

    if(tr->query_type == QUERY_TYPE_READ_INDEX_RANGE)
    {
        if((int64_t) tw->column_values[tr->idx_idx] <= (int64_t) tr->start_primary_keys[0] && (int64_t) tw->column_values[tr->idx_idx] >= (int64_t) tr->end_primary_keys[0])
            return 1;
        return 0;
    }

    for(int i=0;i<tr->no_primary_keys;i++)
    {
        if(is_exact_read_query && tr->start_primary_keys[i] != tw->column_values[i])
            return 0;
        if(!is_exact_read_query &&
            ((int64_t) tr->start_primary_keys[i] > (int64_t) tw->column_values[i] ||
            (int64_t) tr->end_primary_keys[i] < (int64_t) tw->column_values[i]))
                return 0;
    }
    for(int i=0;i<tr->no_clustering_keys;i++)
    {
        if(is_exact_read_query && tr->start_clustering_keys[i] != tw->column_values[tw->no_primary_keys + i])
            return 0;
        if(!is_exact_read_query &&
            ((int64_t) tr->start_clustering_keys[i] > (int64_t) tw->column_values[tw->no_primary_keys + i] ||
            (int64_t) tr->end_clustering_keys[i] < (int64_t) tw->column_values[tw->no_primary_keys + i]))
                return 0;
    }

    return 1;
}

// Check if txn read op tr is invalidated by write op tw:
int rw_conflict(txn_read * tr, txn_write * tw, int check_exact_match)
{
    if(check_exact_match && tr->table_key != tw->table_key)
        return 0;

    // Check for RW conflicts only on regular data (all conflicts on queues are WW):

    if(tr->query_type >= QUERY_TYPE_READ_COLS && tr->query_type <= QUERY_TYPE_READ_INDEX_RANGE &&
        (tw->query_type == QUERY_TYPE_UPDATE || tw->query_type == QUERY_TYPE_DELETE) )
    {
        switch(tr->query_type)
        {
            case QUERY_TYPE_READ_COLS:
            case QUERY_TYPE_READ_CELL:
            case QUERY_TYPE_READ_CELL_RANGE:
            case QUERY_TYPE_READ_ROW:
            case QUERY_TYPE_READ_ROW_RANGE:
            case QUERY_TYPE_READ_INDEX:
            case QUERY_TYPE_READ_INDEX_RANGE:
            {
                if(check_exact_match && key_path_overlaps(tr, tw)) //  && compare_vc(tr->version, vector_clock * vc2)
                    return 1;
            }
        }
    }

    return 0;
}

// Check if queue op tw1 is invalidated by queue op tw2:
int queue_op_conflict(txn_write * tw1, txn_write * tw2)
{
    // Subscribes and unsubscribes are not currently supported in txns

    assert(tw1->query_type != QUERY_TYPE_SUBSCRIBE_QUEUE && tw1->query_type != QUERY_TYPE_UNSUBSCRIBE_QUEUE);
    assert(tw2->query_type != QUERY_TYPE_SUBSCRIBE_QUEUE && tw2->query_type != QUERY_TYPE_UNSUBSCRIBE_QUEUE);

    // Queue ops only conflict if they are on the same table and queue:

    if(tw1->table_key != tw2->table_key || tw1->queue_id != tw2->queue_id)
        return 0;

    // CREATE_QUEUE and DELETE_QUEUE conflict with everything:

    if(tw1->query_type == QUERY_TYPE_CREATE_QUEUE || tw1->query_type == QUERY_TYPE_DELETE_QUEUE ||
       tw2->query_type == QUERY_TYPE_CREATE_QUEUE || tw2->query_type == QUERY_TYPE_DELETE_QUEUE)
        return 1;

    // Only ENQUEUE/ENQUEUE (by any producers), as well as READ_QUEUE / READ_QUEUE and
    // CONSUME_QUEUE / CONSUME_QUEUE *by the same consumer* are valid conflicts:

    if(tw1->query_type != tw2->query_type)
        return 0;

    if(tw1->query_type == QUERY_TYPE_ENQUEUE)
        return 1;

    if((tw1->query_type == QUERY_TYPE_READ_QUEUE || tw1->query_type == QUERY_TYPE_CONSUME_QUEUE) &&
            tw1->consumer_id == tw2->consumer_id &&
            tw1->shard_id == tw2->shard_id &&
            tw1->app_id == tw2->app_id)
        return 1;

    return 0;
}

// Check if txn write op tw1 is invalidated by write op tw2:
int ww_conflict(txn_write * tw1, txn_write * tw2)
{
    short is_regular_op1 = (tw1->query_type == QUERY_TYPE_UPDATE || tw1->query_type == QUERY_TYPE_DELETE);
    short is_regular_op2 = (tw2->query_type == QUERY_TYPE_UPDATE || tw2->query_type == QUERY_TYPE_DELETE);

    if((is_regular_op1 && !is_regular_op2) || (!is_regular_op1&&is_regular_op2))
        return 0;

    if(is_regular_op1)
        return (txn_write_cmp((WORD) tw1, (WORD) tw2) == 0);
    else
        return queue_op_conflict(tw1, tw2);
}

int is_read_invalidated(txn_read * tr, txn_state * rts, db_t * db)
{
    // Check for conflicts with backend DB:

    switch(tr->query_type)
    {
        case QUERY_TYPE_READ_COLS:
        case QUERY_TYPE_READ_CELL:
        {
            return db_verify_cell_version(tr->start_primary_keys, tr->no_primary_keys, tr->start_clustering_keys, tr->no_clustering_keys, tr->table_key, tr->result_version, db);
        }
        case QUERY_TYPE_READ_ROW:
        {
            return db_verify_cell_version(tr->start_primary_keys, tr->no_primary_keys, NULL, 0, tr->table_key, tr->result_version, db);
        }
        case QUERY_TYPE_READ_INDEX:
        {
            return db_verify_index_version(tr->start_primary_keys, tr->idx_idx, tr->table_key, tr->result_version, db);
        }
        case QUERY_TYPE_READ_CELL_RANGE:
        {
            return db_verify_cell_range_version(tr->start_primary_keys, tr->no_primary_keys,
                                                tr->start_clustering_keys, tr->end_clustering_keys, tr->no_clustering_keys, tr->table_key,
                                                tr->range_result_keys, tr->range_result_versions, tr->no_range_results, db);
        }
        case QUERY_TYPE_READ_ROW_RANGE:
        {
            return db_verify_row_range_version(tr->start_primary_keys, tr->end_primary_keys, tr->no_primary_keys, tr->table_key,
                                                tr->range_result_keys, tr->range_result_versions, tr->no_range_results, db);
        }
        case QUERY_TYPE_READ_INDEX_RANGE:
        {
            return db_verify_index_range_version(tr->idx_idx, tr->start_primary_keys, tr->end_primary_keys,
                                                tr->range_result_keys, tr->range_result_versions, tr->no_range_results, tr->table_key, db);
        }
    }

    // Check for conflicts with the other txn write sets:

    // If exact (non-range) read, and not a secondary index query, create dummy write op on the same key path, to look it up in other txn's write set:

    int is_exact_query = (tr->query_type == QUERY_TYPE_READ_COLS || tr->query_type == QUERY_TYPE_READ_CELL || tr->query_type == QUERY_TYPE_READ_ROW);
    txn_write * dummy_tw_update = is_exact_query? get_dummy_txn_write(QUERY_TYPE_UPDATE, tr->start_primary_keys, tr->no_primary_keys, tr->start_clustering_keys, tr->no_clustering_keys, tr->table_key, 0) : NULL;

#if (MULTI_THREADED == 1)
    pthread_mutex_lock(db->txn_state_lock);
#endif

    for(snode_t * node=HEAD(db->txn_state); node!=NULL; node=NEXT(node))
    {
        assert(node->value != NULL);

        txn_state * ts = (txn_state *) node->value;

        if(ts->state != TXN_STATUS_VALIDATED || uuid_compare(rts->txnid, ts->txnid) == 0)
            continue;

        if(is_exact_query)
        {
            snode_t * write_op_n = skiplist_search(ts->write_set, (WORD) dummy_tw_update);

            if(write_op_n != NULL)
            {
                assert(write_op_n->value != NULL);

                txn_write * tw = (txn_write *) write_op_n->value;

                if(rw_conflict(tr, tw, 0))
                {
#if (VERBOSE_TXNS > 0)
                        log_debug("Invalidating txn due to rw conflict");
#endif

#if (MULTI_THREADED == 1)
                        pthread_mutex_unlock(db->txn_state_lock);
#endif

                        return 1;
                }
            }
        }
        else
        // For range or index queries, we need to iterate the other txn write sets to see if any writes conflict with our reads:
        {
            for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
            {
                assert(write_op_n->value != NULL);

                txn_write * tw = (txn_write *) write_op_n->value;

                if(rw_conflict(tr, tw, 1) && ts->state == TXN_STATUS_VALIDATED)
                {
#if (MULTI_THREADED == 1)
                        pthread_mutex_unlock(db->txn_state_lock);
#endif
                    return 1;
                }
            }
        }
    }

#if (MULTI_THREADED == 1)
    pthread_mutex_unlock(db->txn_state_lock);
#endif

    return 0;
}

int is_write_invalidated(txn_write * tw, txn_state * rts, int * schema_status, db_t * db)
{
    // Check for invalidated queue reads / creation / deletion ops with backend DB:

    *schema_status = 0;
    switch(tw->query_type)
    {
        case QUERY_TYPE_READ_QUEUE:
        {
            int status = db_verify_cell_version(&tw->queue_id, 1, NULL, 0, tw->table_key, tw->prh_version, db);
            if(status == DB_ERR_NO_TABLE || status == DB_ERR_NO_QUEUE)
            {
#if (VERBOSE_TXNS > 0)
                log_debug("read_queue(%ld) invalidating txn", tw->queue_id);
#endif
                *schema_status = 1;
            }
            return (status != 0);
        }
        case QUERY_TYPE_CREATE_QUEUE:
        {
            return (db_search(&tw->queue_id, tw->table_key, db) != NULL);
        }
        case QUERY_TYPE_DELETE_QUEUE:
        case QUERY_TYPE_UNSUBSCRIBE_QUEUE:
        {
            return (db_search(&tw->queue_id, tw->table_key, db) == NULL);
        }
        case QUERY_TYPE_ENQUEUE:
        case QUERY_TYPE_CONSUME_QUEUE:
        case QUERY_TYPE_SUBSCRIBE_QUEUE:
        {
            if(db_search(&tw->queue_id, tw->table_key, db) == NULL)
            {
#if (VERBOSE_TXNS > 0)
                log_debug("Query type %d (%ld) invalidating txn", tw->query_type, tw->queue_id);
#endif
                *schema_status = 1;
                return 1;
            }
            return 0;
        }
    }

    // Check for WW conflicts with other txns' write sets:

#if (MULTI_THREADED == 1)
    pthread_mutex_lock(db->txn_state_lock);
#endif

    for(snode_t * node=HEAD(db->txn_state); node!=NULL; node=NEXT(node))
    {
        assert(node->value != NULL);

        txn_state * ts = (txn_state *) node->value;

        if(ts->state != TXN_STATUS_VALIDATED || uuid_compare(rts->txnid, ts->txnid) == 0)
            continue;

        if(tw->query_type == QUERY_TYPE_UPDATE || tw->query_type == QUERY_TYPE_DELETE)
        // Regular ops
        {
            snode_t * write_op_n = skiplist_search(ts->write_set, (WORD) tw);

            if(write_op_n != NULL)
            {
                assert(write_op_n->value != NULL);

                txn_write * tw2 = (txn_write *) write_op_n->value;

//              if(ww_conflict(tw, tw2, 0))
//              {
#if (VERBOSE_TXNS > 0)
                    char uuid_str1[37], uuid_str2[37];
                    uuid_unparse_lower(rts->txnid, uuid_str1);
                    uuid_unparse_lower(ts->txnid, uuid_str2);

                    log_debug("Invalidating txn due to ww conflict on table=%" PRId64 "/%" PRId64 ", write_type=%d/%d, key=%" PRId64 "/%" PRId64 ", txn=%s/%s",
                                (int64_t) tw->table_key, (int64_t) tw2->table_key,
                                tw->query_type, tw2->query_type,
                                ((int64_t *) tw->column_values)[0], ((int64_t *) tw2->column_values)[0],
                                uuid_str2, uuid_str1);
#endif

#if (MULTI_THREADED == 1)
                    pthread_mutex_unlock(db->txn_state_lock);
#endif

                    return 1;
//              }
            }
        }
        else
        // Queue ops:
        {
            for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
            {
                assert(write_op_n->value != NULL);

                txn_write * tw2 = (txn_write *) write_op_n->value;

                // No conflict between regular ops and queue ops:

                if(tw->query_type == QUERY_TYPE_UPDATE || tw->query_type == QUERY_TYPE_DELETE)
                    continue;

                if(queue_op_conflict(tw, tw2))

                {
#if (MULTI_THREADED == 1)
                    pthread_mutex_unlock(db->txn_state_lock);
#endif
                    return 1;
                }
            }
        }
    }

#if (MULTI_THREADED == 1)
    pthread_mutex_unlock(db->txn_state_lock);
#endif

    return 0;
}


int validate_txn(uuid_t * txnid, vector_clock * version, db_t * db)
{
    txn_state * ts = get_txn_state(txnid, db);
    int schema_status = 0;
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    assert(ts->state == TXN_STATUS_ACTIVE);

    // Txn now gets a new version stamp:

    set_version(ts, version);

    for(snode_t * read_op_n=HEAD(ts->read_set); read_op_n!=NULL; read_op_n=NEXT(read_op_n))
    {
        if(read_op_n->value != NULL)
        {
            txn_read * tr = (txn_read *) read_op_n->value;

            if(is_read_invalidated(tr, ts, db))
            {
                return VAL_STATUS_ABORT;
            }
        }
    }

    for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
    {
        if(write_op_n->value != NULL)
        {
            txn_write * tw = (txn_write *) write_op_n->value;

            if(is_write_invalidated(tw, ts, &schema_status, db))
            {
                return (schema_status == 0)?VAL_STATUS_ABORT:VAL_STATUS_ABORT_SCHEMA;
            }
        }
    }

    ts->state = TXN_STATUS_VALIDATED;

    return VAL_STATUS_COMMIT;
}

int persist_write(txn_write * tw, vector_clock * version, db_t * db, unsigned int * fastrandstate)
{
    switch(tw->query_type)
    {
        case QUERY_TYPE_UPDATE:
        {
            // Note: This also updates or creates the version of the updated / created cell:

            return db_insert_transactional(tw->column_values, tw->no_cols, tw->no_clustering_keys, tw->blob_size, version, tw->table_key, db, fastrandstate);
        }
        case QUERY_TYPE_DELETE:
        {
            // Update row tombstone version for handling shadow range reads and reads by incomplete partition / clustering key path?

            return db_delete_row_transactional(tw->column_values, version, tw->table_key, db, fastrandstate); // TO DO: use tw->no_primary_keys and tw->no_clustering_keys
        }
        case QUERY_TYPE_ENQUEUE:
        {
            return enqueue(tw->column_values, tw->no_cols, tw->blob_size, tw->table_key, tw->queue_id, 1, db, fastrandstate);
        }
        case QUERY_TYPE_READ_QUEUE:
        {
            // Note: This also updates queue private read head version:

            return set_private_read_head(tw->consumer_id, tw->shard_id, tw->app_id, tw->table_key, tw->queue_id, tw->new_read_head, version, 1, db);
        }
        case QUERY_TYPE_CONSUME_QUEUE:
        {
            return set_private_consume_head(tw->consumer_id, tw->shard_id, tw->app_id, tw->table_key, tw->queue_id, tw->new_consume_head, version, db);
        }
        case QUERY_TYPE_CREATE_QUEUE:
        {
            return create_queue(tw->table_key, tw->queue_id, version, 1, db, fastrandstate);
        }
        case QUERY_TYPE_DELETE_QUEUE:
        {
            // Note: This also updates queue tombstone version (if queue was not already deleted by a different txn, in which case earliest deletion timestamp is kept):

            return delete_queue(tw->table_key, tw->queue_id, version, 1, db, fastrandstate);
        }
        default:
        {
            assert(0);
        }
    }

    return 0;
}

int persist_txn(txn_state * ts, db_t * db, unsigned int * fastrandstate)
{
    int res = 0;

    // Txn needs to have received a commit version by this point:
    assert(ts->version != NULL);

#if (VERBOSE_TXNS_PERSIST > 0)
        char uuid_str[37];
        uuid_unparse_lower(ts->txnid, uuid_str);
        log_debug("BACKEND: Txn %s has %d writes", uuid_str, ts->write_set->no_items);
#endif

    for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
    {
        if(write_op_n->value != NULL)
        {
            txn_write * tw = (txn_write *) write_op_n->value;

#if (VERBOSE_TXNS_PERSIST > 0)
            log_debug("BACKEND: Txn %s attempting to persist write of type %d", uuid_str, tw->query_type);
#endif

            res = persist_write(tw, ts->version, db, fastrandstate);

#if (VERBOSE_TXNS_PERSIST > 0)
            log_debug("BACKEND: Txn %s successfully persisted write of type %d", uuid_str, tw->query_type);
#endif

            if(res != 0)
                log_debug("BACKEND: persist_write for txn, of type %d returned %d", tw->query_type, res);
        }
    }

    close_txn_state(ts, db);

    return res;
}

int abort_txn(uuid_t * txnid, db_t * db)
{
    return close_txn(txnid, db);
}

int commit_txn(uuid_t * txnid, vector_clock * version, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

#if (VERBOSE_TXNS > 0)
    char uuid_str[37];
    uuid_unparse_lower(*txnid, uuid_str);
#endif
#if (VERBOSE_TXNS > 1)
    log_debug("BACKEND: Attempting to validate txn %s", uuid_str);
#endif

    int res = validate_txn(txnid, version, db);

#if (VERBOSE_TXNS > 1)
    log_debug("BACKEND: validate txn %s returned %d", uuid_str, res);
#endif

    if(res == VAL_STATUS_COMMIT)
    {
        res = persist_txn(ts, db, fastrandstate);

#if (VERBOSE_TXNS > 0)
        log_debug("BACKEND: persist txn %s returned %d", uuid_str, res);
#endif
    }
    else if(res == VAL_STATUS_ABORT || res == VAL_STATUS_ABORT_SCHEMA)
    {
        res = abort_txn(txnid, db);

#if (VERBOSE_TXNS > 0)
        log_debug("BACKEND: abort txn %s returned %d", uuid_str, res);
#endif
    }
    else
    {
        assert(0);
    }

    return res;
}


int db_insert_in_txn(WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, size_t blob_size, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    return add_write_to_txn(QUERY_TYPE_UPDATE, column_values, no_cols, no_primary_keys, no_clustering_keys, blob_size, table_key, ts, fastrandstate);
}

db_row_t* db_search_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NULL; // No such txn

    db_row_t* result = db_search(primary_keys, table_key, db);

    // Note that if result == NULL (no such row), we still add that query to the txn read set (to allow txn to be invalidated by "shadow writes")

    int ret = add_row_read_to_txn(primary_keys, no_primary_keys, table_key, result, ts, fastrandstate);

    assert(ret == 0);

    return result;
}

int db_range_search_in_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
                            snode_t** start_row, snode_t** end_row,
                            WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    int no_rows = db_range_search(start_primary_keys, end_primary_keys, start_row, end_row, table_key, db);

    // Note that if no_rows == 0 (no rows read), we still add that query to the txn read set (to allow txn to be invalidated by "shadow writes")

    return add_row_range_read_to_txn(start_primary_keys, end_primary_keys, no_primary_keys, table_key, *start_row, *end_row, no_rows, ts, fastrandstate);
}


db_row_t* db_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NULL; // No such txn

    db_row_t* result = db_search_clustering(primary_keys, clustering_keys, no_clustering_keys, table_key, db);

    // Note that if result == NULL (no such row), we still add that query to the txn read set (to allow txn to be invalidated by "shadow writes")

    int ret = add_cell_read_to_txn(primary_keys, no_primary_keys, clustering_keys, no_clustering_keys, table_key, result, ts, fastrandstate);

    assert(ret == 0);

    return result;
}

int db_range_search_clustering_in_txn(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys, snode_t** start_row, snode_t** end_row, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    // Note that if ret == 0 (no rows read), we still add that query to the txn read set (to allow txn to be invalidated by "shadow writes")

    int no_results = db_range_search_clustering(primary_keys, start_clustering_keys, end_clustering_keys, no_clustering_keys, start_row, end_row, table_key, db);

    return add_cell_range_read_to_txn(primary_keys, no_primary_keys, start_clustering_keys,
                                        end_clustering_keys, no_clustering_keys, table_key,
                                        *start_row, *end_row, no_results, ts, fastrandstate);
}

// WORD* db_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int* column_idxs, int no_columns, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
db_row_t* db_search_columns_in_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys, WORD* col_keys, int no_columns, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    assert (0); // This won't work until db_search_columns is refactored as per below:
    return 0;

/*
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

//  db_search_columns(primary_keys, clustering_keys, int* column_idxs, no_columns, table_key, db_t * db);

    db_row_t* result = db_search_columns_result(primary_keys, clustering_keys, col_keys, no_columns, table_key, db);

    return add_col_read_to_txn(primary_keys, no_primary_keys, clustering_keys, no_clustering_keys,
                                col_keys, no_columns, table_key, result, ts, fastrandstate);
*/
}

db_row_t* db_search_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NULL; // No such txn

    db_row_t* result = db_search_index(index_key, idx_idx, table_key, db);

    int ret = add_index_read_to_txn(&index_key, idx_idx, table_key, result, ts, fastrandstate);

    assert(ret == 0);

    return result;
}

int db_range_search_index_in_txn(int idx_idx, WORD start_idx_key, WORD end_idx_key, snode_t** start_row, snode_t** end_row, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    int no_results = db_range_search_index(idx_idx, start_idx_key, end_idx_key, start_row, end_row, table_key, db);

    return add_index_range_read_to_txn(idx_idx, &start_idx_key, &end_idx_key, *start_row, *end_row, no_results, table_key, ts, fastrandstate);
}

int db_delete_row_in_txn(WORD* primary_keys, int no_primary_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    return add_write_to_txn(QUERY_TYPE_DELETE, primary_keys, no_primary_keys, no_primary_keys, 0, 0, table_key, ts, fastrandstate);
}

int db_delete_cell_in_txn(WORD* keys, int no_primary_keys, int no_clustering_keys, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    return add_write_to_txn(QUERY_TYPE_DELETE, keys, no_primary_keys+no_clustering_keys, no_primary_keys, no_clustering_keys, 0, table_key, ts, fastrandstate);
}

int db_delete_by_index_in_txn(WORD index_key, int idx_idx, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    assert (0); // Not supported yet
    return 0;
}

int db_update_in_txn(int * col_idxs, int no_cols, size_t blob_size, WORD * column_values, WORD table_key, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    assert (0); // Not supported in new schema-less model
    return 0;
}

// Queue ops:

int enqueue_in_txn(WORD * column_values, int no_cols, size_t blob_size, WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    return add_enqueue_to_txn(column_values, no_cols, blob_size, table_key, queue_id, ts, fastrandstate);
}

int read_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
        int max_entries, int * entries_read, int64_t * new_read_head,
        snode_t** start_row, snode_t** end_row, uuid_t * txnid,
        db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    int64_t prev_read_head = -1;

    // Lookup previously existing read head in this txn's read set:

    for(snode_t * write_op_n=HEAD(ts->write_set); write_op_n!=NULL; write_op_n=NEXT(write_op_n))
    {
        if(write_op_n->value != NULL)
        {
            txn_write * tw = (txn_write *) write_op_n->value;

            if(tw->query_type == QUERY_TYPE_READ_QUEUE && tw->table_key == table_key &&
                tw->queue_id == queue_id && tw->consumer_id == consumer_id &&
                tw->shard_id == shard_id && tw->app_id == app_id)
            {
                prev_read_head = tw->new_read_head;
                break;
            }
        }
    }

    vector_clock * prh_version = NULL;

    int status = peek_queue(consumer_id, shard_id, app_id, table_key, queue_id,
            max_entries, prev_read_head, entries_read, new_read_head, &prh_version,
            start_row, end_row, db);

    int ret = add_read_queue_to_txn(consumer_id, shard_id, app_id, table_key, queue_id,
                                    *new_read_head, prh_version, ts, fastrandstate);

    return (ret==0)?(status):(ret);
}

int consume_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                    int64_t new_consume_head, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    return add_consume_queue_to_txn(consumer_id, shard_id, app_id, table_key, queue_id,
            new_consume_head, ts, fastrandstate);
}

int subscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
                        uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    assert (0); // Not implemented
    return 0;
}

int unsubscribe_queue_in_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                                uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    assert (0); // Not implemented
    return 0;
}

int create_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    return add_create_queue_to_txn(table_key, queue_id, ts, fastrandstate);
}

int delete_queue_in_txn(WORD table_key, WORD queue_id, uuid_t * txnid, db_t * db, unsigned int * fastrandstate)
{
    txn_state * ts = get_txn_state(txnid, db);
    if(ts == NULL)
        return NO_SUCH_TXN; // No such txn

    return add_delete_queue_to_txn(table_key, queue_id, ts, fastrandstate);
}
