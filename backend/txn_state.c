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
 * txn_state.c
 */

#include <stdlib.h>
#include <uuid/uuid.h>
#include <string.h>
#include <assert.h>

#include "backend/txn_state.h"

int txn_write_cmp(WORD e1, WORD e2)
{
    txn_write * tr1 = (txn_write *) e1;
    txn_write * tr2 = (txn_write *) e2;

    if(tr1->table_key != tr2->table_key)
        return (int) ((int64_t) tr1->table_key - (int64_t) tr2->table_key);

    if(tr1->query_type < QUERY_TYPE_ENQUEUE) // Regular ops ordering:
    {
        // Newer writes always overwrite older writes to the same objects in the same txn (even between updates/deletes to same object):

        int no_key_cols1 = tr1->no_primary_keys + tr1->no_clustering_keys;
        int no_key_cols2 = tr2->no_primary_keys + tr2->no_clustering_keys;

        if(no_key_cols1 != no_key_cols2)
            return no_key_cols1 - no_key_cols2;
        for(int i=0;i<no_key_cols1;i++)
        {
            if(tr1->column_values[i] != tr2->column_values[i])
                return (int) (int64_t) tr1->column_values[i] - (int64_t) tr2->column_values[i];
        }
    }
    else // Queue ops ordering:
    {
        if(tr1->query_type != tr2->query_type)
            return tr1->query_type - tr2->query_type;

        // For queue reads and consumes, the last version of private read/consume head is kept in write set:
        if(tr1->query_type == QUERY_TYPE_READ_QUEUE || tr1->query_type == QUERY_TYPE_CONSUME_QUEUE)
        {
            if(tr1->queue_id != tr2->queue_id)
                return (int) ((int64_t) tr1->queue_id - (int64_t) tr2->queue_id);
            if(tr1->app_id != tr2->app_id)
                return (int) ((int64_t) tr1->app_id - (int64_t) tr2->app_id);
            if(tr1->shard_id != tr2->shard_id)
                return (int) ((int64_t) tr1->shard_id - (int64_t) tr2->shard_id);
            if(tr1->consumer_id != tr2->consumer_id)
                return (int) ((int64_t) tr1->consumer_id - (int64_t) tr2->consumer_id);
        }
        // All enqueues, queue creates, deletes, subscribes and unsubscribes are accumulated in the write set, in the local order they were issued in the txn:
        else if(tr1->query_type == QUERY_TYPE_ENQUEUE ||
                tr1->query_type == QUERY_TYPE_CREATE_QUEUE ||
                tr1->query_type == QUERY_TYPE_SUBSCRIBE_QUEUE ||
                tr1->query_type == QUERY_TYPE_UNSUBSCRIBE_QUEUE)
        {
            if(tr1->local_order != tr2->local_order)
                return (int) ((int64_t) tr1->local_order - (int64_t) tr2->local_order);
        }
    }

    return 0;
}

int txn_read_cmp(WORD e1, WORD e2)
{
    txn_read * tr1 = (txn_read *) e1;
    txn_read * tr2 = (txn_read *) e2;

    // Same read operations (individual or range, queried by various clustering levels) return the same object versions while the txn is open:

    if(tr1->query_type != tr2->query_type)
        return tr1->query_type - tr2->query_type;

    if(tr1->table_key != tr2->table_key)
        return (int) ((int64_t) tr1->table_key - (int64_t) tr2->table_key);

    if(tr1->query_type == QUERY_TYPE_READ_INDEX || tr1->query_type == QUERY_TYPE_READ_INDEX_RANGE)
    {
        if(tr1->idx_idx != tr2->idx_idx)
            return tr1->idx_idx - tr2->idx_idx;
    }

    if(tr1->no_primary_keys != tr2->no_primary_keys)
        return tr1->no_primary_keys - tr2->no_primary_keys;
    for(int i=0;i<tr1->no_primary_keys;i++)
    {
        if(tr1->start_primary_keys[i] != tr2->start_primary_keys[i])
            return tr1->start_primary_keys[i] - tr2->start_primary_keys[i];
    }

    if(tr1->query_type == QUERY_TYPE_READ_ROW_RANGE || tr1->query_type == QUERY_TYPE_READ_INDEX_RANGE)
    {
        for(int i=0;i<tr1->no_primary_keys;i++)
        {
            if(tr1->end_primary_keys[i] != tr2->end_primary_keys[i])
                return tr1->end_primary_keys[i] - tr2->end_primary_keys[i];
        }
    }

    if(tr1->query_type != QUERY_TYPE_READ_ROW && tr1->query_type != QUERY_TYPE_READ_ROW_RANGE)
    {
        if(tr1->no_clustering_keys != tr2->no_clustering_keys)
            return tr1->no_clustering_keys - tr2->no_clustering_keys;
        for(int i=0;i<tr1->no_clustering_keys;i++)
        {
            if(tr1->start_clustering_keys[i] != tr2->start_clustering_keys[i])
                return tr1->start_clustering_keys[i] - tr2->start_clustering_keys[i];
        }

        if(tr1->query_type == QUERY_TYPE_READ_CELL_RANGE)
        {
            for(int i=0;i<tr1->no_clustering_keys;i++)
            {
                if(tr1->end_clustering_keys[i] != tr2->end_clustering_keys[i])
                    return tr1->end_clustering_keys[i] - tr2->end_clustering_keys[i];
            }
        }
    }

    if(tr1->query_type == QUERY_TYPE_READ_COLS)
    {
        if(tr1->no_col_keys != tr2->no_col_keys)
            return tr1->no_col_keys - tr2->no_col_keys;
        for(int i=0;i<tr1->no_col_keys;i++)
        {
            if(tr1->col_keys[i] != tr2->col_keys[i])
                return tr1->col_keys[i] - tr2->col_keys[i];
        }
    }

    return 0;
}

txn_state * init_txn_state()
{
    txn_state * ts = (txn_state *) malloc(sizeof(txn_state));
    uuid_generate(ts->txnid);
    ts->read_set = create_skiplist(&txn_read_cmp);
    ts->write_set = create_skiplist(&txn_write_cmp);
    ts->state = TXN_STATUS_ACTIVE;
    ts->version = NULL;

    return ts;
}

void set_version(txn_state * ts, vector_clock * vc)
{
    if(ts->version == NULL)
        ts->version = copy_vc(vc);
    else
        update_or_replace_vc(&(ts->version), vc);
}

void free_txn_state(txn_state * ts)
{
    skiplist_free(ts->read_set);
    skiplist_free(ts->write_set);
    free(ts);
}

txn_write * get_txn_write(short query_type, WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, size_t blob_size, WORD table_key, int64_t local_order)
{
    txn_write * tw = (txn_write *) malloc(sizeof(txn_write) + no_cols*sizeof(WORD));
    memset(tw, 0, sizeof(txn_write) + no_cols*sizeof(WORD));

    tw->table_key = table_key;
    tw->no_cols = no_cols;
    tw->no_primary_keys = no_primary_keys;
    tw->no_clustering_keys = no_clustering_keys;
    tw->blob_size = blob_size;
    tw->column_values = (WORD *) ((char *) tw + sizeof(txn_write));
    for(int i=0;i<tw->no_cols;i++)
        tw->column_values[i] = column_values[i];

    tw->query_type = query_type;
    tw->local_order = local_order;

    return tw;
}

txn_write * get_dummy_txn_write(short query_type, WORD * primary_keys, int no_primary_keys, WORD * clustering_keys, int no_clustering_keys, WORD table_key, int64_t local_order)
{
    int no_cols = no_primary_keys + no_clustering_keys;
    txn_write * tw = (txn_write *) malloc(sizeof(txn_write) + no_cols*sizeof(WORD));
    memset(tw, 0, sizeof(txn_write) + no_cols*sizeof(WORD));

    tw->table_key = table_key;
    tw->no_cols = no_cols;
    tw->blob_size = 0;
    tw->no_primary_keys = no_primary_keys;
    tw->no_clustering_keys = no_clustering_keys;
    tw->column_values = (WORD *) ((char *) tw + sizeof(txn_write));
    int i=0;
    for(;i<tw->no_primary_keys;i++)
        tw->column_values[i] = primary_keys[i];
    for(;i<tw->no_cols;i++)
        tw->column_values[i] = primary_keys[i-no_primary_keys];

    tw->query_type = query_type;
    tw->local_order = local_order;

    return tw;
}

txn_write * get_txn_queue_op(short query_type, WORD * column_values, int no_cols, size_t blob_size, WORD table_key,
                    WORD queue_id, WORD consumer_id, WORD shard_id, WORD app_id,
                    int64_t new_read_head, vector_clock * prh_version, int64_t new_consume_head, int64_t local_order)
{
    assert(query_type >= QUERY_TYPE_ENQUEUE && query_type <= QUERY_TYPE_UNSUBSCRIBE_QUEUE);

    txn_write * tw = get_txn_write(query_type, column_values, no_cols, 0, 0, blob_size, table_key, local_order);

    tw->queue_id = queue_id;

    if(query_type == QUERY_TYPE_READ_QUEUE || query_type == QUERY_TYPE_CONSUME_QUEUE ||
        query_type == QUERY_TYPE_SUBSCRIBE_QUEUE || query_type == QUERY_TYPE_UNSUBSCRIBE_QUEUE)
    {
        tw->consumer_id = consumer_id;
        tw->shard_id = shard_id;
        tw->app_id = app_id;
    }
    else
    {
        assert(consumer_id == NULL && shard_id == NULL && app_id == NULL);
    }

    if(query_type == QUERY_TYPE_READ_QUEUE)
    {
        tw->new_read_head = new_read_head;
        tw->prh_version = prh_version;
    }
    else
    {
        assert(new_read_head == -1);
    }

    if(query_type == QUERY_TYPE_CONSUME_QUEUE)
    {
        tw->new_consume_head = new_consume_head;
    }
    else
    {
        assert(new_consume_head == -1);
    }

    return tw;
}

void free_txn_write(txn_write * tw)
{
    free(tw);
}

txn_read * get_txn_read(short query_type,
                        WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
                        WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
                        WORD* col_keys, int no_col_keys,
                        int idx_idx,
                        vector_clock * result_version,
                        int64_t * range_result_keys, vector_clock ** range_result_versions, int no_range_results,
                        WORD table_key, int64_t local_order)
{
    int total_col_count = no_primary_keys + no_clustering_keys;
    if(query_type == QUERY_TYPE_READ_ROW_RANGE)
        total_col_count += no_primary_keys;
    if(query_type == QUERY_TYPE_READ_CELL_RANGE)
        total_col_count += no_clustering_keys;
    if(query_type == QUERY_TYPE_READ_COLS)
        total_col_count += no_col_keys;

    txn_read * tr = (txn_read *) malloc(sizeof(txn_read) + total_col_count * sizeof(WORD));
    memset(tr, 0, sizeof(txn_read) + total_col_count * sizeof(WORD));

    tr->table_key = table_key;
    tr->query_type = query_type;
    tr->local_order = local_order;

    int offset = sizeof(txn_write);
    tr->no_primary_keys = no_primary_keys;
    tr->start_clustering_keys = (WORD *) ((char *) tr + offset);
    for(int i=0;i<tr->no_primary_keys;i++)
        tr->start_primary_keys[i] = start_primary_keys[i];
    offset += tr->no_primary_keys * sizeof(WORD);

    if(query_type == QUERY_TYPE_READ_ROW_RANGE)
    {
        tr->end_clustering_keys = (WORD *) ((char *) tr + offset);
        for(int i=0;i<tr->no_primary_keys;i++)
            tr->end_clustering_keys[i] = end_clustering_keys[i];
        offset += tr->no_primary_keys * sizeof(WORD);
    }

    if(query_type != QUERY_TYPE_READ_ROW && query_type != QUERY_TYPE_READ_ROW_RANGE)
    {
        tr->start_clustering_keys = (WORD *) ((char *) tr + offset);
        for(int i=0;i<tr->no_clustering_keys;i++)
            tr->start_clustering_keys[i] = start_clustering_keys[i];
        offset += tr->no_clustering_keys * sizeof(WORD);
    }

    if(query_type == QUERY_TYPE_READ_CELL_RANGE)
    {
        tr->end_clustering_keys = (WORD *) ((char *) tr + offset);
        for(int i=0;i<tr->no_clustering_keys;i++)
            tr->end_clustering_keys[i] = end_clustering_keys[i];
        offset += tr->no_clustering_keys * sizeof(WORD);
    }

    if(query_type == QUERY_TYPE_READ_COLS)
    {
        tr->col_keys = (WORD *) ((char *) tr + offset);
        for(int i=0;i<tr->no_col_keys;i++)
            tr->col_keys[i] = col_keys[i];
        offset += tr->no_col_keys * sizeof(WORD);
    }

    if(query_type == QUERY_TYPE_READ_INDEX || query_type == QUERY_TYPE_READ_INDEX_RANGE)
        assert(idx_idx != -1);

    tr->idx_idx = idx_idx;

    tr->result_version = result_version;
    tr->range_result_keys = range_result_keys;
    tr->range_result_versions = range_result_versions;
    tr->no_range_results = no_range_results;

    return tr;
}

void free_txn_read(txn_read * tr)
{
    if(tr->result_version != NULL)
        free_vc(tr->result_version);

    if(tr->range_result_keys && tr->no_range_results > 0)
    {
        free(tr->range_result_keys);
        for(int i=0;i<tr->no_range_results;i++)
            free_vc(tr->range_result_versions[i]);
        free(tr->range_result_versions);
    }

    free(tr);
}

int add_write_to_txn(short query_type, WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, size_t blob_size, WORD table_key, txn_state * ts, unsigned int * fastrandstate)
{
    assert((query_type == QUERY_TYPE_UPDATE) || (query_type == QUERY_TYPE_DELETE));
    txn_write * tw = get_txn_write(query_type, column_values, no_cols, no_primary_keys, no_clustering_keys, blob_size, table_key, (int64_t) ts->write_set->no_items);

    // Note that this will overwrite previous values written for the variable in the same txn (last write wins):

    snode_t * prev_tw_node = skiplist_search(ts->write_set, (WORD) tw);
    txn_write * prev_tw = (prev_tw_node != NULL)?prev_tw_node->value : NULL;

    int ret = skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate);

    if(prev_tw != NULL)
        free_txn_write(prev_tw);

    return ret;
}

int add_row_read_to_txn(WORD* primary_keys, int no_primary_keys,
                        WORD table_key, db_row_t* result,
                        txn_state * ts, unsigned int * fastrandstate)
{
    txn_read * tr = get_txn_read(QUERY_TYPE_READ_ROW, primary_keys, NULL, no_primary_keys, NULL, NULL, 0, NULL, 0, -1, copy_vc(result->version), NULL, NULL, 0, table_key, (int64_t) ts->read_set->no_items);

    // Note that this will overwrite previous values read for the variable in the same txn (last read wins):

    snode_t * prev_tr_node = skiplist_search(ts->read_set, (WORD) tr);
    txn_read * prev_tr = (prev_tr_node != NULL)?prev_tr_node->value : NULL;

    int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate);

    if(prev_tr != NULL)
        free_txn_read(prev_tr);

    return ret;
}

int add_row_range_read_to_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys, WORD table_key,
                                snode_t* start_row, snode_t* end_row, int no_results,
                                txn_state * ts, unsigned int * fastrandstate)
{
    int64_t * range_result_keys = (int64_t *) malloc(no_results * sizeof(int64_t));
    vector_clock ** range_result_versions = (vector_clock **) malloc(no_results * sizeof(vector_clock *));;
    int i=0;

    for(snode_t* crt_row = start_row;crt_row != end_row;crt_row=NEXT(crt_row), i++)
    {
        db_row_t * row = (db_row_t *) crt_row->value;
        range_result_keys[i] = (int64_t) row->key;
        range_result_versions[i] = (row->version != NULL)? copy_vc(row->version) : NULL;
    }

    txn_read * tr = get_txn_read(QUERY_TYPE_READ_ROW_RANGE, start_primary_keys, end_primary_keys, no_primary_keys, NULL, NULL, 0, NULL, 0, -1, NULL, range_result_keys, range_result_versions, no_results, table_key, (int64_t) ts->read_set->no_items);
    snode_t * prev_tr_node = skiplist_search(ts->read_set, (WORD) tr);
    txn_read * prev_tr = (prev_tr_node != NULL)?prev_tr_node->value : NULL;

    int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
    if(prev_tr != NULL)
        free_txn_read(prev_tr);

    return ret;
}

int add_cell_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
                                WORD table_key, db_row_t* result,
                                txn_state * ts, unsigned int * fastrandstate)
{
    txn_read * tr = get_txn_read(QUERY_TYPE_READ_CELL, primary_keys, NULL, no_primary_keys, clustering_keys, NULL, no_clustering_keys, NULL, 0, -1, copy_vc(result->version), NULL, NULL, 0, table_key, (int64_t) ts->read_set->no_items);
    snode_t * prev_tr_node = skiplist_search(ts->read_set, (WORD) tr);
    txn_read * prev_tr = (prev_tr_node != NULL)?prev_tr_node->value : NULL;

    int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
    if(prev_tr != NULL)
        free_txn_read(prev_tr);

    return ret;
}

int add_cell_range_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys,
                                WORD* end_clustering_keys, int no_clustering_keys,
                                WORD table_key,
                                snode_t* start_row, snode_t* end_row, int no_results,
                                txn_state * ts, unsigned int * fastrandstate)
{
    int64_t * range_result_keys = (int64_t *) malloc(no_results * sizeof(int64_t));
    vector_clock ** range_result_versions = (vector_clock **) malloc(no_results * sizeof(vector_clock *));;
    int i=0;

    for(snode_t* crt_row = start_row;crt_row != end_row;crt_row=NEXT(crt_row), i++)
    {
        db_row_t * row = (db_row_t *) crt_row->value;
        range_result_keys[i] = (int64_t) row->key;
        range_result_versions[i] = (row->version != NULL)? copy_vc(row->version) : NULL;
    }

    txn_read * tr = get_txn_read(QUERY_TYPE_READ_CELL_RANGE, primary_keys, NULL, no_primary_keys,
                                start_clustering_keys, end_clustering_keys, no_clustering_keys,
                                NULL, 0, -1, NULL,
                                range_result_keys, range_result_versions, no_results,
                                table_key, (int64_t) ts->read_set->no_items);

    snode_t * prev_tr_node = skiplist_search(ts->read_set, (WORD) tr);
    txn_read * prev_tr = (prev_tr_node != NULL)?prev_tr_node->value : NULL;

    int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
    if(prev_tr != NULL)
        free_txn_read(prev_tr);

    return ret;
}

int add_col_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
                                WORD* col_keys, int no_columns,
                                WORD table_key, db_row_t* result,
                                txn_state * ts, unsigned int * fastrandstate)
{
    txn_read * tr = get_txn_read(QUERY_TYPE_READ_COLS, primary_keys, NULL, no_primary_keys, clustering_keys, NULL, no_clustering_keys, col_keys, no_columns, -1, copy_vc(result->version), NULL, NULL, 0, table_key, (int64_t) ts->read_set->no_items);
    snode_t * prev_tr_node = skiplist_search(ts->read_set, (WORD) tr);
    txn_read * prev_tr = (prev_tr_node != NULL)?prev_tr_node->value : NULL;

    int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
    if(prev_tr != NULL)
        free_txn_read(prev_tr);

    return ret;
}

int add_index_read_to_txn(WORD* index_key, int idx_idx, WORD table_key, db_row_t* result, txn_state * ts, unsigned int * fastrandstate)
{
    txn_read * tr = get_txn_read(QUERY_TYPE_READ_INDEX, index_key, NULL, 1, NULL, NULL, 0, NULL, 0, idx_idx, copy_vc(result->version), NULL, NULL, 0, table_key, (int64_t) ts->read_set->no_items);
    snode_t * prev_tr_node = skiplist_search(ts->read_set, (WORD) tr);
    txn_read * prev_tr = (prev_tr_node != NULL)?prev_tr_node->value : NULL;

    int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
    if(prev_tr != NULL)
        free_txn_read(prev_tr);

    return ret;
}

int add_index_range_read_to_txn(int idx_idx, WORD* start_idx_key, WORD* end_idx_key, snode_t* start_row, snode_t* end_row, int no_results, WORD table_key, txn_state * ts, unsigned int * fastrandstate)
{
    int64_t * range_result_keys = (int64_t *) malloc(no_results * sizeof(int64_t));
    vector_clock ** range_result_versions = (vector_clock **) malloc(no_results * sizeof(vector_clock *));;
    int i=0;

    for(snode_t* crt_row = start_row;crt_row != end_row;crt_row=NEXT(crt_row), i++)
    {
        db_row_t * row = (db_row_t *) crt_row->value;
        range_result_keys[i] = (int64_t) row->key;
        range_result_versions[i] = (row->version != NULL)? copy_vc(row->version) : NULL;
    }

    txn_read * tr = get_txn_read(QUERY_TYPE_READ_INDEX_RANGE, start_idx_key, end_idx_key, 1, NULL, NULL, 0, NULL, 0, idx_idx, NULL, range_result_keys, range_result_versions, no_results, table_key, (int64_t) ts->read_set->no_items);
    snode_t * prev_tr_node = skiplist_search(ts->read_set, (WORD) tr);
    txn_read * prev_tr = (prev_tr_node != NULL)?prev_tr_node->value : NULL;

    int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
    if(prev_tr != NULL)
        free_txn_read(prev_tr);

    return ret;
}

// Queue ops:

int add_enqueue_to_txn(WORD * column_values, int no_cols, size_t blob_size, WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate)
{
    txn_write * tw = get_txn_queue_op(QUERY_TYPE_ENQUEUE, column_values, no_cols, blob_size, table_key, queue_id,
                        NULL, NULL, NULL, -1, NULL, -1, (int64_t) ts->write_set->no_items);

     // Multiple enqueues in the same txn accumulate in write set (indexed by local_order = ts->write_set->no_items):
    return skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate);
}

int add_read_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        int64_t new_read_head, vector_clock * prh_version, txn_state * ts, unsigned int * fastrandstate)
{
    txn_write * tw = get_txn_queue_op(QUERY_TYPE_READ_QUEUE, NULL, 0, 0, table_key, queue_id,
                                consumer_id, shard_id, app_id, new_read_head, prh_version, -1,
                                (int64_t) ts->write_set->no_items);

    // Keep only latest private_read_head when doing multiple queue reads in the same txn:
    snode_t * prev_tw_node = skiplist_search(ts->write_set, (WORD) tw);
    txn_write * prev_tw = (prev_tw_node != NULL)?prev_tw_node->value : NULL;

    int ret = skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // TO DO: Handle multiple queue reads in the same txn

    if(prev_tw != NULL)
        free_txn_write(prev_tw);

    return ret;
}

int add_consume_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                    int64_t new_consume_head, txn_state * ts, unsigned int * fastrandstate)
{
    txn_write * tw = get_txn_queue_op(QUERY_TYPE_CONSUME_QUEUE, NULL, 0, 0, table_key, queue_id,
                                consumer_id, shard_id, app_id, -1, NULL, new_consume_head,
                                (int64_t) ts->write_set->no_items);
    // Keep only latest private_consume_head when doing multiple queue consumes in the same txn:
    snode_t * prev_tw_node = skiplist_search(ts->write_set, (WORD) tw);
    txn_write * prev_tw = (prev_tw_node != NULL)?prev_tw_node->value : NULL;

    int ret = skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // TO DO: Handle multiple queue reads in the same txn
    if(prev_tw != NULL)
        free_txn_write(prev_tw);

    return ret;
}

int add_create_queue_to_txn(WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate)
{
    txn_write * tw = get_txn_queue_op(QUERY_TYPE_CREATE_QUEUE, NULL, 0, 0, table_key, queue_id,
                                            NULL, NULL, NULL, -1, NULL, -1, (int64_t) ts->write_set->no_items);

     // Multiple "create queue"-s in the same txn accumulate in write set (indexed by local_order = ts->write_set->no_items):
    return skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // TO DO: Handle multiple enqueues in the same txn
}

int add_delete_queue_to_txn(WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate)
{
    txn_write * tw = get_txn_queue_op(QUERY_TYPE_DELETE_QUEUE, NULL, 0, 0, table_key, queue_id,
                                            NULL, NULL, NULL, -1, NULL, -1, (int64_t) ts->write_set->no_items);

     // Multiple "delete queue"-s in the same txn accumulate in write set (indexed by local_order = ts->write_set->no_items):
    return skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // TO DO: Handle multiple enqueues in the same txn
}

int add_subscribe_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        queue_callback * callback, int64_t * prev_read_head, int64_t * prev_consume_head,
                        txn_state * ts, unsigned int * fastrandstate)
{
    assert (0); // Not implemented
    return 0;
}

int add_unsubscribe_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
                        txn_state * ts)
{
    assert (0); // Not implemented
    return 0;
}

