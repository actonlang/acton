/*
 * txn_state.c
 */

#include <stdlib.h>
#include <uuid/uuid.h>
#include <string.h>
#include <assert.h>

#include "txn_state.h"

int txn_write_cmp(WORD e1, WORD e2)
{
	txn_write * tr1 = (txn_write *) e1;
	txn_write * tr2 = (txn_write *) e2;

	if(tr1->query_type != tr2->query_type)
		return tr1->query_type - tr2->query_type;

	if(tr1->table_key != tr2->table_key)
		return (int) ((long) tr1->table_key - (long) tr2->table_key);

	if(tr1->query_type < QUERY_TYPE_ENQUEUE) // Regular ops ordering:
	{
		// Newer writes always overwrite older writes to the same objects in the same txn:

		int no_key_cols1 = tr1->no_primary_keys + tr1->no_clustering_keys;
		int no_key_cols2 = tr2->no_primary_keys + tr2->no_clustering_keys;

		if(no_key_cols1 != no_key_cols2)
			return no_key_cols1 - no_key_cols2;
		for(int i=0;i<no_key_cols1;i++)
		{
			if(tr1->column_values[i] != tr2->column_values[i])
				return (int) (long) tr1->column_values[i] - (long) tr2->column_values[i];
		}
	}
	else // Queue ops ordering:
	{
		// For queue reads and consumes, the last version of private read/consume head is kept in write set:
		if(tr1->query_type == QUERY_TYPE_READ_QUEUE || tr1->query_type == QUERY_TYPE_CONSUME_QUEUE)
		{
			if(tr1->queue_id != tr2->queue_id)
				return (int) ((long) tr1->queue_id - (long) tr2->queue_id);
			if(tr1->app_id != tr2->app_id)
				return (int) ((long) tr1->app_id - (long) tr2->app_id);
			if(tr1->shard_id != tr2->shard_id)
				return (int) ((long) tr1->shard_id - (long) tr2->shard_id);
			if(tr1->consumer_id != tr2->consumer_id)
				return (int) ((long) tr1->consumer_id - (long) tr2->consumer_id);
		}
		// All enqueues, queue creates, deletes, subscribes and unsubscribes are accumulated in the write set:
		else if(tr1->query_type == QUERY_TYPE_ENQUEUE ||
				tr1->query_type == QUERY_TYPE_CREATE_QUEUE ||
				tr1->query_type == QUERY_TYPE_SUBSCRIBE_QUEUE ||
				tr1->query_type == QUERY_TYPE_UNSUBSCRIBE_QUEUE)
		{
			if(tr1->local_order != tr2->local_order)
				return (int) ((long) tr1->local_order - (long) tr2->local_order);
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
		return (int) ((long) tr1->table_key - (long) tr2->table_key);

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

	return ts;
}

void free_txn_state(txn_state * ts)
{
	skiplist_free(ts->read_set);
	skiplist_free(ts->write_set);
	free(ts);
}

txn_write * get_txn_write(short query_type, WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, WORD table_key, long local_order)
{
	txn_write * tw = (txn_write *) malloc(sizeof(txn_write) + no_cols*sizeof(WORD));
	memset(tw, 0, sizeof(txn_write) + no_cols*sizeof(WORD));

	tw->table_key = table_key;
	tw->no_cols = no_cols;
	tw->no_primary_keys = no_primary_keys;
	tw->no_clustering_keys = no_clustering_keys;
	tw->column_values = (WORD *) ((char *) tw + sizeof(txn_write));
	for(int i=0;i<tw->no_cols;i++)
		tw->column_values[i] = column_values[i];

	tw->query_type = query_type;
	tw->local_order = local_order;

	return tw;
}

txn_write * get_txn_queue_op(short query_type, WORD * column_values, int no_cols, WORD table_key,
					WORD queue_id, WORD consumer_id, WORD shard_id, WORD app_id,
					long new_read_head, long new_consume_head, long local_order)
{
	assert(query_type >= QUERY_TYPE_ENQUEUE && query_type <= QUERY_TYPE_UNSUBSCRIBE_QUEUE);

	txn_write * tw = get_txn_write(query_type, column_values, no_cols, 0, 0, table_key, local_order);

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
						db_row_t* result, snode_t* start_row, snode_t* end_row,
						WORD table_key, long local_order)
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
		assert(idx_idx == -1);
	tr->idx_idx = idx_idx;

	tr->result = result;
	tr->start_row = start_row;
	tr->end_row = end_row;

	return tr;
}

void free_txn_read(txn_read * tr)
{
	free(tr);
}

int add_write_to_txn(short query_type, WORD * column_values, int no_cols, int no_primary_keys, int no_clustering_keys, WORD table_key, txn_state * ts, unsigned int * fastrandstate)
{
	assert((query_type == QUERY_TYPE_UPDATE) || (query_type == QUERY_TYPE_DELETE));
	txn_write * tw = get_txn_write(query_type, column_values, no_cols, no_primary_keys, no_clustering_keys, table_key, (long) ts->write_set->no_items);

	// Note that this will overwrite previous values written for the variable in the same txn (last write wins):

	txn_write * prev_tw = skiplist_search(ts->write_set, (WORD) tw);

	int ret = skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate);

	if(prev_tw != NULL)
		free_txn_write(prev_tw);

	return ret;
}

int add_row_read_to_txn(WORD* primary_keys, int no_primary_keys,
						WORD table_key, db_row_t* result,
						txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_ROW, primary_keys, NULL, no_primary_keys, NULL, NULL, 0, NULL, 0, -1, result, NULL, NULL, table_key, (long) ts->read_set->no_items);

	// Note that this will overwrite previous values read for the variable in the same txn (last read wins):

	txn_read * prev_tr = skiplist_search(ts->read_set, (WORD) tr);

	int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate);

	if(prev_tr != NULL)
		free_txn_read(prev_tr);

	return ret;
}

int add_row_range_read_to_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys, WORD table_key,
								snode_t* start_row, snode_t* end_row,
								txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_ROW_RANGE, start_primary_keys, end_primary_keys, no_primary_keys, NULL, NULL, 0, NULL, 0, -1, NULL, start_row, end_row, table_key, (long) ts->read_set->no_items);
	txn_read * prev_tr = skiplist_search(ts->read_set, (WORD) tr);

	int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
	if(prev_tr != NULL)
		free_txn_read(prev_tr);

	return ret;
}

int add_cell_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
								WORD table_key, db_row_t* result,
								txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_CELL, primary_keys, NULL, no_primary_keys, clustering_keys, NULL, no_clustering_keys, NULL, 0, -1, result, NULL, NULL, table_key, (long) ts->read_set->no_items);
	txn_read * prev_tr = skiplist_search(ts->read_set, (WORD) tr);

	int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
	if(prev_tr != NULL)
		free_txn_read(prev_tr);

	return ret;
}

int add_cell_range_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys,
								WORD* end_clustering_keys, int no_clustering_keys,
								WORD table_key,
								snode_t* start_row, snode_t* end_row,
								txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_CELL_RANGE, primary_keys, NULL, no_primary_keys, start_clustering_keys, end_clustering_keys, no_clustering_keys, NULL, 0, -1, NULL, start_row, end_row, table_key, (long) ts->read_set->no_items);
	txn_read * prev_tr = skiplist_search(ts->read_set, (WORD) tr);

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
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_COLS, primary_keys, NULL, no_primary_keys, clustering_keys, NULL, no_clustering_keys, col_keys, no_columns, -1, result, NULL, NULL, table_key, (long) ts->read_set->no_items);
	txn_read * prev_tr = skiplist_search(ts->read_set, (WORD) tr);

	int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
	if(prev_tr != NULL)
		free_txn_read(prev_tr);

	return ret;
}

int add_index_read_to_txn(WORD* index_key, int idx_idx, WORD table_key, db_row_t* result, txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_INDEX, index_key, NULL, 1, NULL, NULL, 0, NULL, 0, idx_idx, result, NULL, NULL, table_key, (long) ts->read_set->no_items);
	txn_read * prev_tr = skiplist_search(ts->read_set, (WORD) tr);

	int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
	if(prev_tr != NULL)
		free_txn_read(prev_tr);

	return ret;
}

int add_index_range_read_to_txn(int idx_idx, WORD* start_idx_key, WORD* end_idx_key, snode_t* start_row, snode_t* end_row, WORD table_key, txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_INDEX_RANGE, start_idx_key, end_idx_key, 1, NULL, NULL, 0, NULL, 0, idx_idx, NULL, start_row, end_row, table_key, (long) ts->read_set->no_items);
	txn_read * prev_tr = skiplist_search(ts->read_set, (WORD) tr);

	int ret = skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
	if(prev_tr != NULL)
		free_txn_read(prev_tr);

	return ret;
}

// Queue ops:

int add_enqueue_to_txn(WORD * column_values, int no_cols, WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate)
{
	txn_write * tw = get_txn_queue_op(QUERY_TYPE_ENQUEUE, column_values, no_cols, table_key, queue_id,
						NULL, NULL, NULL, -1, -1, (long) ts->write_set->no_items);

	 // Multiple enqueues in the same txn accumulate in write set (indexed by local_order = ts->write_set->no_items):
	return skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate);
}

int add_read_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						long new_read_head, txn_state * ts, unsigned int * fastrandstate)
{
	txn_write * tw = get_txn_queue_op(QUERY_TYPE_READ_QUEUE, NULL, 0, table_key, queue_id,
								consumer_id, shard_id, app_id, new_read_head, -1,
								(long) ts->write_set->no_items);

	// Keep only latest private_read_head when doing multiple queue reads in the same txn:
	txn_write * prev_tw = skiplist_search(ts->write_set, (WORD) tw);
	int ret = skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // TO DO: Handle multiple queue reads in the same txn

	if(prev_tw != NULL)
		free_txn_write(prev_tw);

	return ret;
}

int add_consume_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
					long new_consume_head, txn_state * ts, unsigned int * fastrandstate)
{
	txn_write * tw = get_txn_queue_op(QUERY_TYPE_CONSUME_QUEUE, NULL, 0, table_key, queue_id,
								consumer_id, shard_id, app_id, -1, new_consume_head,
								(long) ts->write_set->no_items);
	// Keep only latest private_consume_head when doing multiple queue consumes in the same txn:
	txn_write * prev_tw = skiplist_search(ts->write_set, (WORD) tw);

	int ret = skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // TO DO: Handle multiple queue reads in the same txn
	if(prev_tw != NULL)
		free_txn_write(prev_tw);

	return ret;
}

int add_create_queue_to_txn(WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate)
{
	txn_write * tw = get_txn_queue_op(QUERY_TYPE_CREATE_QUEUE, NULL, 0, table_key, queue_id,
											NULL, NULL, NULL, -1, -1, (long) ts->write_set->no_items);

	 // Multiple "create queue"-s in the same txn accumulate in write set (indexed by local_order = ts->write_set->no_items):
	return skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // TO DO: Handle multiple enqueues in the same txn
}

int add_delete_queue_to_txn(WORD table_key, WORD queue_id, txn_state * ts, unsigned int * fastrandstate)
{
	txn_write * tw = get_txn_queue_op(QUERY_TYPE_DELETE_QUEUE, NULL, 0, table_key, queue_id,
											NULL, NULL, NULL, -1, -1, (long) ts->write_set->no_items);

	 // Multiple "delete queue"-s in the same txn accumulate in write set (indexed by local_order = ts->write_set->no_items):
	return skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // TO DO: Handle multiple enqueues in the same txn
}

int add_subscribe_queue_to_txn(WORD consumer_id, WORD shard_id, WORD app_id, WORD table_key, WORD queue_id,
						queue_callback * callback, long * prev_read_head, long * prev_consume_head,
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

