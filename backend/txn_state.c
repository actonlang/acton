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

	if(tr1->no_cols != tr2->no_cols)
		return tr1->no_cols - tr2->no_cols;
	for(int i=0;i<tr1->no_cols;i++)
	{
		if(tr1->column_values[i] != tr2->column_values[i])
			return (int) (long) tr1->column_values[i] - (long) tr2->column_values[i];
	}

	return 0;
}

int txn_read_cmp(WORD e1, WORD e2)
{
	txn_read * tr1 = (txn_read *) e1;
	txn_read * tr2 = (txn_read *) e2;

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

txn_write * get_txn_write(short query_type, WORD * column_values, int no_cols, WORD table_key)
{
	txn_write * tw = (txn_write *) malloc(sizeof(txn_write) + no_cols*sizeof(WORD));
	memset(tw, 0, sizeof(txn_write) + no_cols*sizeof(WORD));

	tw->table_key = table_key;
	tw->no_cols = no_cols;
	tw->column_values = (WORD *) ((char *) tw + sizeof(txn_write));
	for(int i=0;i<tw->no_cols;i++)
		tw->column_values[i] = column_values[i];

	tw->query_type = query_type;

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
						WORD table_key)
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

int add_write_to_txn(short query_type, WORD * column_values, int no_cols, WORD table_key, txn_state * ts, unsigned int * fastrandstate)
{
	assert((query_type == QUERY_TYPE_UPDATE) || (query_type == QUERY_TYPE_DELETE));
	txn_write * tw = get_txn_write(query_type, column_values, no_cols, table_key);
	return skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate); // Note that this will overwrite previous values written for the variable in the same txn (last write wins)
}

int add_row_read_to_txn(WORD* primary_keys, int no_primary_keys,
						WORD table_key, db_row_t* result,
						txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_ROW, primary_keys, NULL, no_primary_keys, NULL, NULL, 0, NULL, 0, -1, result, NULL, NULL, table_key);
	return skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
}

int add_row_range_read_to_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys, WORD table_key,
								snode_t* start_row, snode_t* end_row,
								txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_ROW_RANGE, start_primary_keys, end_primary_keys, no_primary_keys, NULL, NULL, 0, NULL, 0, -1, NULL, start_row, end_row, table_key);
	return skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
}

int add_cell_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
								WORD table_key, db_row_t* result,
								txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_CELL, primary_keys, NULL, no_primary_keys, clustering_keys, NULL, no_clustering_keys, NULL, 0, -1, result, NULL, NULL, table_key);
	return skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
}

int add_cell_range_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys,
								WORD* end_clustering_keys, int no_clustering_keys,
								WORD table_key,
								snode_t* start_row, snode_t* end_row,
								txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_CELL_RANGE, primary_keys, NULL, no_primary_keys, start_clustering_keys, end_clustering_keys, no_clustering_keys, NULL, 0, -1, NULL, start_row, end_row, table_key);
	return skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
}

int add_col_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
								WORD* col_keys, int no_columns,
								WORD table_key, db_row_t* result,
								txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_COLS, primary_keys, NULL, no_primary_keys, clustering_keys, NULL, no_clustering_keys, col_keys, no_columns, -1, result, NULL, NULL, table_key);
	return skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
}

int add_index_read_to_txn(WORD* index_key, int idx_idx, WORD table_key, db_row_t* result, txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_INDEX, index_key, NULL, 1, NULL, NULL, 0, NULL, 0, idx_idx, result, NULL, NULL, table_key);
	return skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
}

int add_index_range_read_to_txn(int idx_idx, WORD* start_idx_key, WORD* end_idx_key, snode_t* start_row, snode_t* end_row, WORD table_key, txn_state * ts, unsigned int * fastrandstate)
{
	txn_read * tr = get_txn_read(QUERY_TYPE_READ_INDEX_RANGE, start_idx_key, end_idx_key, 1, NULL, NULL, 0, NULL, 0, idx_idx, NULL, start_row, end_row, table_key);
	return skiplist_insert(ts->read_set, (WORD) tr, (WORD) tr, fastrandstate); // Note that this will overwrite previous values read for the variable in the same txn (last read wins)
}
