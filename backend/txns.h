/*
 * txns.h
 */

#ifndef BACKEND_TXNS_H_
#define BACKEND_TXNS_H_

#include <stdlib.h>
#include <uuid/uuid.h>

#include "db.h"

// Query types:

#define QUERY_TYPE_UPDATE 0
#define QUERY_TYPE_DELETE 1
#define QUERY_TYPE_READ_COLS 2
#define QUERY_TYPE_READ_ROW 3
#define QUERY_TYPE_READ_CLUSTERING_RANGE 4
#define QUERY_TYPE_READ_PARTITION_RANGE 5
#define QUERY_TYPE_READ_INDEX 6
#define QUERY_TYPE_READ_INDEX_RANGE 7


typedef struct txn_write
{
	short query_type;
	WORD table_key;

	WORD * column_values;
	int no_cols;
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

	short query_type;

	db_row_t* results;
} txn_read;

typedef struct txn_state
{
	uuid_t txnid;
	skiplist_t * read_set;
	skiplist_t * write_set;
} txn_state;

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
			return int((long) tr1->column_values[i] - (long) tr2->column_values[i]);
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

	if(tr1->no_primary_keys != tr2->no_primary_keys)
		return tr1->no_primary_keys - tr2->no_primary_keys;
	for(int i=0;i<tr1->no_primary_keys;i++)
	{
		if(tr1->start_primary_keys[i] != tr2->start_primary_keys[i])
			return tr1->start_primary_keys[i] - tr2->start_primary_keys[i];
	}

	if(tr1->query_type == QUERY_TYPE_READ_PARTITION_RANGE)
	{
		for(int i=0;i<tr1->no_primary_keys;i++)
		{
			if(tr1->end_primary_keys[i] != tr2->end_primary_keys[i])
				return tr1->end_primary_keys[i] - tr2->end_primary_keys[i];
		}
	}

	if(tr1->no_clustering_keys != tr2->no_clustering_keys)
		return tr1->no_clustering_keys - tr2->no_clustering_keys;
	for(int i=0;i<tr1->no_clustering_keys;i++)
	{
		if(tr1->start_clustering_keys[i] != tr2->start_clustering_keys[i])
			return tr1->start_clustering_keys[i] - tr2->start_clustering_keys[i];
	}

	if(tr1->query_type == QUERY_TYPE_READ_CLUSTERING_RANGE)
	{
		for(int i=0;i<tr1->no_clustering_keys;i++)
		{
			if(tr1->end_clustering_keys[i] != tr2->end_clustering_keys[i])
				return tr1->end_clustering_keys[i] - tr2->end_clustering_keys[i];
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

txn_write * get_txn_write(WORD * column_values, int no_cols, WORD table_key)
{
	txn_write * tw = (txn_write *) malloc(sizeof(txn_write) + no_cols*sizeof(WORD));
	tw->table_key = table_key;
	tw->no_cols = no_cols;
	tw->column_values = (WORD *) ((char *) tw + sizeof(txn_write));
	tw->query_type = QUERY_TYPE_UPDATE;

	return tw;
}

int add_write_to_txn(WORD * column_values, int no_cols, WORD table_key, txn_state * ts, unsigned int * fastrandstate)
{
	txn_write * tw = get_txn_write(column_values, no_cols, table_key);
	skiplist_insert(ts->write_set, (WORD) tw, (WORD) tw, fastrandstate);

}



#endif /* BACKEND_TXNS_H_ */
