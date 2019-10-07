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
#define QUERY_TYPE_READ_ROW 2
#define QUERY_TYPE_READ_COLS 3
#define QUERY_TYPE_READ_PARTITION_RANGE 4
#define QUERY_TYPE_READ_CLUSTERING_RANGE 5
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


txn_state * init_txn_state()
{
	txn_state * ts = (txn_state *) malloc(sizeof(txn_state));
	uuid_generate(ts->txnid);
	ts->read_set = create_skiplist();
	ts->write_set = create_skiplist();

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
	skiplist_insert(ts->write_set, (long) column_values[schema->primary_key_idxs[0]], (WORD) tw, fastrandstate);

}



#endif /* BACKEND_TXNS_H_ */
