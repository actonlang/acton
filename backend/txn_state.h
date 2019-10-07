/*
 * txn_state.h
 */

#ifndef BACKEND_TXN_STATE_H_
#define BACKEND_TXN_STATE_H_

#include <uuid/uuid.h>
#include "db.h"

// Query types:

#define QUERY_TYPE_UPDATE 0
#define QUERY_TYPE_DELETE 1
#define QUERY_TYPE_READ_COLS 2
#define QUERY_TYPE_READ_CELL 3
#define QUERY_TYPE_READ_CELL_RANGE 4
#define QUERY_TYPE_READ_ROW 5
#define QUERY_TYPE_READ_ROW_RANGE 6
#define QUERY_TYPE_READ_INDEX 7
#define QUERY_TYPE_READ_INDEX_RANGE 8


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

	WORD* col_keys;
	int no_col_keys;

	// For idx queries:

	int idx_idx;

	short query_type;

	// For non-range queries:

	db_row_t* result;

	// For range queries:

	snode_t* start_row;
	snode_t* end_row;
} txn_read;

typedef struct txn_state
{
	uuid_t txnid;
	skiplist_t * read_set;
	skiplist_t * write_set;
} txn_state;

int txn_write_cmp(WORD e1, WORD e2);
int txn_read_cmp(WORD e1, WORD e2);

txn_state * init_txn_state();
void free_txn_state(txn_state * ts);

txn_write * get_txn_write(short query_type, WORD * column_values, int no_cols, WORD table_key);
void free_txn_write(txn_write * tw);
txn_read * get_txn_read(short query_type,
						WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys,
						WORD* start_clustering_keys, WORD* end_clustering_keys, int no_clustering_keys,
						WORD* col_keys, int no_col_keys,
						int idx_idx,
						db_row_t* result, snode_t* start_row, snode_t* end_row,
						WORD table_key);
void free_txn_read(txn_read * tr);
int add_write_to_txn(short query_type, WORD * column_values, int no_cols, WORD table_key, txn_state * ts, unsigned int * fastrandstate);
int add_row_read_to_txn(WORD* primary_keys, int no_primary_keys,
						WORD table_key, db_row_t* result,
						txn_state * ts, unsigned int * fastrandstate);
int add_row_range_read_to_txn(WORD* start_primary_keys, WORD* end_primary_keys, int no_primary_keys, WORD table_key,
								snode_t* start_row, snode_t* end_row,
								txn_state * ts, unsigned int * fastrandstate);
int add_cell_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
								WORD table_key, db_row_t* result,
								txn_state * ts, unsigned int * fastrandstate);
int add_cell_range_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* start_clustering_keys,
								WORD* end_clustering_keys, int no_clustering_keys,
								WORD table_key,
								snode_t* start_row, snode_t* end_row,
								txn_state * ts, unsigned int * fastrandstate);
int add_col_read_to_txn(WORD* primary_keys, int no_primary_keys, WORD* clustering_keys, int no_clustering_keys,
								WORD* col_keys, int no_columns,
								WORD table_key, db_row_t* result,
								txn_state * ts, unsigned int * fastrandstate);
int add_index_read_to_txn(WORD* index_key, int idx_idx, WORD table_key, db_row_t* result, txn_state * ts, unsigned int * fastrandstate);
int add_index_range_read_to_txn(int idx_idx, WORD* start_idx_key, WORD* end_idx_key, snode_t* start_row, snode_t* end_row, WORD table_key, txn_state * ts, unsigned int * fastrandstate);

#endif /* BACKEND_TXN_STATE_H_ */
